{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad (unless, forM_, foldM)
import Control.Monad.IO.Class ( MonadIO )

import GHC.Generics

import qualified SDL

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL ( HasSetter(($=)), HasGetter(get) )

import qualified Linear as L
import qualified Linear.V4 as L (point)
import           Linear (V2(..), V3(..), (!*), (*!))

import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as CVec
import qualified Data.Vector.Storable.Mutable as CMVec

import Data.Word
import Data.Int

import qualified Codec.GlTF as GlTF
import qualified Codec.GlTF.URI as GlTF
import qualified Codec.GlTF.Mesh as GlTF
import qualified Codec.GlTF.Accessor as GlTF
import qualified Codec.GlTF.BufferView as GlTF
import qualified Codec.GlTF.Buffer as GlTF

-- TODO generic replacements for this (i.e. fromEnum/fromIntegral)
import Codec.GlTF.BufferView (BufferViewIx(unBufferViewIx))
import Codec.GlTF.Buffer (BufferIx(unBufferIx))

import Debug.Trace
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Optics
--import Optics.Extra
import Optics.Operators.Unsafe ((^?!))
import Data.Aeson.Optics
--import Data.Generics.Product

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Internal as BS (toForeignPtr0)

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.ForeignPtr.Unsafe

import Shaders
import Utils
import Data.Maybe (fromJust)
import Data.Either (fromRight)

import System.FilePath
import qualified Codec.GlTF.Accessor as GlFT
import qualified Graphics.Rendering.OpenGL as Gl

fromRight' (Right a) = a

data Camera = Camera
  { camProjLoc :: !GL.UniformLocation
  , camViewLoc :: !GL.UniformLocation
  } deriving (Show, Eq)

data Scene = Scene
  { win :: !SDL.Window
  , asset :: !Asset
  , shaderProgram :: !GL.Program
  , triVAO :: !GL.VertexArrayObject
  , triVBO :: !GL.BufferObject
  , triEBO :: !GL.BufferObject
  , count :: !Int32
  , camera :: !Camera
  } deriving (Show, Eq)

viewMat :: Float -> L.M44 GL.GLfloat
viewMat t = L.lookAt pos cen up
  where
    r   = 4
    x   = cos t * r
    z   = sin t * r
    y   = sin (2*t) * 1/pi
    pos = V3 x y z
    cen = V3 0.0 0.0 0.0
    up  = V3 0.0 1.0 0.0

m44ToGLmatrix :: GL.MatrixComponent a => L.M44 a -> IO (GL.GLmatrix a)
m44ToGLmatrix m = GL.withNewMatrix GL.ColumnMajor $ \p->poke (castPtr p) m
{-# INLINABLE m44ToGLmatrix #-}

m44ToGLmatrixRow :: GL.MatrixComponent a => L.M44 a -> IO (GL.GLmatrix a)
m44ToGLmatrixRow m = GL.withNewMatrix GL.RowMajor $ \p->poke (castPtr p) m
{-# INLINABLE m44ToGLmatrixRow #-}

mat4FloatUniform :: GL.Program -> String -> L.M44 Float -> IO GL.UniformLocation
mat4FloatUniform p str val = do
  loc <- get $ GL.uniformLocation p str
  m4 <- m44ToGLmatrixRow val
  GL.uniform loc $= m4
  return loc

setUniforms :: GL.Program -> SDL.Window -> Float -> IO (GL.UniformLocation, GL.UniformLocation)
setUniforms p win t = do
  (L.V2 x y) :: L.V2 Float <- fmap (fmap fromIntegral) <$> get $ SDL.windowSize win
  let perspective = L.perspective (pi / 2) -- 90 degrees FOV
                                   (x / y) -- Aspect Ratio
                                      0.1  -- near plane
                                      100  -- far plane
      view         = viewMat t

  viewLoc <- mat4FloatUniform p "view" view
  projLoc <- mat4FloatUniform p "projection" perspective
  return (projLoc, viewLoc)


type Buffer a = CVec.Vector a
type BufferCache = HM.HashMap Int (Buffer Word8)

type Idx = Int

loadBufferData :: FilePath -> BufferCache -> Idx -> GlTF.Buffer -> IO (BufferCache, Buffer Word8)
loadBufferData dir hm idx (GlTF.Buffer size fp' _ _ _) = do
  b <- case HM.lookup idx hm of
    Nothing -> do
      allocNewBuffer
    Just b -> return b
  return (HM.insert idx b hm, b)
  where
    fpf (Just (GlTF.URI s)) = s
    fpf Nothing = error "can't load buffer URI doesn't exist"
    fp = fpf fp'
    allocNewBuffer = do
      bs <- BSL.take (fromIntegral size) <$> BSL.readFile (dir </> T.unpack fp)
      return . CVec.concat $ uncurry CVec.unsafeFromForeignPtr0 . BS.toForeignPtr0 <$> BSL.toChunks bs

data Vertices a = Vertices
  { buffer :: Buffer a
  , type' :: GlFT.ComponentType
  , count :: Int32
  -- Stride? Name? Target?
  } deriving (Eq, Show, Generic)

data Asset = Asset
  { mode :: GL.PrimitiveMode
--  , buffers :: BufferCache
--  , count :: Int32
  -- TODO remove Word16 hard coding to support more than 65k verts
  , indices   :: Vertices Word16
  , positions :: Vertices (V3 Float)
  , normals   :: Vertices (V3 Float)
  , uvMap     :: Vertices (V2 Float)
  } deriving (Eq, Show, Generic)

loadAsset fp = do
  egltf <- GlTF.fromFile fp
  let
    bufCache = HM.empty :: BufferCache
    gltf = fromRight (error "Failed to load asset") egltf :: GlTF.GlTF
    meshes = GlTF.meshes gltf
    mesh = Vec.head . fromJust $ meshes
    prim = mesh ^?! #primitives % ix 0
    primAttrs = prim ^. #attributes
    primMode = toGLPrimitive $ prim ^. #mode

  T.putStrLn $ "Mesh: " <> mesh ^?! #name % _Just
  putStrLn   $ "Prim: " <> show prim

  (bufCache, posIndices) <- mkVertices gltf bufCache $ prim ^?! #indices % _Just
  (bufCache, posVerts) <- mkVertices gltf bufCache $ primAttrs ^?! ix "POSITION"
  (bufCache, normalVerts) <- mkVertices gltf bufCache $ primAttrs ^?! ix "NORMAL"
  (bufCache, uvVerts) <- mkVertices gltf bufCache $ primAttrs ^?! ix "TEXCOORD_0"

  return $ Asset primMode posIndices posVerts normalVerts uvVerts
    where
      dir = takeDirectory fp
      mkVertices :: Storable a => GlTF.GlTF -> BufferCache -> GlTF.AccessorIx -> IO (BufferCache, Vertices a)
      mkVertices gltf bc idx = do
        let acc = gltf ^?! #accessors % _Just % ix (GlTF.unAccessorIx idx)
            count = fromIntegral $ acc ^. #count
            bvidx =  unBufferViewIx $ acc ^?! #bufferView % _Just
            bv = gltf ^?! #bufferViews % _Just % ix bvidx
            bidx = unBufferIx $ bv ^?! #buffer
            b = gltf ^?! #buffers % _Just % ix bidx
        print acc
        (bc', buf) <- loadBufferData dir bc bidx b
        let buf' = CVec.unsafeCast
                 . CVec.take (bv ^?! #byteLength)
                 . CVec.drop (bv ^?! #byteOffset)
                 $ buf
        return (bc', Vertices buf' GlTF.BYTE count)



mkScene :: SDL.Window -> Asset -> IO Scene
mkScene win ass = do
  -- Shader setup
  p <- GL.createProgram
  GL.attachShader p =<< vertexShader
  GL.attachShader p =<< fragmentShader
  GL.linkProgram p
  linkStatus <- get $ GL.linkStatus p

  -- vertex buffer object setup
  triVAO <- GL.genObjectName
  triVBO <- GL.genObjectName
  triEBO <- GL.genObjectName
  GL.bindVertexArrayObject $= Just triVAO

  GL.bindBuffer GL.ArrayBuffer $= Just triVBO
  tri <- glBufferFromVertices (ass ^. #positions)
  GL.bufferData GL.ArrayBuffer $= tri

  Gl.bindBuffer GL.ElementArrayBuffer $= Just triEBO
  indices <- glBufferFromVertices (ass ^. #indices)
  GL.bufferData GL.ElementArrayBuffer $= indices

  let loc = GL.AttribLocation 0
  GL.vertexAttribPointer loc
    $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr )
  GL.vertexAttribArray loc $= GL.Enabled

  t <- fmap ((/1000) . fromIntegral) SDL.ticks

  (projLoc, viewLoc) <- setUniforms p win t

  GL.validateProgram p
  log_ <- get $ GL.programInfoLog p
  valStatus <- get $ GL.validateStatus p
  if linkStatus && valStatus
    then do
    return $! Scene win ass p triVAO triVBO triEBO (fromIntegral . CVec.length $ ass ^. #indices % #buffer) (Camera projLoc viewLoc)
    else error log_


main :: IO ()
main = do
  putStrLn "Welcome!"
  SDL.initializeAll
  ass <- loadAsset "data/monkey.gltf"
  print ass
  win <- SDL.createWindow "hal-game"
    SDL.defaultWindow
    { SDL.windowInitialSize = L.V2 1600 900
    , SDL.windowGraphicsContext =
        SDL.OpenGLContext SDL.defaultOpenGL
        { SDL.glProfile = SDL.Core SDL.Normal 4 3 }
    }
  ctx <- createContext win
  GL.debugOutput $= GL.Enabled
  GL.debugMessageCallback $= Just (print)

  scene <- mkScene win ass
  putStrLn $ "starting main loop"
  appLoop scene
  SDL.destroyWindow win

createContext :: MonadIO m => SDL.Window -> m SDL.GLContext
createContext win = do
  ctx <- SDL.glCreateContext win
  SDL.glMakeCurrent win ctx
  return ctx

appLoop :: Scene -> IO ()
appLoop scene = do
  events <- SDL.pollEvents
  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  x <- drawAll scene
  x `seq` unless qPressed (appLoop scene)

glBufferFromVertices :: Storable a => Vertices a -> IO (GL.GLsizeiptr, Ptr a, GL.BufferUsage)
glBufferFromVertices (Vertices buf _ _) =
  return (fromIntegral l * fromIntegral (sizeOf (CVec.head buf)), unsafeForeignPtrToPtr ptr, GL.StaticDraw)
  where
    (ptr, l) = CVec.unsafeToForeignPtr0 buf

triangle :: IO (GL.GLsizeiptr, Ptr (L.V3 Float), GL.BufferUsage)
triangle = do
  ptr <- newArray pt
  let size = fromIntegral $ sizeOf (head pt) * length pt
  return (size, ptr, GL.StaticDraw)

d30 :: Float
d30 = 0.5/(pi / 3)
pt :: [ L.V3 Float ]
pt =  [ L.V3 (-d30) (-0.5) (0.0)
      , L.V3   d30  (-0.5) (0.0)
      , L.V3   0.0    0.5  (0.0) ]


vertexShader :: IO GL.Shader
vertexShader = mkShader GL.VertexShader (ShaderFile "data/vertex.glsl")


fragmentShader :: IO GL.Shader
fragmentShader = mkShader GL.FragmentShader (ShaderFile "data/frag.glsl")

drawAll :: Scene -> IO ()
drawAll Scene{..} = do
  t <- fmap ((/1000) . fromIntegral) SDL.ticks
  GL.clearColor $= let x = 0.1 in
    GL.Color4 (x * sin             t  + x)
              (x * sin (pi * 2/3 + t) + x)
              (x * sin (pi * 4/3 + t) + x)
    1.0
  GL.clear [ GL.ColorBuffer ]

  GL.currentProgram $= Just shaderProgram

  _ <- setUniforms shaderProgram win t

  GL.bindVertexArrayObject $= Just triVAO
  GL.drawElements (asset ^. #mode) (asset ^. #indices % #count) GL.UnsignedShort nullPtr
  GL.bindVertexArrayObject $= Nothing

  SDL.glSwapWindow win
