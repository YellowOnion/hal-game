{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class ( MonadIO )

import qualified SDL

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL ( HasSetter(($=)), HasGetter(get) )

import qualified Linear as L
import qualified Linear.V4 as L (point)
import           Linear (V3(..), (!*), (*!))

import Debug.Trace

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Shaders

data Camera = Camera
  { camProjLoc :: !GL.UniformLocation
  , camViewLoc :: !GL.UniformLocation
  } deriving (Show, Eq)

data Scene = Scene
  { win :: !SDL.Window
  , shaderProgram :: !GL.Program
  , triVAO :: !GL.VertexArrayObject
  , triVBO :: !GL.BufferObject
  , camera :: !Camera
  } deriving (Show, Eq)

viewMat :: Float -> L.M44 GL.GLfloat
viewMat t = L.lookAt pos cen up
  where
    r   = 1
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

  projLoc <- mat4FloatUniform p "projection" perspective
  viewLoc <- mat4FloatUniform p "view" view
  return (projLoc, viewLoc)

mkScene :: SDL.Window -> IO Scene
mkScene win = do
  -- Shader setup
  p <- GL.createProgram
  GL.attachShader p =<< vertexShader
  GL.attachShader p =<< fragmentShader
  GL.linkProgram p
  status <- get $ GL.linkStatus p
  log_ <- get $ GL.programInfoLog p

  -- vertex buffer object setup
  triVBO <- GL.genObjectName
  triVAO <- GL.genObjectName
  GL.bindVertexArrayObject $= Just triVAO
  GL.bindBuffer GL.ArrayBuffer $= Just triVBO
  tri <- triangle
  GL.bufferData GL.ArrayBuffer $= tri

  let loc = GL.AttribLocation 0
  GL.vertexAttribPointer loc
    $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr )
  GL.vertexAttribArray loc $= GL.Enabled

  t <- fmap ((/1000) . fromIntegral) SDL.ticks

  (projLoc, viewLoc) <- setUniforms p win t

  if status
    then do
    return $! Scene win p triVAO triVBO (Camera projLoc viewLoc)
    else error log_


main :: IO ()
main = do
  putStrLn "Welcome!"
  SDL.initializeAll
  win <- SDL.createWindow "hal-game"
    SDL.defaultWindow
    { SDL.windowInitialSize = L.V2 1600 900
    , SDL.windowGraphicsContext =
        SDL.OpenGLContext SDL.defaultOpenGL
        { SDL.glProfile = SDL.Core SDL.Normal 3 3 }
    }
  ctx <- createContext win
  scene <- mkScene win
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


triangle :: IO (GL.GLsizeiptr, Ptr (L.V3 Float), GL.BufferUsage)
triangle = do
  ptr <- newArray pt
  let size = fromIntegral $ sizeOf (head pt) * length pt
  return (size, ptr, GL.StaticDraw)

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
  GL.drawArrays GL.Triangles 0 3

  SDL.glSwapWindow win
