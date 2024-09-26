{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class ( MonadIO )

import qualified SDL

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL ( HasSetter(($=)), HasGetter(get) )

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Shaders

data Scene = Scene { win :: SDL.Window,
                shaderProgram :: GL.Program }

mkScene :: SDL.Window -> IO Scene
mkScene win = do
  p <- GL.createProgram
  GL.attachShader p =<< vertexShader
  GL.attachShader p =<< fragmentShader
  GL.linkProgram p
  status <- get $ GL.linkStatus p
  log_ <- get $ GL.programInfoLog p
  if status
    then do
    GL.currentProgram $= Just p
    return $ Scene win p
    else error log_


main :: IO ()
main = do
  putStrLn "Welcome!"
  SDL.initializeAll
  win <- SDL.createWindow "hal-game"
    SDL.defaultWindow { SDL.windowGraphicsContext =
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
  drawAll scene
  unless qPressed (appLoop scene)


triangle = do
  ptr <- newArray pt
  let size = fromIntegral $ sizeOf (0.0 :: Float) * length pt
  return (size, ptr, GL.StaticDraw)
  where
    pt :: [Float]
    pt = [ (-0.5), (-0.5), 0.0,
             0.5 , (-0.5), 0.0,
             0.0 ,   0.5 , 0.0 ]


vertexShader :: IO GL.Shader
vertexShader = mkShader GL.VertexShader (ShaderFile "data/vertex.glsl")


fragmentShader :: IO GL.Shader
fragmentShader = mkShader GL.FragmentShader (ShaderFile "data/frag.glsl")

drawAll Scene{..} = do
  GL.clearColor $= GL.Color4 1.0 1.0 1.0 1.0
  GL.clear [GL.ColorBuffer]

  -- useProgram
  -- bindVBO
  -- drawArrays

  let buf = GL.bindBuffer GL.ArrayBuffer
  t <- triangle
  GL.bufferData GL.ArrayBuffer $= t
  SDL.glSwapWindow win
