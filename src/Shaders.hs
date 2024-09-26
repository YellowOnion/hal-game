module Shaders where

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL ( HasSetter(($=)), HasGetter(get) )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS

data ShaderSource = ShaderString String
                  | ShaderFile FilePath

getSource (ShaderFile f) = BS.readFile f
getSource (ShaderString s) = return $ GL.packUtf8 s

mkShader :: GL.ShaderType -> ShaderSource -> IO GL.Shader
mkShader t str = do
  s <- GL.createShader t
  src <- getSource str
  GL.shaderSourceBS s $= src
  GL.compileShader s
  status <- get $ GL.compileStatus s
  log_ <- get $ GL.shaderInfoLog s

  return $ if status
         then s
         else error log_
