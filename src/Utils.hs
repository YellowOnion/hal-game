-- |

module Utils where

import qualified Graphics.Rendering.OpenGL as GL

import qualified Codec.GlTF                as GlTF
import           Codec.GlTF.Mesh

toGLPrimitive :: MeshPrimitiveMode -> GL.PrimitiveMode
toGLPrimitive p = case p of
   POINTS               -> GL.Points
   LINES                -> GL.Lines
   LINE_LOOP            -> GL.LineLoop
   LINE_STRIP           -> GL.LineStrip
   TRIANGLES            -> GL.Triangles
   TRIANGLE_STRIP       -> GL.TriangleStrip
   TRIANGLE_FAN         -> GL.TriangleFan
   MeshPrimitiveMode 7  -> GL.Quads
   MeshPrimitiveMode 8  -> GL.QuadStrip
   MeshPrimitiveMode 9  -> GL.Polygon
   MeshPrimitiveMode 10 -> GL.Patches
