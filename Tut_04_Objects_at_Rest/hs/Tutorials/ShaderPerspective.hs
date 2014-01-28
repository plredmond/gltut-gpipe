import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.GLTut.Tut04.Models as Models
import qualified Graphics.UI.GLUT as GLUT

import Graphics.GPipe
import Data.Vec as V
import Prelude as P

main :: IO ()
main = do
    cube <- Models.load_cube
    -- enter common mainloop
    Framework.main keyboard
                   (displayIO cube)
                   initialize

-- Set up the window.
initialize :: GLUT.Window -> IO ()
initialize w = GLUT.idleCallback GLUT.$= (Just . GLUT.postRedisplay . Just $ w)

-- Handle keyboard events.
keyboard :: Char -> GLUT.Position -> IO ()
keyboard '\ESC' _ = do GLUT.leaveMainLoop
keyboard _      _ = do return ()

-- Perform IO on behalf of display. Call display to produce the framebuffer.
displayIO :: Models.PrimStream -> Vec2 Int -> IO (FrameBuffer RGBFormat () ())
displayIO stream size = do
    return $ display stream size

-- Combine scene elements on a framebuffer.
display :: Models.PrimStream -> Vec2 Int -> FrameBuffer RGBFormat () ()
display stream _ = draw fragments cleared
    where
        draw = paintColor NoBlending (RGB $ vec True)
        cleared = newFrameBufferColor (RGB $ vec 0)
        fragments = fmap fs
                  $ rasterizeBack
                  $ fmap (vs offset frustrumScale zNear zFar)
                  stream
        -- constant uniforms, calculated once
        offset = toGPU (0.5:.0.5:.(-2):.0:.()) -- Minor deviation from tutorial: We offset the Z of the vertex data by -2 here instead of duplicating the data inside the code.
        frustrumScale = toGPU 1
        zNear = toGPU 1
        zFar = toGPU 3

-- Offset the position. Perform projection manually.
vs  :: Vec4 (Vertex Float)
    -> Vertex Float
    -> Vertex Float
    -> Vertex Float
    -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
    -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
vs offset frustrumScale zNear zFar (pos, col) = (clipPos, col)
    where
        camX:.camY:.camZ:._:.() = pos + offset
        pr1 = (zNear + zFar) / (zNear - zFar)
        pr2 = (2 * zNear * zFar) / (zNear - zFar)
        clipPos = (camX * frustrumScale) :.
                  (camY * frustrumScale) :.
                  (camZ * pr1 + pr2) :.
                  (camZ / (-1)) :. ()

-- Use the provided color.
fs :: Vec4 (Fragment Float) -> Color RGBFormat (Fragment Float)
fs = RGB . V.take n3

-- eof
