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
                  $ fmap (vs offset)
                  stream
        -- constant uniforms, calculated once
        offset = toGPU (0.5:.0.25:.0:.0:.())

-- Offset the position. Don't perform any projection.
vs  :: Vec4 (Vertex Float)
    -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
    -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
vs offset (pos, col) = (offset + pos, col)

-- Use the provided color.
fs :: Vec4 (Fragment Float) -> Color RGBFormat (Fragment Float)
fs = RGB . V.take n3

-- eof
