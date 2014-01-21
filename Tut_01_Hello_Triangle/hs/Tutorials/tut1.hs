import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.UI.GLUT as GLUT

import Graphics.GPipe
import Data.Vec

main :: IO ()
main = Framework.main keyboard displayIO initialize

keyboard :: Char -> GLUT.Position -> IO ()
keyboard '\ESC' _ = do GLUT.leaveMainLoop
keyboard _      _ = do return ()

initialize :: GLUT.Window -> IO ()
initialize w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)

displayIO :: Vec2 Int -> IO (FrameBuffer RGBAFormat () ())
displayIO size = do
    return $ display size

display :: Vec2 Int -> FrameBuffer RGBAFormat () ()
display size = draw fragments cleared
    where
        -- draw -- curry blending mode and boolean color mask onto paintColor
        draw :: FragmentStream (Color RGBAFormat (Fragment Float))
                -> FrameBuffer RGBAFormat () ()
                -> FrameBuffer RGBAFormat () ()
        draw = paintColor NoBlending (RGBA (vec True) True)
        -- cleared -- a solid color framebuffer
        cleared :: FrameBuffer RGBAFormat () ()
        cleared = newFrameBufferColor $ RGBA (vec 0) 1
        -- fragment stream
        fragments :: FragmentStream (Color RGBAFormat (Fragment Float))
        fragments = fmap fs
                  $ rasterizeBack
                  $ fmap vs
                  stream

stream :: PrimitiveStream Triangle (Vec4 (Vertex Float))
stream = toGPUStream TriangleList $
    [   0.75  :.   0.75  :.  0.00  :.  1.00  :.  ()
    ,   0.75  :. (-0.75) :.  0.00  :.  1.00  :.  ()
    , (-0.75) :. (-0.75) :.  0.00  :.  1.00  :.  ()
    ]

vs :: Vec4 (Vertex Float) -> (Vec4 (Vertex Float), ())
vs pos = (pos, ())

fs :: () -> Color RGBAFormat (Fragment Float)
fs _ = RGBA (vec 1) 1

-- eof
