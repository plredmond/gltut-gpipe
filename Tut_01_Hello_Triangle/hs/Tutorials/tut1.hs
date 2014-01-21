import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.UI.GLUT as GLUT

import Graphics.GPipe
import Data.Vec

main :: IO ()
main = Framework.main keyboard displayIO (\_ -> return ())

keyboard :: Char -> GLUT.Position -> IO ()
keyboard '\ESC' _ = do GLUT.leaveMainLoop
keyboard _      _ = do return ()

displayIO :: Vec2 Int -> IO (FrameBuffer RGBFormat () ())
displayIO size = do
    return $ display size

display :: Vec2 Int -> FrameBuffer RGBFormat () ()
display _ = draw fragments cleared
    where
        -- draw -- curry blending mode and boolean color mask onto paintColor
        draw :: FragmentStream (Color RGBFormat (Fragment Float))
                -> FrameBuffer RGBFormat () ()
                -> FrameBuffer RGBFormat () ()
        draw = paintColor NoBlending (RGB $ vec True)
        -- cleared -- a solid color framebuffer
        cleared :: FrameBuffer RGBFormat () ()
        cleared = newFrameBufferColor (RGB $ vec 0)
        -- fragment stream
        fragments :: FragmentStream (Color RGBFormat (Fragment Float))
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

fs :: () -> Color RGBFormat (Fragment Float)
fs _ = RGB $ vec 1

-- eof
