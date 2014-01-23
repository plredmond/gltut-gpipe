import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.UI.GLUT as GLUT

import Graphics.GPipe
import Data.Vec as V

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
        draw = paintColor NoBlending (RGB $ vec True)
        cleared = newFrameBufferColor (RGB $ vec 0)
        fragments = fmap fs
                  $ rasterizeBack
                  $ fmap vs
                  stream

stream :: PrimitiveStream Triangle (Vec4 (Vertex Float), Vec4 (Vertex Float))
stream = toGPUStream TriangleList $
    [ (  0   :.  0.5   :.0:.1:.(), 1:.0:.0:.1:.())
    , (  0.5 :.(-0.366):.0:.1:.(), 0:.1:.0:.1:.())
    , ((-0.5):.(-0.366):.0:.1:.(), 0:.0:.1:.1:.())
    ]

vs :: (Vec4 (Vertex Float), Vec4 (Vertex Float)) -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
vs = id

fs :: Vec4 (Fragment Float) -> Color RGBFormat (Fragment Float)
fs col = RGB (V.take n3 col)

-- eof
