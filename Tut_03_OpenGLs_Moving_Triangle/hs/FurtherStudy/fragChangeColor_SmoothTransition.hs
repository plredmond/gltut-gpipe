import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.UI.GLUT as GLUT

import Graphics.GPipe
import Data.Vec as V
import Prelude as P

main :: IO ()
main = Framework.main keyboard displayIO initialize

initialize :: GLUT.Window -> IO ()
initialize w = GLUT.idleCallback GLUT.$= (Just . GLUT.postRedisplay . Just $ w)

keyboard :: Char -> GLUT.Position -> IO ()
keyboard '\ESC' _ = do GLUT.leaveMainLoop
keyboard _      _ = do return ()

displayIO :: Vec2 Int -> IO (FrameBuffer RGBFormat () ())
displayIO size = do
    milliseconds <- GLUT.get GLUT.elapsedTime
    return $ display size (fromIntegral milliseconds / 1000)

display :: Vec2 Int -> Float -> FrameBuffer RGBFormat () ()
display _ sec = draw fragments cleared
    where
        draw = paintColor NoBlending (RGB $ vec True)
        cleared = newFrameBufferColor (RGB $ vec 0)
        fragments = fmap (fs $ toGPU sec)
                  $ rasterizeBack
                  $ fmap (vs $ toGPU sec)
                  stream

stream :: PrimitiveStream Triangle (Vec4 (Vertex Float))
stream = toGPUStream TriangleList
    [ ( 0.25):.( 0.25):.0:.1:.()
    , ( 0.25):.(-0.25):.0:.1:.()
    , (-0.25):.(-0.25):.0:.1:.()
    ]

computePositionOffsets :: Vertex Float -> Vec2 (Vertex Float)
computePositionOffsets elapsedTime = (0.5 * cos (currTimeThroughLoop * sf)) :.
                                     (0.5 * sin (currTimeThroughLoop * sf)) :. ()
    where
        loopDuration = 5
        sf = pi * 2 / loopDuration
        currTimeThroughLoop = mod' elapsedTime loopDuration

vs :: Vertex Float -> Vec4 (Vertex Float) -> (Vec4 (Vertex Float), ())
vs sec pos = (xyoffset + pos, ())
    where
        xyoffset = V.append (computePositionOffsets sec) (vec 0)

fs :: Fragment Float -> () -> Color RGBFormat (Fragment Float)
fs sec _ = RGB outputColor
    where
        dur = 10
        sf = pi * 2 / dur
        currTime = mod' sec dur
        currLerp = 0.5 * (1 + (sin $ currTime * sf))
        comb = \a -> \b -> mix a b currLerp
        outputColor = V.zipWith comb (1:.1:.1:.()) (0:.1:.0:.())

-- eof
