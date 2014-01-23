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
        -- primitive stream on gpu with baked-in offset
        xyoffset = V.append (computePositionOffsets sec) (vec 0)
        stream = toGPUStream TriangleList $ P.map (+ xyoffset) vertexes
        draw = paintColor NoBlending (RGB $ vec True)
        cleared = newFrameBufferColor (RGB $ vec 0)
        fragments = fmap fs
                  $ rasterizeBack
                  $ fmap vs
                  stream

vertexes :: [Vec4 Float]
vertexes = [ ( 0.25):.( 0.25):.0:.1:.()
           , ( 0.25):.(-0.25):.0:.1:.()
           , (-0.25):.(-0.25):.0:.1:.()
           ]

computePositionOffsets :: Float -> Vec2 Float
computePositionOffsets elapsedTime = (0.5 * cos (currTimeThroughLoop * sf)) :.
                                     (0.5 * sin (currTimeThroughLoop * sf)) :. ()
    where
        loopDuration = 5
        sf = pi * 2 / loopDuration
        currTimeThroughLoop = mod' elapsedTime loopDuration

vs :: Vec4 (Vertex Float) -> (Vec4 (Vertex Float), ())
vs pos = (pos, ())

fs :: () -> Color RGBFormat (Fragment Float)
fs _ = RGB $ vec 1

-- eof
