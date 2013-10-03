import System.Environment (getProgName)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec as V
import Prelude as P

main :: IO ()
main = do
    GLUT.getArgsAndInitialize
    n <- getProgName
    newWindow
        n -- window title
        (300:.200:.()) -- desired window position
        (500:.500:.()) -- desired window size
        displayIO
        initWindow
    GLUT.mainLoop

initWindow :: GLUT.Window -> IO ()
initWindow w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)
    GLUT.keyboardMouseCallback GLUT.$= Just onKeyMouse
    where
        onKeyMouse :: GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
        onKeyMouse (GLUT.Char '\ESC') GLUT.Down _ _ = do GLUT.leaveMainLoop
        onKeyMouse _ _ _ _ = do return ()

displayIO :: Vec2 Int -> IO (FrameBuffer RGBAFormat () ())
displayIO size = do
    milliseconds <- GLUT.get GLUT.elapsedTime
    return $ display size (fromIntegral milliseconds / 1000)

display :: Vec2 Int -> Float -> FrameBuffer RGBAFormat () ()
display size sec = draw fragments cleared
    where
        -- primitive stream on gpu
        xyoffset :: Vec4 Float
        xyoffset = V.append (computePositionOffsets sec) (vec 0)
        stream = toGPUStream TriangleList $ P.map (+ xyoffset) vertexes
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

vertexes :: [Vec4 Float]
vertexes = [ ( 0.25):.( 0.25):.0:.1:.()
           , ( 0.25):.(-0.25):.0:.1:.()
           , (-0.25):.(-0.25):.0:.1:.()
           ]

computePositionOffsets :: Float -> Vec2 Float
computePositionOffsets elapsedTime = (0.5 * cos (currTimeThroughLoop * scale)) :.
                                     (0.5 * sin (currTimeThroughLoop * scale)) :. ()
    where
        loopDuration = 5
        scale = pi * 2 / loopDuration
        currTimeThroughLoop = mod' elapsedTime loopDuration

vs :: Vec4 (Vertex Float) -> (Vec4 (Vertex Float), ())
vs pos = (pos, ())

fs :: () -> Color RGBAFormat (Fragment Float)
fs _ = RGBA (vec 1) 1

-- eof
