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
        (displayIO (toGPU 5) (toGPU 10))
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

displayIO :: Vertex Float -> Fragment Float -> Vec2 Int -> IO (FrameBuffer RGBAFormat () ())
displayIO duration fragduration size = do
    milliseconds <- GLUT.get GLUT.elapsedTime
    return $ display duration fragduration size (fromIntegral milliseconds / 1000)

display :: Vertex Float -> Fragment Float -> Vec2 Int -> Float -> FrameBuffer RGBAFormat () ()
display dur fdur size sec = draw fragments cleared
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
        fragments = fmap (fs fdur $ toGPU sec)
                  $ rasterizeBack
                  $ fmap (vs dur $ toGPU sec)
                  stream

stream :: PrimitiveStream Triangle (Vec4 (Vertex Float))
stream = toGPUStream TriangleList
    [ ( 0.25):.( 0.25):.0:.1:.()
    , ( 0.25):.(-0.25):.0:.1:.()
    , (-0.25):.(-0.25):.0:.1:.()
    ]

computePositionOffsets :: Vertex Float -> Vec2 (Vertex Float)
computePositionOffsets elapsedTime = (0.5 * cos (currTimeThroughLoop * scale)) :.
                                     (0.5 * sin (currTimeThroughLoop * scale)) :. ()
    where
        loopDuration = 5
        scale = pi * 2 / loopDuration
        currTimeThroughLoop = mod' elapsedTime loopDuration

vs :: Vertex Float -> Vertex Float -> Vec4 (Vertex Float) -> (Vec4 (Vertex Float), ())
vs dur sec pos = (xyoffset + pos, ())
    where
        xyoffset = V.append (computePositionOffsets sec) (vec 0)

fs :: Fragment Float -> Fragment Float -> () -> Color RGBAFormat (Fragment Float)
fs dur sec _ = RGBA (V.take n3 outputColor) (V.last outputColor)
    where
        currTime = mod' sec dur
        currLerp = currTime / dur
        comb = \a -> \b -> mix a b currLerp
        outputColor = V.zipWith comb (1:.1:.1:.1:.()) (0:.1:.0:.1:.())

-- eof
