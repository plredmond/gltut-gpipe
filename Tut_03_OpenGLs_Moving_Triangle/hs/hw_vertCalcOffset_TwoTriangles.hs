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
        (displayIO $ toGPU 5)
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

displayIO :: Vertex Float -> Vec2 Int -> IO (FrameBuffer RGBAFormat () ())
displayIO duration size = do
    milliseconds <- GLUT.get GLUT.elapsedTime
    return $ display duration size (fromIntegral milliseconds / 1000)

display :: Vertex Float -> Vec2 Int -> Float -> FrameBuffer RGBAFormat () ()
display dur size sec = P.foldr draw cleared fragments_s
    where
        -- draw -- curry blending mode and boolean color mask onto paintColor
        draw :: FragmentStream (Color RGBAFormat (Fragment Float))
                -> FrameBuffer RGBAFormat () ()
                -> FrameBuffer RGBAFormat () ()
        draw = paintColor NoBlending (RGBA (vec True) True)
        -- cleared -- a solid color framebuffer
        cleared :: FrameBuffer RGBAFormat () ()
        cleared = newFrameBufferColor $ RGBA (vec 0) 1
        -- list of fragment streams
        fragments_s :: [FragmentStream (Color RGBAFormat (Fragment Float))]
        fragments_s = P.map (fmap fs)
                    $ P.map rasterizeBack
                    $ P.map (\(stream', sec') -> fmap (vs dur sec') stream')
                    [ (stream, toGPU sec)
                    , (stream, toGPU $ sec + 2.5)
                    ]

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

fs :: () -> Color RGBAFormat (Fragment Float)
fs _ = RGBA (vec 1) 1

-- eof
