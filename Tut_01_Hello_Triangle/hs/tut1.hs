import System.Environment (getProgName)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec

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
fs col = RGBA (vec 1) 1

-- eof
