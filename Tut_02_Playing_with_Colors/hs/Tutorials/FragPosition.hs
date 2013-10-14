import System.Environment (getProgName)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec as V

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
                  $ fmap (vs $ toGPU $ V.map fromIntegral size) -- give window size as a uniform (Tut 3's homework)
                  stream

stream :: PrimitiveStream Triangle (Vec4 (Vertex Float))
stream = toGPUStream TriangleList $
    [   0.75  :.   0.75  :.  0.00  :.  1.00  :.  ()
    ,   0.75  :. (-0.75) :.  0.00  :.  1.00  :.  ()
    , (-0.75) :. (-0.75) :.  0.00  :.  1.00  :.  ()
    ]

-- Contrary to the cpp tutorial, the window coordinates of a fragment aren't
-- provided to each invocation of a fragment shader in GPipe. To implement the
-- same behavior as the tutorial we manually convert the clip-space coordinates
-- to window coordinates in the vertex shader, and then pass the result out for
-- interpolation accross the generated fragments.

-- clip-space coordinates: [-1,1] [-1,1] [-1,1] 1
-- window coordinates: [0,w] [0,h] [0,1]
vs_clip2win :: Vec2 (Vertex Float) -> Vec4 (Vertex Float) -> Vec3 (Vertex Float)
vs_clip2win (w:.h:.()) pos@(_:._:._:.hom_w:.()) = (x*w):.(y*h):.z:.()
    where
        x:.y:.z:._:.() = V.map ((/2) . (+1) . (/hom_w)) pos

vs :: Vec2 (Vertex Float) -> Vec4 (Vertex Float) -> (Vec4 (Vertex Float), Vec3 (Vertex Float))
vs size pos = (pos, vs_clip2win size pos)

fs :: Vec3 (Fragment Float) -> Color RGBAFormat (Fragment Float)
fs (_:.y:._:.()) = RGBA outputColor 1
    where
        lerpValue = y / 500
        outputColor = vec $ mix 1 0.2 lerpValue

-- eof
