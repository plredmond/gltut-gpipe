import System.Environment (getArgs, getProgName)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec as V
import Prelude as P

import Args
import Load

main :: IO ()
main = do
    GLUT.getArgsAndInitialize
    n <- getProgName
    f <- parseArgs
    s <- readFile f
    newWindow
        n -- window title
        (300:.200:.()) -- desired window position
        (500:.500:.()) -- desired window size
        (displayIO $ readStream s)
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

displayIO :: PrimitiveStream Triangle (Vec4 (Vertex Float), Vec4 (Vertex Float)) -> Vec2 Int -> IO (FrameBuffer RGBAFormat () ())
displayIO stream size = do
    return $ display stream size

display :: PrimitiveStream Triangle (Vec4 (Vertex Float), Vec4 (Vertex Float)) -> Vec2 Int -> FrameBuffer RGBAFormat () ()
display stream size = draw fragments cleared
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
                  $ fmap (vs offset (toGPU 1) (toGPU 1) (toGPU 3))
                  stream
        -- offset is a constant uniform calculated only once
        offset = toGPU (0.5:.0.5:.(-2):.0:.()) -- Minor deviation from tutorial: We offset the Y of the vertex data by -2 here.

-- Offset the position. Perform projection manually.
vs :: Vec4 (Vertex Float) -> Vertex Float -> Vertex Float -> Vertex Float -> (Vec4 (Vertex Float), Vec4 (Vertex Float)) -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
vs offset frustrumScale zNear zFar (pos, col) = (clipPos, col)
    where
        camX:.camY:.camZ:.camW:.() = pos + offset
        pr1 = (zNear + zFar) / (zNear - zFar)
        pr2 = (2 * zNear * zFar) / (zNear - zFar)
        clipPos = (camX * frustrumScale) :.
                  (camY * frustrumScale) :.
                  (camZ * pr1 + pr2) :.
                  (-camZ) :. ()

fs :: Vec4 (Fragment Float) -> Color RGBAFormat (Fragment Float)
fs col = RGBA (V.take n3 col) (V.last col)

-- eof
