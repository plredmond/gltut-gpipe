import System.Environment (getProgName)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec as V
import Prelude as P
import Data.IORef
import Data.HashMap.Lazy as H -- for toList

import RenderState
import MatrixStack
import Skeleton
import RobotArm

main :: IO ()
main = do
    GLUT.getArgsAndInitialize
    n <- getProgName
    r <- newIORef pose
    newWindow
        n -- window title
        (300:.200:.()) -- desired window position
        (500:.500:.()) -- desired window size
        (displayIO r)
        (initWindow r)
    GLUT.mainLoop

initWindow :: IORef (Pose Float) -> GLUT.Window -> IO ()
initWindow r w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)
    GLUT.keyboardMouseCallback GLUT.$= Just onKeyMouse
    GLUT.depthClamp GLUT.$= GLUT.Enabled
    where
        blurt = do
            pose <- readIORef r
            putStrLn $ "Position:"
            mapM_ (\(k,v) -> do
                    putStrLn ("    "++k++": \t"++(show $ pCurrent v))
                ) (H.toList pose)
        onKeyMouse :: GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
        onKeyMouse (GLUT.Char '\ESC') GLUT.Down _ _ = do GLUT.leaveMainLoop
        onKeyMouse (GLUT.Char 'a') GLUT.Down _ _ = do modifyIORef r (adjustPose True  "baseRotY"); blurt
        onKeyMouse (GLUT.Char 'd') GLUT.Down _ _ = do modifyIORef r (adjustPose False "baseRotY"); blurt
        onKeyMouse (GLUT.Char 'w') GLUT.Down _ _ = do modifyIORef r (adjustPose False "upperarmRotX"); blurt
        onKeyMouse (GLUT.Char 's') GLUT.Down _ _ = do modifyIORef r (adjustPose True  "upperarmRotX"); blurt
        onKeyMouse (GLUT.Char 'r') GLUT.Down _ _ = do modifyIORef r (adjustPose False "lowerarmRotX"); blurt
        onKeyMouse (GLUT.Char 'f') GLUT.Down _ _ = do modifyIORef r (adjustPose True  "lowerarmRotX"); blurt
        onKeyMouse (GLUT.Char 't') GLUT.Down _ _ = do modifyIORef r (adjustPose False "wristRotX"); blurt
        onKeyMouse (GLUT.Char 'g') GLUT.Down _ _ = do modifyIORef r (adjustPose True  "wristRotX"); blurt
        onKeyMouse (GLUT.Char 'z') GLUT.Down _ _ = do modifyIORef r (adjustPose True  "wristRotZ"); blurt
        onKeyMouse (GLUT.Char 'c') GLUT.Down _ _ = do modifyIORef r (adjustPose False "wristRotZ"); blurt
        onKeyMouse (GLUT.Char 'q') GLUT.Down _ _ = do modifyIORef r (adjustPose True  "fingerRotY"); blurt
        onKeyMouse (GLUT.Char 'e' ) GLUT.Down _ _ = do modifyIORef r (adjustPose False "fingerRotY"); blurt
        onKeyMouse _ _ _ _ = do return ()

displayIO :: IORef (Pose Float) -> Vec2 Int -> IO (FrameBuffer RGBAFormat DepthFormat ())
displayIO ref size = do
    rs <- mkRenderState size
    p <- readIORef ref
    return $ display rs p

display :: RenderState Float -> Pose Float -> FrameBuffer RGBAFormat DepthFormat ()
display rs p = P.foldl (flip draw) cleared -- draw in-order onto the framebuffer
           $ P.map (mkFragments cameraToClipMatrix)
           $ flattenSkel new -- convert the frozen skeleton into a list of matrices and streams using an empty MatrixStack
           $ freezeSkel p base -- read the pose to find the current transformations of all bendible joints
    where
        -- draw -- curry blending mode and boolean color mask onto paintColor
        draw :: FragmentStream (Color RGBAFormat (Fragment Float))
                -> FrameBuffer RGBAFormat DepthFormat ()
                -> FrameBuffer RGBAFormat DepthFormat ()
        draw = paintColorRastDepth Less True NoBlending (RGBA (vec True) True)
        -- cleared -- a solid color framebuffer
        cleared :: FrameBuffer RGBAFormat DepthFormat ()
        cleared = newFrameBufferColorDepth (RGBA (vec 0) 1) 1
        -- these matrices are uniforms calculated every frame
        cameraToClipMatrix = toGPU $ perspective 1 45 (45 * pi / 180) (rsAspectRatio rs)

mkFragments :: Mat44 (Vertex Float) -> (Mat44 Float, PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float))) -> FragmentStream (Color RGBAFormat (Fragment Float))
mkFragments cam2clip (model2cam, mesh) = fmap fs
                                       $ rasterizeBack
                                       $ fmap (vs (toGPU model2cam, cam2clip))
                                       $ fmap vs0 mesh

-- Make the stream use Vec4 for pos and col.
vs0 :: (Vec3 (Vertex Float), Vec3 (Vertex Float)) -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
vs0 (pos3, rgb) = (homPoint pos3, snoc rgb 1)

-- Offset the position. Perform projection using the provided matrix.
vs :: (Mat44 (Vertex Float), Mat44 (Vertex Float)) -> (Vec4 (Vertex Float), Vec4 (Vertex Float)) -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
vs (modelToCameraMatrix, cameraToClipMatrix) (pos, col) = (clipPos, col)
    where
        camPos = multmv modelToCameraMatrix pos
        clipPos = multmv cameraToClipMatrix camPos

fs :: Vec4 (Fragment Float) -> Color RGBAFormat (Fragment Float)
fs col = RGBA (V.take n3 col) (V.last col)

-- eof
