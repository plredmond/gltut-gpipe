import System.Environment (getProgName)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec as V
import Prelude as P
import Data.IORef
import Control.Monad -- for join
import Data.HashMap.Lazy as H -- for toList
-- local
import RenderState
import Util
import MatrixStack
import Skeleton
import RobotArm

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Main routines

main :: IO ()
main = do
    GLUT.getArgsAndInitialize
    n <- getProgName
    r <- newIORef pose
    newWindow n (vec 8) (768:.512:.()) (displayIO r) (prepWindowIO r)
    GLUT.mainLoop

-- Set up the window before entering the GLUT mainloop
-- IO: Set GLUT state variables
prepWindowIO :: IORef (Pose Float) -> GLUT.Window -> IO ()
prepWindowIO r w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)
    GLUT.keyboardMouseCallback GLUT.$= Just onKeyMouse
    -- depthClamp: How should zNear & zFar be enforced?
    --     Enabled: Clamp the depth of fragments (squish against glass).
    --     Disabled: Clip triangles (cut models).
    GLUT.depthClamp GLUT.$= GLUT.Enabled
    where
        blurt = do
            pose <- readIORef r
            putStrLn $ "Position:"
            mapM_ (\(k,v) -> do
                    putStrLn ("    "++k++": \t"++(show $ pCurrent v))
                ) (H.toList pose)
        -- Handle a keyboard or mouse event
        -- IO: leave the glut mainloop on some key events
        onKeyMouse :: GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
        onKeyMouse (GLUT.Char '\ESC') GLUT.Down _ _ = do GLUT.leaveMainLoop
        onKeyMouse (GLUT.Char 'a') GLUT.Down _ _ = do modifyIORef r (adjustPose True  "baseRotY"); blurt
        onKeyMouse (GLUT.Char 'e') GLUT.Down _ _ = do modifyIORef r (adjustPose False "baseRotY"); blurt
        onKeyMouse (GLUT.Char ',') GLUT.Down _ _ = do modifyIORef r (adjustPose False "upperarmRotX"); blurt
        onKeyMouse (GLUT.Char 'o') GLUT.Down _ _ = do modifyIORef r (adjustPose True  "upperarmRotX"); blurt
        onKeyMouse (GLUT.Char 'p') GLUT.Down _ _ = do modifyIORef r (adjustPose False "lowerarmRotX"); blurt
        onKeyMouse (GLUT.Char 'u') GLUT.Down _ _ = do modifyIORef r (adjustPose True  "lowerarmRotX"); blurt
        onKeyMouse (GLUT.Char 'y') GLUT.Down _ _ = do modifyIORef r (adjustPose False "wristRotX"); blurt
        onKeyMouse (GLUT.Char 'i') GLUT.Down _ _ = do modifyIORef r (adjustPose True  "wristRotX"); blurt
        onKeyMouse (GLUT.Char ';') GLUT.Down _ _ = do modifyIORef r (adjustPose True  "wristRotZ"); blurt
        onKeyMouse (GLUT.Char 'j') GLUT.Down _ _ = do modifyIORef r (adjustPose False "wristRotZ"); blurt
        onKeyMouse (GLUT.Char '\'') GLUT.Down _ _ = do modifyIORef r (adjustPose True  "fingerRotY"); blurt
        onKeyMouse (GLUT.Char '.' ) GLUT.Down _ _ = do modifyIORef r (adjustPose False "fingerRotY"); blurt
        onKeyMouse _ _ _ _ = do return ()

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Display function

type ColorDepthBuffer = FrameBuffer RGBFormat DepthFormat ()

-- An empty framebuffer with depth and color
cleared :: ColorDepthBuffer
cleared = newFrameBufferColorDepth c d
    where
        c = RGB (vec 0)
        d = 1

paint = paintColorRastDepth Lequal True NoBlending (RGB $ vec True)

-- Produce a framebuffer of animation, provided the size of the window
-- IO: Call makeRenderState
displayIO :: IORef (Pose Float) -> Vec2 Int -> IO ColorDepthBuffer
displayIO ref size = do
    rs <- makeRenderState size
    p <- readIORef ref
    return $ display rs p

display :: RenderState Float -> Pose Float -> ColorDepthBuffer
display rs p = P.foldl (flip paint) cleared
        $ P.map (fmap fragmentShader)
        $ P.map rasterizeBack
        $ P.map (\(mo2ca, mesh) -> fmap (vertexShader (toGPU mo2ca) ca2cli) mesh)
        $ flattenSkel new
        $ freezeSkel p base
    where 
        ca2cli :: Vf44
        ca2cli = toGPU $ let zNear = 1; zFar = 100; fov = 45
            in perspective zNear zFar (degToRad fov) (rsAspectRatio rs)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Shader functions

type Vf  = Vertex Float
type Vf3 = Vec3 Vf
type Vf4 = Vec4 Vf
type Vf44 = Mat44 Vf

type Ff  = Fragment Float
type Ff3 = Vec3 Ff
type RGB = Color RGBFormat Ff

vertexShader :: Vf44 -> Vf44 -> (Vf3, Vf3) -> (Vf4, Vf3)
vertexShader modelToCameraMatrix cameraToClipMatrix (position3, color) = (gl_Position, color)
    where
        position = homPoint position3 -- add w coord
        cameraPos = multmv modelToCameraMatrix position
        gl_Position = multmv cameraToClipMatrix cameraPos

fragmentShader :: Ff3 -> RGB
fragmentShader color = RGB color

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- EOF
