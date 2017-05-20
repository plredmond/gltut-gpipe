import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.GLTut.RenderState as RenderState
import qualified Graphics.GLTut.MatrixStack as MatrixStack
import qualified Graphics.GLTut.Skeleton as Skeleton 
import qualified Graphics.GLTut.Tut06.RobotArm as RobotArm
import qualified Graphics.GLTut.Tut06.Models as Models
import qualified Data.HashMap.Lazy as HashMap
import qualified Graphics.UI.GLUT as GLUT

import Data.Monoid (mconcat)
import Control.Monad (forM_)
import Text.Printf (printf)
import Graphics.GLTut.RenderState (RenderState)
import Graphics.GLTut.Skeleton (Skeleton)

import Graphics.GPipe
import Data.Vec as V
import Prelude as P
import Data.IORef

main :: IO ()
main = do
    cube <- Models.load_cube
    ref <- newIORef RobotArm.pose
    -- enter common mainloop
    Framework.main (keyboard ref)
                   (displayIO ref $ RobotArm.base cube)
                   initialize

-- Set up the window.
initialize :: GLUT.Window -> IO ()
initialize w = GLUT.idleCallback GLUT.$= (Just . GLUT.postRedisplay . Just $ w)

-- Handle keyboard events.
keyboard :: IORef (Skeleton.Pose Float) -> Char -> GLUT.Position -> IO ()
keyboard _ '\ESC' _ = GLUT.leaveMainLoop
keyboard r 'a'    _ = modifyIORef r (Skeleton.adjustPose True  "baseRotY"    ) >> blurt r
keyboard r 'd'    _ = modifyIORef r (Skeleton.adjustPose False "baseRotY"    ) >> blurt r
keyboard r 'w'    _ = modifyIORef r (Skeleton.adjustPose False "upperarmRotX") >> blurt r
keyboard r 's'    _ = modifyIORef r (Skeleton.adjustPose True  "upperarmRotX") >> blurt r
keyboard r 'r'    _ = modifyIORef r (Skeleton.adjustPose False "lowerarmRotX") >> blurt r
keyboard r 'f'    _ = modifyIORef r (Skeleton.adjustPose True  "lowerarmRotX") >> blurt r
keyboard r 't'    _ = modifyIORef r (Skeleton.adjustPose False "wristRotX"   ) >> blurt r
keyboard r 'g'    _ = modifyIORef r (Skeleton.adjustPose True  "wristRotX"   ) >> blurt r
keyboard r 'z'    _ = modifyIORef r (Skeleton.adjustPose True  "wristRotZ"   ) >> blurt r
keyboard r 'c'    _ = modifyIORef r (Skeleton.adjustPose False "wristRotZ"   ) >> blurt r
keyboard r 'q'    _ = modifyIORef r (Skeleton.adjustPose True  "fingerRotY"  ) >> blurt r
keyboard r 'e'    _ = modifyIORef r (Skeleton.adjustPose False "fingerRotY"  ) >> blurt r
keyboard _ _      _ = return ()

-- Perform IO on behalf of display. Call display to produce the framebuffer.
displayIO :: IORef (Skeleton.Pose Float) -> Skeleton Models.PrimStream Float -> Vec2 Int -> IO (FrameBuffer RGBFormat DepthFormat ())
displayIO ref skel size = do
    rs <- RenderState.new size
    pose <- readIORef ref
    return $ display rs skel pose

-- Combine scene elements on a framebuffer.
display :: RenderState Float -> Skeleton Models.PrimStream Float -> Skeleton.Pose Float -> FrameBuffer RGBFormat DepthFormat ()
display rs skel pose = draw fragments cleared
    where
        draw = paintColorRastDepth Lequal True NoBlending (RGB $ vec True)
        cleared = newFrameBufferColorDepth (RGB $ vec 0) 1
        fragments = mconcat
                  . P.map (mkFragments cam2clip)
                  . Skeleton.flattenSkel MatrixStack.new -- convert the frozen skeleton into a list of matrices and streams
                  . Skeleton.freezeSkel pose             -- read the pose to find the current transformations of all bendable joints
                  $ skel
        -- variable uniforms, calculated every frame
        cam2clip = toGPU $ perspective 1 100 (45 * pi / 180) (RenderState.getAspectRatio rs)

-- Convert a scene element to a FragmentStream.
mkFragments :: Mat44 (Vertex Float)
            -> (Mat44 Float, Models.PrimStream)
            -> FragmentStream (Color RGBFormat (Fragment Float))
mkFragments cam2clip (mdl2cam, stream) = fmap fs
                                       $ rasterizeBack
                                       $ fmap (vs mdl2cam' cam2clip)
                                       $ fmap (\(p, c) -> (homPoint p, snoc c 1))
                                       stream
    where
        mdl2cam' = toGPU mdl2cam

-- Perform projection and transformation using the provided matrices.
vs  :: Mat44 (Vertex Float)
    -> Mat44 (Vertex Float)
    -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
    -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
vs mdl2cam cam2clip (pos, col) = (clipPos, col)
    where
        cameraPos = multmv mdl2cam pos
        clipPos = multmv cam2clip cameraPos

-- Use the provided color.
fs :: Vec4 (Fragment Float) -> Color RGBFormat (Fragment Float)
fs = RGB . V.take n3

-- Output the current pose positions.
blurt :: IORef (Skeleton.Pose Float) -> IO ()
blurt r = do
    pose <- readIORef r
    printf "Position:\n"
    forM_ (HashMap.toList pose) $ \(k, v) -> do
        printf "    %s \t%s\n" k (show v)

-- eof
