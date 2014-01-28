import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.GLTut.RenderState as RenderState
import qualified Graphics.GLTut.Tut06.Models as Models
import qualified Graphics.GLTut.Easing as Easing
import qualified Graphics.UI.GLUT as GLUT

import Data.Monoid (mconcat)
import Graphics.GLTut.RenderState (RenderState)

import Graphics.GPipe
import Data.Vec as V
import Prelude as P

main :: IO ()
main = do
    tetrahedron <- Models.load_tetrahedron
    -- enter common mainloop
    Framework.main keyboard
                   (displayIO tetrahedron)
                   initialize

-- Set up the window.
initialize :: GLUT.Window -> IO ()
initialize w = GLUT.idleCallback GLUT.$= (Just . GLUT.postRedisplay . Just $ w)

-- Handle keyboard events.
keyboard :: Char -> GLUT.Position -> IO ()
keyboard '\ESC' _ = do GLUT.leaveMainLoop
keyboard _      _ = do return ()

-- Perform IO on behalf of display. Call display to produce the framebuffer.
displayIO :: Models.PrimStream -> Vec2 Int -> IO (FrameBuffer RGBFormat DepthFormat ())
displayIO tetrahedron size = do
    rs <- RenderState.new size
    return $ display tetrahedron rs

-- Combine scene elements on a framebuffer.
display :: Models.PrimStream -> RenderState Float -> FrameBuffer RGBFormat DepthFormat ()
display tetrahedron rs = draw fragments cleared
    where
        draw = paintColorRastDepth Lequal True NoBlending (RGB $ vec True)
        cleared = newFrameBufferColorDepth (RGB $ vec 0) 1
        fragments = mconcat
                  . P.map (mkFragments cam2clip tetrahedron)
                  $ mdl2cams
        -- variable uniforms, calculated every frame
        mdl2cams = let sec = RenderState.getSeconds rs
                       mk inst = toGPU $ constructMatrix inst sec
                   in P.map mk g_instanceList
        cam2clip = toGPU $ perspective 1 61 (45 * pi / 180) (RenderState.getAspectRatio rs)

-- Convert a scene element to a FragmentStream.
mkFragments :: Mat44 (Vertex Float)
            -> Models.PrimStream
            -> Mat44 (Vertex Float)
            -> FragmentStream (Color RGBFormat (Fragment Float))
mkFragments cam2clip stream mdl2cam = fmap fs
                                    $ rasterizeBack
                                    $ fmap (vs mdl2cam cam2clip)
                                    $ fmap (\(p, c) -> (homPoint p, snoc c 1))
                                    stream

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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Types of rotated object instances

type RotationFunc = Float -> Mat44 Float
type Instance = (RotationFunc, Vec3 Float)

-- Produce a rotation and translation matrix.
constructMatrix :: Instance -> Float -> Mat44 Float
constructMatrix (calcRotation, offset3) elapsedTime = ro_mat
    where
        r_mat = calcRotation elapsedTime
        offset = homPoint offset3
--      Matrix plan:
--      [[ r, r, r,ox],
--       [ r, r, r,oy],
--       [ r, r, r,oz],
--       [ 0, 0, 0, 1] ]
        ro_mat = transpose $ set n3 offset $ transpose r_mat :: Mat44 Float

-- A list of instances which produce translation/scaling matrices
g_instanceList :: [Instance]
g_instanceList = [ (nullRotation, ( 0):.( 0):.(-25):.())
                 , (rotateX,      (-5):.(-5):.(-25):.())
                 , (rotateY,      (-5):.( 5):.(-25):.())
                 , (rotateZ,      ( 5):.( 5):.(-25):.())
                 , (rotateAxis,   ( 5):.(-5):.(-25):.())
                 ]

nullRotation :: RotationFunc
nullRotation _ = identity

rotateX :: RotationFunc
rotateX elapsedTime = rotationX $ computeAngleRad elapsedTime 3

rotateY :: RotationFunc
rotateY elapsedTime = rotationY $ computeAngleRad elapsedTime 2

rotateZ :: RotationFunc
rotateZ elapsedTime = rotationZ $ computeAngleRad elapsedTime 2

rotateAxis :: RotationFunc
rotateAxis elapsedTime = rotationVec axis angRad
    where
        axis = normalize $ vec 1 :: Vec3 Float
        angRad = computeAngleRad elapsedTime 2

-- Compute an angle in radians [0, 2*pi) which ranges from 0 to 2*pi
-- every loopDuration seconds of elapsedTime.
computeAngleRad :: Float -> Float -> Float
computeAngleRad elapsedTime loopDuration = Easing.lerpThere elapsedTime loopDuration
                                           `Easing.onRange` (0, 2 * pi)

-- eof
