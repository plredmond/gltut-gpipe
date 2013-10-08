import System.Environment (getProgName)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec as V
import Prelude as P

import Tetrahedron
import RenderState
import AnimUtils

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
    GLUT.depthClamp GLUT.$= GLUT.Enabled
    where
        onKeyMouse :: GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
        onKeyMouse (GLUT.Char '\ESC') GLUT.Down _ _ = do GLUT.leaveMainLoop
        onKeyMouse _ _ _ _ = do return ()

displayIO :: Vec2 Int -> IO (FrameBuffer RGBAFormat DepthFormat ())
displayIO size = do
    rs <- mkRenderState size
    return $ display rs

display :: RenderState Float -> FrameBuffer RGBAFormat DepthFormat ()
display rs = P.foldl (flip draw) cleared -- draw in-order onto the framebuffer
           $ P.map mkFragments 
           $ zip modelToCameraMatrices (repeat cameraToClipMatrix)
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
        modelToCameraMatrices = P.map toGPU
                              $ P.map ($ rsSeconds rs)
                              $ P.map (constructMatrix $)
                              $ g_instanceList

mkFragments :: (Mat44 (Vertex Float), Mat44 (Vertex Float)) -> FragmentStream (Color RGBAFormat (Fragment Float))
mkFragments mattup = fmap fs
                   $ rasterizeBack
                   $ fmap (vs mattup)
                   $ fmap vs0 tetrahedron -- imported immutable stream

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
                 --, (rotateAxis2,  ( 8):.( 0):.(-25):.())
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

rotateAxis2 :: RotationFunc
rotateAxis2 elapsedTime = rotationVec axis angRad
    where
        lerp = calcLerpFactor elapsedTime 6
        axis = normalize $ (lerp:.(1 - lerp):.0:.()) :: Vec3 Float
        angRad = computeAngleRad elapsedTime 2

-- Compute an angle in radians [0, 2*pi) which ranges from 0 to 2*pi
-- every loopDuration seconds of elapsedTime.
computeAngleRad :: Float -> Float -> Float
computeAngleRad elapsedTime loopDuration = curTimeThroughLoop * scale
    where
        curTimeThroughLoop = mod' elapsedTime loopDuration
        scale = pi * 2 / loopDuration

-- eof
