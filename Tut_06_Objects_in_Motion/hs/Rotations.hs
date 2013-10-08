import System.Environment (getProgName)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec as V
import Prelude as P
import Data.IORef
-- local
--import Typenames
import RenderState
import Tetrahedron
import Util

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Main routines

main :: IO ()
main = do
    GLUT.getArgsAndInitialize
    n <- getProgName
    newWindow n (vec 8) (768:.512:.()) displayIO prepWindow
    GLUT.mainLoop

-- Set up the window before entering the GLUT mainloop
-- IO: Set GLUT state variables
prepWindow :: GLUT.Window -> IO ()
prepWindow w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)
    GLUT.keyboardMouseCallback GLUT.$= Just onKeyMouse
    -- depthClamp: How should zNear & zFar be enforced?
    --     Enabled: Clamp the depth of fragments (squish against glass).
    --     Disabled: Clip triangles (cut models).
    GLUT.depthClamp GLUT.$= GLUT.Enabled
    where
        -- Handle a keyboard or mouse event
        -- IO: leave the glut mainloop on some key events
        onKeyMouse :: GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
        onKeyMouse (GLUT.Char '\ESC') GLUT.Down _ _ = do GLUT.leaveMainLoop
        onKeyMouse _             _         _ _ = do return ()

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Types of translated object instances

type RotationFunc = Float -> Mat44 Float
data Instance = Instance { instCalcRotation :: RotationFunc
                         , instOffset :: Vec3 Float
                         }
-- Produce a rotation and translation matrix.
constructMatrix :: Instance -> Float -> Mat44 Float
constructMatrix inst elapsedTime = ro_mat
    where
        r_mat = instCalcRotation inst $ elapsedTime :: Mat44 Float
        offset = snoc (instOffset inst) 1 :: Vec4 Float
--      [[ r, r, r,ox],
--       [ r, r, r,oy],
--       [ r, r, r,oz],
--       [ 0, 0, 0, 1] ]
        ro_mat = transpose $ set n3 offset $ transpose r_mat :: Mat44 Float

-- A list of instances which produce translation/scaling matrices
g_instanceList :: [Instance]
g_instanceList = [ Instance nullRotation (( 0):.( 0):.(-25):.())
                 , Instance rotateX      ((-5):.(-5):.(-25):.())
                 , Instance rotateY      ((-5):.( 5):.(-25):.())
                 , Instance rotateZ      (( 5):.( 5):.(-25):.())
                 , Instance rotateAxis   (( 5):.(-5):.(-25):.())
                 , Instance rotateAxis2  (( 8):.( 0):.(-25):.())
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

-- Compute a lerp value [0, 1) which ranges from 0 to 1 to 0 every
-- loopDuration seconds of elapsedTime.
calcLerpFactor :: Float -> Float -> Float
calcLerpFactor elapsedTime loopDuration = 2 * zeroToHalf
    where
        loopFraction = mod' elapsedTime loopDuration / loopDuration
        zeroToHalf = if loopFraction > 0.5 then 1 - loopFraction else loopFraction

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
displayIO :: Vec2 Int -> IO ColorDepthBuffer
displayIO size = do
    rs <- makeRenderState size
    return $ display rs

display :: RenderState Float -> ColorDepthBuffer
--display rs = cleared
display rs = P.foldl (flip paint) cleared
        $ P.map (fmap fragmentShader)
        $ P.map rasterizeBack
        $ P.map (\mo2ca -> fmap (vertexShader mo2ca cameraToClipMatrix) tetrahedron)
        $ modelToCameraMatrices
    where 
        modelToCameraMatrices :: [Vf44]
        modelToCameraMatrices = P.map toGPU
                              $ P.map ($ rsSeconds rs)
                              $ P.map (constructMatrix $)
                              $ g_instanceList
        cameraToClipMatrix :: Vf44
        cameraToClipMatrix = toGPU $ let zNear = 1; zFar = 61; fov = 45
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
        position = snoc position3 1 -- add w coord
        cameraPos = multmv modelToCameraMatrix position
        gl_Position = multmv cameraToClipMatrix cameraPos

fragmentShader :: Ff3 -> RGB
fragmentShader color = RGB color

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- EOF
