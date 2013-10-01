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

type ScaleFunc = Float -> Vec3 Float
data Instance = Instance { instCalcScale :: ScaleFunc
                         , instOffset :: Vec3 Float
                         }
-- Produce a scale and translation matrix.
constructMatrix :: Instance -> Float -> Mat44 Float
constructMatrix inst elapsedTime = so_mat
    where
        scale = snoc (instCalcScale inst $ elapsedTime) 1 :: Vec4 Float
        offset = snoc (instOffset inst) 1 :: Vec4 Float
--      [[sx, 0, 0,ox],
--       [ 0,sy, 0,oy],
--       [ 0, 0,sz,oz],
--       [ 0, 0, 0, 1] ]
        s_mat = diagonal scale :: Mat44 Float
        so_mat = transpose $ set n3 offset s_mat :: Mat44 Float

-- A list of instances which produce translation/scaling matrices
g_instanceList :: [Instance]
g_instanceList = [ Instance nullScale              ((  0):.(  0):.(-45):.())
                 , Instance staticUniformScale     ((-10):.(-10):.(-45):.())
                 , Instance staticNonUniformScale  ((-10):.( 10):.(-45):.())
                 , Instance dynamicUniformScale    (( 10):.( 10):.(-45):.())
                 , Instance dynamicNonUniformScale (( 10):.(-10):.(-45):.())
                 ]

nullScale :: ScaleFunc
nullScale _ = vec 1

staticUniformScale :: ScaleFunc
staticUniformScale _ = vec 4

staticNonUniformScale :: ScaleFunc
staticNonUniformScale _ = 0.5:.1:.10:.()
 
dynamicUniformScale :: ScaleFunc
dynamicUniformScale elapsedTime = vec scale
    where
        loopDuration = 3
        scale = mix 1 4 $ calcLerpFactor elapsedTime loopDuration

dynamicNonUniformScale :: ScaleFunc
dynamicNonUniformScale elapsedTime = x:.y:.z:.()
    where
        xLoopDuration = 3
        zLoopDuration = 5
        x = mix 1 0.5 $ calcLerpFactor elapsedTime xLoopDuration
        y = 1
        z = mix 1 10 $ calcLerpFactor elapsedTime zLoopDuration

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
