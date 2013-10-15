import System.Environment (getProgName)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec as V
import Prelude as P

import Lib.RenderState
import Lib.AnimUtils
import DatLib.Tetrahedron

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
-- Types of scaled object instances

type ScaleFunc = Float -> Vec3 Float
type Instance = (ScaleFunc, Vec3 Float)

-- Produce a scale and translation matrix
constructMatrix :: Instance -> Float -> Mat44 Float
constructMatrix (calcScale, offset3) elapsedTime = so_mat
    where
        scale = homPoint $ calcScale elapsedTime
        offset = homPoint offset3
--      [[sx, 0, 0,ox],
--       [ 0,sy, 0,oy],
--       [ 0, 0,sz,oz],
--       [ 0, 0, 0, 1] ]
        s_mat = diagonal scale :: Mat44 Float
        so_mat = transpose $ set n3 offset s_mat :: Mat44 Float

-- A list of instances which produce translation/scaling matrices
g_instanceList :: [Instance]
g_instanceList = [ (nullScale,              (  0):.(  0):.(-45):.())
                 , (staticUniformScale,     (-10):.(-10):.(-45):.())
                 , (staticNonUniformScale,  (-10):.( 10):.(-45):.())
                 , (dynamicUniformScale,    ( 10):.( 10):.(-45):.())
                 , (dynamicNonUniformScale, ( 10):.(-10):.(-45):.())
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

-- eof
