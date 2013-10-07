import System.Environment (getProgName)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec as V
import Prelude as P

import Tetrahedron
import RenderState

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
-- Types of translated object instances

type CalcOffset = Float -> Vec3 Float

stationaryOffset :: CalcOffset
stationaryOffset elapsedTime = 0 :. 0 :. (-20) :. ()

ovalOffset :: CalcOffset
ovalOffset elapsedTime = (4 * cos currRadsThroughLoop) :.
                         (6 * sin currRadsThroughLoop) :.
                         (-20) :.
                         ()
    where
        loopDuration = 3 -- sec
        scale = pi * 2 / loopDuration -- rad/sec
        currTimeThroughLoop = mod' elapsedTime loopDuration -- sec
        currRadsThroughLoop = currTimeThroughLoop * scale -- rad

bottomCircleOffset :: CalcOffset
bottomCircleOffset elapsedTime = (5 * cos currRadsThroughLoop) :.
                                 (-3.5) :.
                                 (5 * sin currRadsThroughLoop - 20) :.
                                 ()
    where
        loopDuration = 12 -- sec
        scale = pi * 2 / loopDuration -- rad/sec
        currTimeThroughLoop = mod' elapsedTime loopDuration -- sec
        currRadsThroughLoop = currTimeThroughLoop * scale -- rad

-- Produce a translation matrix (identity with offset in the w-column).
constructMatrix :: CalcOffset -> Float -> Mat44 Float
constructMatrix calcOffset elapsedTime = transpose $ set n3 offset identity
    where
        offset :: Vec4 Float
        offset = snoc (calcOffset elapsedTime) 1.0

-- A list of functions which produce translation matrices
g_instanceList :: [CalcOffset]
g_instanceList = [ stationaryOffset
                 , ovalOffset
                 , bottomCircleOffset ]

-- eof
