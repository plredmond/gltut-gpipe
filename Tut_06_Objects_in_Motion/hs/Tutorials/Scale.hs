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

-- Covert a scene element to a FragmentStream.
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
-- Types of scaled object instances

type ScaleFunc = Float -> Vec3 Float
type Instance = (ScaleFunc, Vec3 Float)

-- Produce a scale and translation matrix
constructMatrix :: Instance -> Float -> Mat44 Float
constructMatrix (calcScale, offset3) elapsedTime = so_mat
    where
        scalev = homPoint $ calcScale elapsedTime
        offset = homPoint offset3
--      Matrix plan:
--      [[sx, 0, 0,ox],
--       [ 0,sy, 0,oy],
--       [ 0, 0,sz,oz],
--       [ 0, 0, 0, 1] ]
        s_mat = diagonal scalev :: Mat44 Float
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
dynamicUniformScale elapsedTime = vec scalev
    where
        loopDuration = 3
        scalev = Easing.lerpThereAndBack elapsedTime loopDuration
                 `Easing.onRange` (1, 4)

dynamicNonUniformScale :: ScaleFunc
dynamicNonUniformScale elapsedTime = x:.y:.z:.()
    where
        xLoopDuration = 3
        zLoopDuration = 5
        x = Easing.lerpThereAndBack elapsedTime xLoopDuration
            `Easing.onRange` (1, 0.5)
        y = 1
        z = Easing.lerpThereAndBack elapsedTime zLoopDuration
            `Easing.onRange` (1, 10)

-- eof
