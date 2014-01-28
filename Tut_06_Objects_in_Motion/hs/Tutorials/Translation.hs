import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.GLTut.RenderState as RenderState
import qualified Graphics.GLTut.Tut06.Models as Models
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
        cam2clip = toGPU $ perspective 1 45 (45 * pi / 180) (RenderState.getAspectRatio rs)

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
-- Types of translated object instances

type CalcOffset = Float -> Vec3 Float

-- Produce a translation matrix (identity with offset in the w-column)
constructMatrix :: CalcOffset -> Float -> Mat44 Float
constructMatrix calcOffset elapsedTime = o_mat
    where
        offset = homPoint $ calcOffset elapsedTime
--      Matrix plan:
--      [[ 1, 0, 0,ox],
--       [ 0, 1, 0,oy],
--       [ 0, 0, 1,oz],
--       [ 0, 0, 0, 1] ]
        o_mat = transpose $ set n3 offset identity

-- A list of functions which produce translation matrices
g_instanceList :: [CalcOffset]
g_instanceList = [ stationaryOffset
                 , ovalOffset
                 , bottomCircleOffset ]

stationaryOffset :: CalcOffset
stationaryOffset _ = 0 :. 0 :. (-20) :. ()

ovalOffset :: CalcOffset
ovalOffset elapsedTime = (4 * cos currRadsThroughLoop) :.
                         (6 * sin currRadsThroughLoop) :.
                         (-20) :.
                         ()
    where
        loopDuration = 3 -- sec
        sf = pi * 2 / loopDuration -- rad/sec
        currTimeThroughLoop = mod' elapsedTime loopDuration -- sec
        currRadsThroughLoop = currTimeThroughLoop * sf -- rad

bottomCircleOffset :: CalcOffset
bottomCircleOffset elapsedTime = (5 * cos currRadsThroughLoop) :.
                                 (-3.5) :.
                                 (5 * sin currRadsThroughLoop - 20) :.
                                 ()
    where
        loopDuration = 12 -- sec
        sf = pi * 2 / loopDuration -- rad/sec
        currTimeThroughLoop = mod' elapsedTime loopDuration -- sec
        currRadsThroughLoop = currTimeThroughLoop * sf -- rad

-- eof
