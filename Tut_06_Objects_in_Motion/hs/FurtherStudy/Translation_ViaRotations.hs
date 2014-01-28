import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.GLTut.RenderState as RenderState
import qualified Graphics.GLTut.Tut06.Models as Models
import qualified Graphics.UI.GLUT as GLUT

import Data.Monoid (mconcat)
import Graphics.GLTut.RenderState (RenderState)

import Graphics.GLTut.MatrixStack
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
-- Types of translated object instances

type MakeStack = Float -> MatrixStack Float

-- Produce a translation matrix (identity with offset in the w-column)
constructMatrix :: MakeStack -> Float -> Mat44 Float
constructMatrix mkStack elapsedTime = peek $ mkStack elapsedTime

-- A list of functions which produce translation matrices
g_instanceList :: [MakeStack]
g_instanceList = [ stationaryOffset
                 , ovalOffset
                 , bottomCircleOffset ]

stationaryOffset :: MakeStack
stationaryOffset _ = push new [Translate Z $ -20]

ovalOffset :: MakeStack
ovalOffset elapsedTime = push new [ RotateRad Z t
                                  , TranslateAll $ (r):.(0):.(-20):.()
                                  , RotateRad Z $ -t
                                  ]
    where
        loopDuration = 3 -- sec
        sf = pi * 2 / loopDuration -- rad/sec
        currTimeThroughLoop = mod' elapsedTime loopDuration -- sec
        currRadsThroughLoop = currTimeThroughLoop * sf -- rad
        -- cartesian
        x = 4 * cos currRadsThroughLoop 
        y = 6 * sin currRadsThroughLoop 
        -- polar
        r = norm $ x:.y:.()
        t = atan2 y x

bottomCircleOffset :: MakeStack
bottomCircleOffset elapsedTime = push new [ Translate Z $ -20
                                          , RotateRad Y $ -currRadsThroughLoop 
                                          , TranslateAll $ (5):.(-3.5):.(0):.()
                                          , RotateRad Y currRadsThroughLoop 
                                          ]
    where
        loopDuration = 12 -- sec
        sf = pi * 2 / loopDuration -- rad/sec
        currTimeThroughLoop = mod' elapsedTime loopDuration -- sec
        currRadsThroughLoop = currTimeThroughLoop * sf -- rad

-- eof
