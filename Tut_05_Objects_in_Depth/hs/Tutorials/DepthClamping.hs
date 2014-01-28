import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.GLTut.Perspective as Perspective
import qualified Graphics.GLTut.Easing as Easing
import qualified Graphics.GLTut.Tut05.Models as Models
import qualified Graphics.UI.GLUT as GLUT

import Data.Monoid (mconcat)

import Graphics.GPipe
import Data.Vec as V
import Prelude as P

main :: IO ()
main = do
    horiz <- Models.load_horizwedge
    verti <- Models.load_vertiwedge
    let objs = zip [horiz, verti]
                   [toGPU $ 0:.0:.0.5:.0:.(), toGPU $ 0:.0:.(-1):.0:.()]
    -- enter common mainloop
    Framework.main keyboard
                   (displayIO objs)
                   initialize

-- Set up the window.
initialize :: GLUT.Window -> IO ()
initialize w = do GLUT.idleCallback GLUT.$= (Just . GLUT.postRedisplay . Just $ w)
                  GLUT.depthClamp GLUT.$= GLUT.Enabled

-- Handle keyboard events.
keyboard :: Char -> GLUT.Position -> IO ()
keyboard '\ESC' _ = do GLUT.leaveMainLoop
keyboard ' '    _ = do x <- GLUT.get GLUT.depthClamp
                       GLUT.depthClamp GLUT.$= if x == GLUT.Disabled then GLUT.Enabled else GLUT.Disabled
keyboard _      _ = do return ()

-- Perform IO on behalf of display. Call display to produce the framebuffer.
displayIO :: [(Models.PrimStream, Vec4 (Vertex Float))] -> Vec2 Int -> IO (FrameBuffer RGBFormat DepthFormat ())
displayIO objects size = do
    milliseconds <- GLUT.get GLUT.elapsedTime
    return $ display objects size (fromIntegral milliseconds / 1000)

-- Combine scene elements on a framebuffer.
display :: [(Models.PrimStream, Vec4 (Vertex Float))] -> Vec2 Int -> Float -> FrameBuffer RGBFormat DepthFormat ()
display objects size sec = draw fragments cleared
    where
        draw = paintColorRastDepth Less True NoBlending (RGB $ vec True)
        cleared = newFrameBufferColorDepth (RGB $ vec 0) 1
        fragments = mconcat
                  . P.map (mkFragments cam2clip)
                  $ objects'
        -- Deviation from tutorial: We vary the z of the first object's offset to demonstrate the problem with depth.
        objects' = let (stream, offset        ):rest = objects
                    in (stream, offset + tweak):rest
        -- variable uniforms, calculated every frame
        tweak :: Vec4 (Vertex Float)
        tweak = let z = Easing.lerpThereAndBack sec 5 `Easing.onRange` (-1.5, 0)
                 in toGPU (0:.0:.z:.0:.())
        cam2clip = toGPU $ Perspective.m_ar 1 1 3 (V.map fromIntegral size)

-- Convert a scene element to a FragmentStream.
mkFragments :: Mat44 (Vertex Float)
            -> (Models.PrimStream, Vec4 (Vertex Float))
            -> FragmentStream (Color RGBFormat (Fragment Float))
mkFragments matrix (stream, offset) = fmap fs
                                    $ rasterizeBack
                                    $ fmap (vs offset matrix)
                                    stream

-- Offset the position. Perform projection using the provided matrix.
vs  :: Vec4 (Vertex Float)
    -> Mat44 (Vertex Float)
    -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
    -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
vs offset mat (pos, col) = (clipPos, col)
    where
        cameraPos = pos + offset
        clipPos = multmv mat cameraPos

-- Use the provided color.
fs :: Vec4 (Fragment Float) -> Color RGBFormat (Fragment Float)
fs = RGB . V.take n3

-- eof
