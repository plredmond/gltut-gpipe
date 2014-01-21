import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.UI.GLUT as GLUT

import Graphics.GPipe
import Data.Vec as V
import Prelude as P

import System.Environment (getArgs)

main :: IO ()
main = do
    a <- getArgs
    Framework.main keyboard (displayIO $ parseArgs a) (\_ -> return ())

-- This is not a good way to handle command-line arguments.
parseArgs :: [String] -> Vec2 Float
parseArgs argv = if P.length argv == 2
                 then fromList $ P.map read argv
                 else error $ "USAGE: prog offset-x offset-y"

keyboard :: Char -> GLUT.Position -> IO ()
keyboard '\ESC' _ = do GLUT.leaveMainLoop
keyboard _      _ = do return ()

displayIO :: Vec2 Float -> Vec2 Int -> IO (FrameBuffer RGBFormat () ())
displayIO offset size = do
    return $ display offset size

display :: Vec2 Float -> Vec2 Int -> FrameBuffer RGBFormat () ()
display offset size = draw fragments cleared
    where
        -- draw -- curry blending mode and boolean color mask onto paintColor
        draw :: FragmentStream (Color RGBFormat (Fragment Float))
                -> FrameBuffer RGBFormat () ()
                -> FrameBuffer RGBFormat () ()
        draw = paintColor NoBlending (RGB $ vec True)
        -- cleared -- a solid color framebuffer
        cleared :: FrameBuffer RGBFormat () ()
        cleared = newFrameBufferColor (RGB $ vec 0)
        -- fragment stream
        fragments :: FragmentStream (Color RGBFormat (Fragment Float))
        fragments = fmap fs
                  $ rasterizeBack
                  $ fmap (vs (toGPU $ append offset (vec 0))
                             (toGPU $ V.map fromIntegral size))
                  stream

stream :: PrimitiveStream Triangle (Vec4 (Vertex Float))
stream = toGPUStream TriangleList $
    [   0.75  :.   0.75  :.  0.00  :.  1.00  :.  ()
    ,   0.75  :. (-0.75) :.  0.00  :.  1.00  :.  ()
    , (-0.75) :. (-0.75) :.  0.00  :.  1.00  :.  ()
    ]

-- Contrary to the cpp tutorial, the window coordinates of a fragment aren't
-- provided to each invocation of a fragment shader in GPipe. To implement the
-- same behavior as the tutorial we manually convert the clip-space coordinates
-- to window coordinates in the vertex shader, and then pass the result out for
-- interpolation accross the generated fragments.

-- clip-space coordinates: [-1,1] [-1,1] [-1,1] 1
-- window coordinates: [0,w] [0,h] [0,1]
vs_clip2win :: Vec2 (Vertex Float) -> Vec4 (Vertex Float) -> Vec3 (Vertex Float)
vs_clip2win (w:.h:.()) pos@(_:._:._:.hom_w:.()) = (x*w):.(y*h):.z:.()
    where
        x:.y:.z:._:.() = V.map ((/2) . (+1) . (/hom_w)) pos

vs :: Vec4 (Vertex Float) -> Vec2 (Vertex Float) -> Vec4 (Vertex Float) -> (Vec4 (Vertex Float), Vec3 (Vertex Float))
vs offset size pos = let pos' = pos + offset in (pos', vs_clip2win size pos')

fs :: Vec3 (Fragment Float) -> Color RGBFormat (Fragment Float)
fs (_:.y:._:.()) = RGB outputColor
    where
        lerpValue = y / 500
        outputColor = vec $ mix 1 0.2 lerpValue

-- eof
