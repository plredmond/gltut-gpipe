import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.GLTut.Perspective as Perspective
import qualified Graphics.GLTut.VecFile as VecFile
import qualified Graphics.GLTut.Easing as Easing
import qualified Graphics.UI.GLUT as GLUT
import qualified Paths_gltut_tut05 as Paths

import Data.Monoid (mconcat)

import Graphics.GPipe
import Data.Vec as V
import Prelude as P

type Object = ( PrimitiveStream Triangle (Vec4 (Vertex Float), Vec4 (Vertex Float))
              , Vec4 (Vertex Float)
              )

main :: IO ()
main = do
    dat_h <- Paths.getDataFileName "obj-horiz.vec4" >>= readFile
    dat_v <- Paths.getDataFileName "obj-vert.vec4"  >>= readFile
    Framework.main keyboard
                   (displayIO [ (VecFile.readIndexedStream dat_h indexes, toGPU (0:.0:.0.5:.0:.()))
                              , (VecFile.readIndexedStream dat_v indexes, toGPU (0:.0:.(-1):.0:.()))
                              ])
                   initialize
    where
        indexes = [ 0, 2, 1
                  , 3, 2, 0
                  , 4, 5, 6
                  , 6, 7, 4
                  , 8, 9, 10
                  , 11, 13, 12
                  , 14, 16, 15
                  , 17, 16, 14 ]

initialize :: GLUT.Window -> IO ()
initialize w = GLUT.idleCallback GLUT.$= (Just . GLUT.postRedisplay . Just $ w)

keyboard :: Char -> GLUT.Position -> IO ()
keyboard '\ESC' _ = do GLUT.leaveMainLoop
keyboard _      _ = do return ()

displayIO :: [Object] -> Vec2 Int -> IO (FrameBuffer RGBFormat DepthFormat ())
displayIO objects size = do
    milliseconds <- GLUT.get GLUT.elapsedTime
    return $ display objects size (fromIntegral milliseconds / 1000)

display :: [Object] -> Vec2 Int -> Float -> FrameBuffer RGBFormat DepthFormat ()
display objects size sec = draw fragments cleared
    where
        draw = paintColorRastDepth Less True NoBlending (RGB $ vec True)
        cleared = newFrameBufferColorDepth (RGB $ vec 0) 1
        fragments = mconcat
                  . P.map (mkFragments matrix)
                  $ objects'
        -- Deviation from tutorial: We vary the z of the first object's offset to demonstrate the problem with depth.
        objects' = let (stream, offset        ):rest = objects
                    in (stream, offset + tweak):rest
        -- constant uniforms, calculated once
        -- variable uniforms, calculated every frame
        tweak :: Vec4 (Vertex Float)
        tweak = let z = (-1.3) * Easing.computeCycle 5 sec
                 in toGPU (0:.0:.z:.0:.())
        matrix = toGPU $ Perspective.m_ar 1 1 3 (V.map fromIntegral size)

mkFragments :: Mat44 (Vertex Float) -> Object -> FragmentStream (Color RGBFormat (Fragment Float))
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

fs :: Vec4 (Fragment Float) -> Color RGBFormat (Fragment Float)
fs = RGB . V.take n3

-- eof
