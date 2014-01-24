import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.GLTut.VecFile as VecFile
import qualified Graphics.UI.GLUT as GLUT
import qualified Paths_gltut_tut04 as Paths

import Graphics.GPipe
import Data.Vec as V
import Prelude as P

main :: IO ()
main = do
    dat <- Paths.getDataFileName "model.vec4" >>= readFile
    Framework.main keyboard (displayIO $ VecFile.readStream dat) initialize

initialize :: GLUT.Window -> IO ()
initialize w = GLUT.idleCallback GLUT.$= (Just . GLUT.postRedisplay . Just $ w)

keyboard :: Char -> GLUT.Position -> IO ()
keyboard '\ESC' _ = do GLUT.leaveMainLoop
keyboard _      _ = do return ()

displayIO :: PrimitiveStream Triangle (Vec4 (Vertex Float), Vec4 (Vertex Float)) -> Vec2 Int -> IO (FrameBuffer RGBFormat () ())
displayIO stream size = do
    return $ display stream size

display :: PrimitiveStream Triangle (Vec4 (Vertex Float), Vec4 (Vertex Float)) -> Vec2 Int -> FrameBuffer RGBFormat () ()
display stream _ = draw fragments cleared
    where
        draw = paintColor NoBlending (RGB $ vec True)
        cleared = newFrameBufferColor (RGB $ vec 0)
        fragments = fmap fs
                  $ rasterizeBack
                  $ fmap (vs offset)
                  stream
        -- constant uniforms, calculated once
        offset = toGPU (0.5:.0.25:.0:.0:.())

-- Offset the position. Don't perform any projection.
vs  :: Vec4 (Vertex Float)
    -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
    -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
vs offset (pos, col) = (offset + pos, col)

fs :: Vec4 (Fragment Float) -> Color RGBFormat (Fragment Float)
fs = RGB . V.take n3

-- eof
