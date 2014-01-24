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
                  $ fmap (vs offset frustrumScale zNear zFar)
                  stream
        -- constant uniforms, calculated once
        offset = toGPU (0.5:.0.5:.(-2):.0:.()) -- Minor deviation from tutorial: We offset the Z of the vertex data by -2 here instead of duplicating the data inside the code.
        frustrumScale = toGPU 1
        zNear = toGPU 1
        zFar = toGPU 3

-- Offset the position. Perform projection manually.
vs  :: Vec4 (Vertex Float)
    -> Vertex Float
    -> Vertex Float
    -> Vertex Float
    -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
    -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
vs offset frustrumScale zNear zFar (pos, col) = (clipPos, col)
    where
        camX:.camY:.camZ:._:.() = pos + offset
        pr1 = (zNear + zFar) / (zNear - zFar)
        pr2 = (2 * zNear * zFar) / (zNear - zFar)
        clipPos = (camX * frustrumScale) :.
                  (camY * frustrumScale) :.
                  (camZ * pr1 + pr2) :.
                  (camZ / (-1)) :. ()

fs :: Vec4 (Fragment Float) -> Color RGBFormat (Fragment Float)
fs = RGB . V.take n3

-- eof
