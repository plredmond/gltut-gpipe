import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.GLTut.Perspective as Perspective
import qualified Graphics.GLTut.VecFile as VecFile
import qualified Graphics.GLTut.Easing as Easing
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
    milliseconds <- GLUT.get GLUT.elapsedTime
    return $ display stream size (fromIntegral milliseconds / 1000)

display :: PrimitiveStream Triangle (Vec4 (Vertex Float), Vec4 (Vertex Float)) -> Vec2 Int -> Float -> FrameBuffer RGBFormat () ()
display stream size sec = draw fragments cleared
    where
        draw = paintColor NoBlending (RGB $ vec True)
        cleared = newFrameBufferColor (RGB $ vec 0)
        fragments = fmap fs
                  $ rasterizeBack
                  $ fmap (vs offset matrix)
                  stream
        -- constant uniforms, calculated once
        offset = toGPU (0.5:.0.5:.(-2):.0:.()) -- Minor deviation from tutorial: We offset the Z of the vertex data by -2 here instead of duplicating the data inside the code.
        -- variable uniforms, calculated every frame
        matrix = toGPU $ Perspective.m_ar 1 (0.5 + Easing.computeCycle 5 sec) 3 (V.map fromIntegral size)

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
