module RenderState where

import qualified Graphics.UI.GLUT as GLUT
import Data.Vec as Vec

data RenderState a = RenderState
    { rsSize :: Vec2 a
    , rsSeconds :: a
    } deriving (Show, Eq)

-- Construct a RenderState
-- IO: Read GLUT.elapsedTime
makeRenderState :: Fractional a => Vec2 Int -> IO (RenderState a)
makeRenderState size = do
    ms <- GLUT.get GLUT.elapsedTime
    return RenderState
        { rsSize = Vec.map fromIntegral size
        , rsSeconds = fromIntegral ms / 1000 }

rsAspectRatio :: Fractional a => RenderState a -> a
rsAspectRatio rs = let w:.h:.() = rsSize rs
                   in  w / h
