module Graphics.GLTut.RenderState
( RenderState()
, new
, getSize
, getSeconds
, getAspectRatio
) where

-- Provide basic environmental information for tutorial 6 and others.

import qualified Graphics.UI.GLUT as GLUT
import Data.Vec as Vec

data RenderState a = RenderState
    { getSize :: Vec2 a
    , getSeconds :: a
    } deriving (Show, Eq)

-- Construct a RenderState
-- IO: Read GLUT.elapsedTime
new :: Fractional a => Vec2 Int -> IO (RenderState a)
new size = do
    milliseconds <- GLUT.get GLUT.elapsedTime
    return RenderState
        { getSize = Vec.map fromIntegral size
        , getSeconds = fromIntegral milliseconds / 1000
        }

getAspectRatio :: Fractional a => RenderState a -> a
getAspectRatio rs = let w:.h:.() = getSize rs
                    in  w / h

-- eof
