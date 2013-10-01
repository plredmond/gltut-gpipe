module Util where

import qualified Graphics.UI.GLUT as GLUT
import Data.Vec as Vec
import Graphics.GPipe

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

degToRad :: Float -> Float
degToRad = (deg *)
    where
        tau = pi * 2
        deg = tau / 360

-- Output a value [0, 1) where n traces a cycle each max through  0..1..0..-1..repeat
cycle f max n = f $ full * curr
    where
        full = pi * 2 / max -- full is a full cycle scaled to max
        curr = mod' n max   -- curr is in [0, max)

toNDC cam = clip / (vec w)
    where
        m = perspective 1 3 (degToRad 45) 1
        clip = multmv m cam
        (_:._:._:.w:.()) = clip

-- eof
