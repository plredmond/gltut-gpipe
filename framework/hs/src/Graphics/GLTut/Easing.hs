module Graphics.GLTut.Easing where

-- Provide animation easing routines for tutorials 5, 6, and others.

import Graphics.GPipe
import Data.Vec

-- A value ranging up and down over [0, 1] during the duration.
computeCycle :: Float -> Float -> Float
computeCycle dur sec = 0.5 * (1 + (sin $ curTime * scale))
    where
        scale = pi * 2 / dur
        curTime = mod' sec dur

-- eof
