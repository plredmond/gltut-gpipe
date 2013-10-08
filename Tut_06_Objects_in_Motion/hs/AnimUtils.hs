module AnimUtils where

import Data.Fixed (mod')

-- Compute a lerp value [0, 1) which ranges from 0 to 1 to 0 every
-- loopDuration seconds of elapsedTime.
calcLerpFactor :: Float -> Float -> Float
calcLerpFactor elapsedTime loopDuration = 2 * zeroToHalf
    where
        loopFraction = mod' elapsedTime loopDuration / loopDuration
        zeroToHalf = if loopFraction > 0.5 then 1 - loopFraction else loopFraction
