module AnimUtils where

import Data.Fixed (mod')

calcLerpFactor :: Float -> Float -> Float
calcLerpFactor elapsedTime loopDuration = 2 * zeroToHalf
    where
        loopFraction = mod' elapsedTime loopDuration / loopDuration
        zeroToHalf = if loopFraction > 0.5 then 1 - loopFraction else loopFraction
