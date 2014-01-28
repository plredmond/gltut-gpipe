module Graphics.GLTut.Easing where

-- Provide animation easing routines for tutorials 5, 6, and others.

import Graphics.GPipe (Real'(..))
import Data.Boolean (IfB(..), OrdB(..))

-- Compute a real [0, 1) which ranges linearly on 0..1 as elapsedTime ranges on 0..loopDuration
lerpThere :: (Real' a) => a -> a -> a
lerpThere elapsedTime loopDuration = (mod' elapsedTime loopDuration) / loopDuration

-- Compute a real [0, 1] which ranges linearly on 0..1..0 as elapsedTime ranges on 0..loopDuration
lerpThereAndBack :: (Real' a, OrdB bool a, IfB bool a) => a -> a -> a
lerpThereAndBack elapsedTime loopDuration = 2 * (ifB (f >* 0.5) (1 - f) f)
    where f = lerpThere elapsedTime loopDuration

-- Compute a real [0, 1) which eases heavily on 0..1 as elapsedTime ranges on 0..loopDuration
easeThere :: (Real' a) => a -> a -> a
easeThere elapsedTime loopDuration = (negate . cos . (pi*) $ f) / 2 + 0.5
    where f = lerpThere elapsedTime loopDuration

-- Compute a real [0, 1] which eases heavily on 0..1..0 as elapsedTime ranges on 0..loopDuration
easeThereAndBack :: (Real' a) => a -> a -> a
easeThereAndBack elapsedTime loopDuration = (negate . cos . (pi*2*) $ f) / 2 + 0.5
    where f = lerpThere elapsedTime loopDuration

-- Compute a real [0, 1] which eases heavily on 0.5..1..0..0.5 as elapsedTime ranges on 0..loopDuration
easeMiddUpDownUp :: (Real' a) => a -> a -> a
easeMiddUpDownUp elapsedTime loopDuration = (sin . (pi*2*) $ f) / 2 + 0.5
    where f = lerpThere elapsedTime loopDuration

-- Shift and scale a fraction [0, 1] onto the given range.
onRange :: (Real' a) => a -> (a, a) -> a
v `onRange` (mn, mx) = mix mn mx v
-- v `onRange` (mn, mx) = v * (mx - mn) + mn
-- mix x y a = x*(1-a)+y*a

-- eof
