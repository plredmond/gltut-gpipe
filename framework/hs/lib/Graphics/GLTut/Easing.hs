module Graphics.GLTut.Easing where
-- Provide animation easing routines for tutorials 5, 6, and others.
--
-- TODO: Reimplement these using native GLSL step, smoothstep, clamp, saturate,
-- min, max, etc...

import Graphics.GPipe
-- import Data.Boolean (IfB(..), OrdB(..))

-- | Compute a real [0, 1) which ranges linearly on 0..1 as elapsedTime ranges
-- on 0..loopDuration.
--
-- >>> lerpUp 0 10
-- 0.0
-- >>> lerpUp 5 10
-- 0.5
-- >>> lerpUp 9.9 10
-- 0.99
-- >>> lerpUp 10 10
-- 0.0
--
-- >>> lerpUp 0 0
-- NaN
lerpUp :: (Real' a) => a -> a -> a
lerpUp elapsedTime loopDuration = (mod'' elapsedTime loopDuration) / loopDuration

-- | Compute a real [0, 1] which ranges linearly on 0..1..0 as elapsedTime
-- ranges on 0..loopDuration.
--
-- >>> lerpUpDn 0 10
-- 0.0
-- >>> lerpUpDn 2.5 10
-- 0.5
-- >>> lerpUpDn 5 10
-- 1.0
-- >>> lerpUpDn 7.5 10
-- 0.5
-- >>> lerpUpDn 10 10
-- 0.0
--
-- >>> lerpUpDn 0 0
-- NaN
--
-- TODO: Implement with /ifThenElse'/ instead of /ifB/?
lerpUpDn :: (Real' a, OrdB a, IfB a) => a -> a -> a
lerpUpDn elapsedTime loopDuration = 2 * (ifB (f >* 0.5) (1 - f) f)
    where f = lerpUp elapsedTime loopDuration

-- | Compute a real [0, 1) which eases heavily on 0..1 as elapsedTime ranges on
-- 0..loopDuration.
--
-- >>> easeUp 0 10
-- 0.0
-- >>> easeUp 5 10 :: Float
-- 0.5
-- >>> easeUp 5 10 < easeUp 9.9 10
-- True
-- >>> easeUp 10 10
-- 0.0
--
-- >>> easeUp 0 0
-- NaN
easeUp :: (Real' a) => a -> a -> a
easeUp elapsedTime loopDuration = - cos (f * pi) * 0.5 + 0.5
    where f = lerpUp elapsedTime loopDuration

-- | Compute a real [0, 1] which eases heavily on 0..1..0 as elapsedTime ranges
-- on 0..loopDuration.
--
-- >>> easeUpDn 0 10
-- 0.0
-- >>> easeUpDn 2.5 10 :: Float
-- 0.5
-- >>> lerpUpDn 4 10 < easeUpDn 4 10
-- True
-- >>> easeUpDn 5 10
-- 1.0
-- >>> easeUpDn 7.5 10 :: Float
-- 0.5
-- >>> easeUpDn 10 10
-- 0.0
--
-- >>> easeUpDn 0 0
-- NaN
easeUpDn :: (Real' a) => a -> a -> a
easeUpDn elapsedTime loopDuration = - cos (elapsedTime * 2 * pi / loopDuration) * 0.5 + 0.5

-- Compute a real [0, 1] which eases heavily on 0.5..1..0..0.5 as elapsedTime
-- ranges on 0..loopDuration.
--
-- >>> easeMidUpDnUp 0 10
-- 0.5
-- >>> easeMidUpDnUp 2.5 10 :: Float
-- 1.0
-- >>> easeMidUpDnUp 5 10
-- 0.5
-- >>> easeMidUpDnUp 7.5 10 :: Float
-- 0.0
-- >>> easeMidUpDnUp 10 10
-- 0.5
--
-- >>> easeMidUpDnUp 0 0
-- NaN
easeMidUpDnUp :: (Real' a) => a -> a -> a
easeMidUpDnUp elapsedTime loopDuration = sin (elapsedTime * 2 * pi / loopDuration) * 0.5 + 0.5

-- Shift and scale a fraction [0, 1] onto the given range.
--
-- >>> 0 onRange 10 50
-- 10
-- >>> 0.5 onRange 10 50
-- 30
-- >>> 1 onRange 10 50
-- 50
onRange :: (Real' a) => a -> (a, a) -> a
v `onRange` (mn, mx) = mix mn mx v
-- v `onRange` (mn, mx) = v * (mx - mn) + mn
-- mix x y a = x*(1-a)+y*a
