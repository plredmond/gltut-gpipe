module Graphics.GLTut.Tut07.Angle
( Angle()
, fromRad, toRad
, fromDeg, toDeg
) where

import Data.Fixed (mod')

-- Datatype -- A normalized angle datatype stored in radians.
newtype Angle a = Angle { toRad :: a }
        deriving (Show, Eq, Ord)

-- Accessors
toDeg :: Floating a => Angle a -> a
toDeg (Angle n) = n * (360 / tau)

-- Constructors -- Perform normalization.
fromRad :: (Real a, Floating a) => a -> Angle a
fromRad = Angle . (`mod'` tau)

fromDeg :: (Real a, Floating a) => a -> Angle a
fromDeg = fromRad . ((tau / 360) *)

-- Helpers
tau :: Floating a => a
tau = 2 * pi

-- eof
