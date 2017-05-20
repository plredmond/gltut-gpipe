module Graphics.GLTut.Tut07.SphereCoord where

-- module imports

import qualified Graphics.GLTut.Tut07.Angle as Angle

import Graphics.GLTut.Tut07.Angle (Angle)

-- test imports

import Control.Monad (forM_, when)
import Text.Printf (printf)

tau :: Floating a => a
tau = 2 * pi

-- Datatype
data SphereCoord a = SphereCoord
    { radius      :: a -- [0, inf) distance from center
    , azimuth     :: a -- [0, 2pi) around from forward-dir
    , inclination :: a -- [0, pi] down from up-dir
    } deriving (Show, Eq, Ord)
    -- There is no Read instance because data should be constructed only using the smart constructor below.
    -- The Eq and Ord instances don't work for edge cases where coordinates are ambiguous; see 'unique' below.
    -- It might be valuable at some point to define a newtype wrapper via 'unique' with proper Eq and Ord instances.

-- Construct with in-range values.
sphereCoord :: (Real a, Floating a) => a -> Angle a -> Angle a -> SphereCoord a
sphereCoord r aA iA | r < 0     = sphereCoord (-r) (rad $ a + pi) (rad $ i + pi)
--                  | a < 0     = -- Angle is normalized.
--                  | a >= tau  = -- Angle is normalized.
--                  | i < 0     = -- Angle is normalized.
                    | i > pi    = SphereCoord r a (tau - i) -- it might be more maintainable to recurse here
                    | otherwise = SphereCoord r a i
    where
        a = Angle.toRad aA
        i = Angle.toRad iA
        rad = Angle.fromRad

-- Make the coordinates unique.
unique :: (Eq a, Floating a) => SphereCoord a -> SphereCoord a
unique sc@(SphereCoord r _ i)
    | r == 0            = SphereCoord r 0 0 -- when radius is zero, azimuth and inclination are arbitrary -- it might be more maintainable to use the smart constructor here (or even just use a constant SphereCoords 0 0 0)
    | i == 0 || i == pi = SphereCoord r 0 i -- when inclination is zero or pi, azimuth is arbitrary       -- it might be more maintainable to use the smart constructor here
    | otherwise         = sc                -- otherwise, the coordinates are already unique

-- Conversion to cartesian coordinates.
--          zenith is z because (toCartesian $ SphereCoord 1      0      0) is approximately (0, 0, 1)
--  prime meridian is x because (toCartesian $ SphereCoord 1      0 (pi/2)) is approximately (1, 0, 0)
-- 90 deg meridian is y because (toCartesian $ SphereCoord 1 (pi/2) (pi/2)) is approximately (0, 1, 0)
-- 90 deg meridian will be measured east of prime meridian if system is treated as right-handed; west if left-handed
-- opengl uses a right-handed system
toCartesian :: Floating a => SphereCoord a -> (a, a, a)
toCartesian (SphereCoord r a i) = (x, y, z)
    where
        x = r * sin i * cos a
        y = r * sin i * sin a
        z = r * cos i

-- Tests
test_sphereCoord :: IO ()
test_sphereCoord = do
    let deg = Angle.fromDeg :: Double -> Angle Double
    let ts = [ ( 1, deg  45, deg  45) -- (front, right,   up) in range
             , (-1, deg  45, deg  45) -- ( back,  left, down) negated radius
             , ( 1, deg 135, deg 135) -- ( back, right, down) in range
             , (-1, deg 135, deg 135) -- (front,  left,   up) negated radius
             , ( 1, deg 225, deg  45) -- ( back,  left,   up) in range
             , (-1, deg 225, deg  45) -- (front, right, down) negated radius
             -- these are all in range, just not unique
             , ( 1, deg  45, deg   0) -- (front, right,   up) ambiguous a (straight up)
             , ( 1, deg  45, deg 180) -- (front, right, down) ambiguous a (straight down)
             , ( 0, deg  45, deg  45) -- (front, right,   up) ambiguous a,i (center)
             , ( 0, deg  45, deg   0) -- (front, right,   up) ambiguous a,i (center)
             , ( 0, deg  45, deg 180) -- (front, right, down) ambiguous a,i (center)
             ]
    let es = [ ( 1, deg  45, deg  45) -- (front, right,   up) in range
             , ( 1, deg 225, deg 135) -- ( back,  left, down) negated radius
             , ( 1, deg 135, deg 135) -- ( back, right, down) in range
             , ( 1, deg 315, deg  45) -- (front,  left,   up) negated radius
             , ( 1, deg 225, deg  45) -- ( back,  left,   up) in range
             , ( 1, deg  45, deg 135) -- (front, right, down) negated radius
             -- these are all in range, just not unique
             , ( 1, deg  45, deg   0) -- (front, right,   up) ambiguous a (straight up)
             , ( 1, deg  45, deg 180) -- (front, right, down) ambiguous a (straight down)
             , ( 0, deg  45, deg  45) -- (front, right,   up) ambiguous a,i (center)
             , ( 0, deg  45, deg   0) -- (front, right,   up) ambiguous a,i (center)
             , ( 0, deg  45, deg 180) -- (front, right, down) ambiguous a,i (center)
             ]
    forM_ (zip ts es) $ \(test@(test_r, test_a, test_i), expd) -> do
        let actual = sphereCoord test_r test_a test_i
            actl_r = radius actual
            actl_a = Angle.fromRad $ azimuth actual
            actl_i = Angle.fromRad $ inclination actual
            actl = (actl_r, actl_a, actl_i)
        when (expd /= actl) $ do
            if test == expd then do printf "Passthru test: %s\n" (show test)
                            else do printf "Test______: %s\n  Expected: %s\n" (show test) (show expd)
            printf "  Output__: %s\n" (show actl)

test_unique :: IO ()
test_unique = do
    let deg = Angle.fromDeg :: Double -> Angle Double
    let ts = [ sphereCoord 1 (deg  45) (deg  45) -- (front, right,   up) in range
             -- these are all in range, just not unique
             , sphereCoord 1 (deg  45) (deg   0) -- (front, right,   up) ambiguous a (straight up)
             , sphereCoord 1 (deg  45) (deg 180) -- (front, right, down) ambiguous a (straight down)
             , sphereCoord 0 (deg  45) (deg  45) -- (front, right,   up) ambiguous a,i (center)
             , sphereCoord 0 (deg  45) (deg   0) -- (front, right,   up) ambiguous a,i (center)
             , sphereCoord 0 (deg  45) (deg 180) -- (front, right, down) ambiguous a,i (center)
             ]
    let es = [ sphereCoord 1 (deg  45) (deg  45) -- (front, right,   up) in range
             -- these are all in range, just not unique
             , sphereCoord 1 (deg   0) (deg   0) -- (front, right,   up) ambiguous a (straight up)
             , sphereCoord 1 (deg   0) (deg 180) -- (front, right, down) ambiguous a (straight down)
             , sphereCoord 0 (deg   0) (deg   0) -- (front, right,   up) ambiguous a,i (center)
             , sphereCoord 0 (deg   0) (deg   0) -- (front, right,   up) ambiguous a,i (center)
             , sphereCoord 0 (deg   0) (deg   0) -- (front, right, down) ambiguous a,i (center)
             ]
    forM_ (zip ts es) $ \(test, expd) -> do
        let actl = unique test
        when (expd /= actl) $ do
            if test == expd then do printf "Passthru test: %s\n" (show test)
                            else do printf "Test______: %s\n  Expected: %s\n" (show test) (show expd)
            printf "  Output__: %s\n" (show actl)

-- eof
