{-# LANGUAGE MultiWayIf #-}
module Lib.Mesh.Util where

-- hackage
import Text.Printf (printf)
import Text.Read (readEither)

-- General functions ---------------------------------------------------------

-- Right if the value is within bounds. Left if it is not.
constrainBounds :: (Show b, Ord b, Bounded b) => b -> Either String b
constrainBounds v = if | v < minBound -> Left $ printf "Value \"%s\" underwhelms minBound" (show v)
                       | v > maxBound -> Left $ printf "Value \"%s\" exceeds maxBound" (show v)
                       | otherwise    -> Right v

-- Convert a Maybe to an Either using the given error string.
m2e :: String -> Maybe a -> Either String a
m2e _ (Just v) = Right v
m2e s Nothing = Left s

-- Read multiple values into an either containing a list.
readManyEither :: (Read b) => [String] -> Either String [b]
readManyEither = mapM readEither

-- Modify a left, passthru a right.
modLeft :: (a -> a') -> Either a b -> Either a' b
modLeft f = either (Left . f) Right

-- eof
