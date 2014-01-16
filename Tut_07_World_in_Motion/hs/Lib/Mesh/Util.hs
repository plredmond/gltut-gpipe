module Lib.Mesh.Util where

-- hackage
import Text.Printf (printf)
import Text.Read (readEither)
import Data.Monoid (Monoid(), mconcat)
import Data.Maybe (catMaybes)
import qualified Data.IntMap.Lazy as IM
import qualified Data.HashMap.Lazy as HM
import qualified Data.Hashable as H

-- General datatypes ---------------------------------------------------------

data Tri a b c = Jyan a
               | Ken b
               | Po c
               deriving (Show, Read, Eq, Ord)

-- Case analysis for Tri.
tri :: (a -> q) -> (b -> q) -> (c -> q) -> Tri a b c -> q
tri f _ _ (Jyan x) = f x
tri _ g _ (Ken y) = g y
tri _ _ h (Po z) = h z

-- Extract all the Jyan, Ken, or Po elements from a list of Tri.
jyans :: [Tri a b c] -> [a]
kens  :: [Tri a b c] -> [b]
pos   :: [Tri a b c] -> [c]
jyans xs = [a | Jyan a <- xs]
kens  xs = [a | Ken  a <- xs]
pos   xs = [a | Po   a <- xs]

-- Parition a list of Tri into three lists.
partitionTris :: [Tri a b c] -> ([a], [b], [c])
partitionTris = foldr (tri jyan ken po) ([], [], [])
    where
        jyan a ~(j, k, p) = (a:j, k, p)
        ken  a ~(j, k, p) = (j, a:k, p)
        po   a ~(j, k, p) = (j, k, a:p)

-- triEither :: Tri (Either a x) (Either a y) (Either a z) -> Either a (Tri x y z)
-- triEither = tri (fmap Jyan) (fmap Ken) (fmap Po)

-- General functions ---------------------------------------------------------

lookupEitherIM :: IM.IntMap b -> a -> IM.Key -> Either a b
lookupEitherIM map err key = m2e err (IM.lookup key map)

lookupEitherHM :: (Eq k, H.Hashable k) => HM.HashMap k b -> a -> k -> Either a b
lookupEitherHM map err key = m2e err (HM.lookup key map)

-- Condense a list of monadic Tri or smth..
mconcatTris :: (Monoid a, Monoid b, Monoid c) => [Tri a b c] -> [Tri a b c]
mconcatTris ts = let (as, bs, cs) = partitionTris ts
                 in [Jyan $ mconcat as, Ken $ mconcat bs, Po $ mconcat cs]

-- -- Mconcat all the Just values from a list of Maybes which contain monoids.
-- mconcatMaybes :: Monoid m => [Maybe m] -> m
-- mconcatMaybes = mconcat . catMaybes

-- Split a list into chunks sized by the first argument.
chunkBy :: [Int] -> [a] -> [[a]]
chunkBy (size:sizes) xs' = let (x, xs) = splitAt size xs'
                           in x : chunkBy sizes xs

-- Like Control.Monad.guard, but allows for error information.
guard :: Bool -> a -> Either a ()
guard True  err = return ()
guard False err = Left err

-- Right if the value is within bounds. Left if it is not.
constrainBounds :: (Show b, Ord b, Bounded b) => b -> Either String b
constrainBounds val
    | val < minBound = Left $ printf "Value \"%s\" underwhelms minBound" (show val)
    | val > maxBound = Left $ printf "Value \"%s\" exceeds maxBound" (show val)
    | otherwise      = Right val

-- Convert a Maybe to an Either using the given error data.
m2e :: a -> Maybe b -> Either a b
m2e _ (Just val) = Right val
m2e err Nothing = Left err

-- Read multiple values into an either containing a list.
readManyEither :: (Read b) => [String] -> Either String [b]
readManyEither = mapM readEither

-- Modify a left, passthru a right.
modLeft :: (a -> a') -> Either a b -> Either a' b
modLeft f = either (Left . f) Right

-- eof
