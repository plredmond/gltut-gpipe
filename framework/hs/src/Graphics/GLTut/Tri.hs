module Graphics.GLTut.Tri where

-- (Tri a b c) is like (Either a b) except it has three type parameters. ------

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

-- eof
