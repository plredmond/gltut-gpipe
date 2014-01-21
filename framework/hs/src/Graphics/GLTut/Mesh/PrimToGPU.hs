{-# LANGUAGE FlexibleContexts #-}

module Graphics.GLTut.Mesh.PrimToGPU
( Attr()
, attr
, attrStride
, attrLength
, attrData
, attrSlice
, primToGPU
) where

-- hackage
import qualified Data.Vec as Vec
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Graphics.GPipe (toGPUStream, toIndexedGPUStream, CPU, VertexInput, Primitive, Vertex, PrimitiveStream)
import Data.List (transpose)
import Text.Printf (printf)
import Data.List.Split (chunksOf)

-- local
import Graphics.GLTut.Mesh.Util

--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
data Attr = Attribute Int [Float] deriving (Show, Eq)

-- Create an Attr. Invariant: stride divides data evenly into chunks.
attr :: Int -> [Float] -> Either String Attr
attr stride xs | length xs `mod` stride == 0 = Right $ Attribute stride xs
               | otherwise = Left $ printf "Stride %d doesn't divide attr data evenly" stride

-- Extract the size of chunks in an Attr.
attrStride :: Attr -> Int
attrStride (Attribute stride _) = stride

-- Calculate the number of chunks in an Attr.
attrLength :: Attr -> Int
attrLength (Attribute stride xs) = length xs `div` stride

-- Extract all the chunks from an Attr.
attrData :: Attr -> [[Float]]
attrData (Attribute stride xs) = chunksOf stride xs

-- Make a new Attr from a slice of this Attr's chunks.
-- To support arrays better, map attrSlice over (IntMap Attr) before primToGPU.
attrSlice :: Int -> Int -> Attr -> Attr
attrSlice start count a@(Attribute stride _) =
    -- Check the invariant. It should always hold unless a bug is introduced.
    either error id $ attr stride (concat . take count . drop start $ attrData a)

--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
type VFloat = Vertex Float

-- Convert primitive data to a PrimitiveStream.
-- To load a set of different set of attributes, filterWithKey (IntMap Attr) before primToGPU.
primToGPU :: (Primitive p) => p -> [Int] -> IntMap Attr
    -> Either String (PrimitiveStream p (IntMap [VFloat]))
primToGPU primcmd indexes attrm = do
    guard (length attrs > 0)
          "No attributes given"
    guard (let n = attrLength . head $ attrs in all ((n ==) . attrLength) attrs)
          "Attributes must all have the same length"
    return (mkMap attrindexes strides . mkStream primcmd indexes . mkInput $ attrs)
    where
        (attrindexes, attrs) = unzip . IntMap.toList $ attrm
        strides = map attrStride attrs

-- Transpose a list of attributes to a list of flat arg-lists.
mkInput :: [Attr] -> [[Float]]
mkInput = map concat . transpose . map attrData

-- Convert a list of flat arg-lists to a PrimitiveStream.
-- All the arg-lists should be of the same length on the interval [1, 19].
mkStream :: (Primitive p) => p -> [Int] -> [[Float]] -> PrimitiveStream p [VFloat]
mkStream primcmd indexes argvs = mk primcmd indexes argvs
    where
        mk = case length . head $ argvs of
         -- 0 -> mkStream' Vec.n0 -- Type information isn't preserved by the empty vec.
            1 -> mkStream' Vec.n1
            2 -> mkStream' Vec.n2
            3 -> mkStream' Vec.n3
            4 -> mkStream' Vec.n4
            5 -> mkStream' Vec.n5
            6 -> mkStream' Vec.n6
            7 -> mkStream' Vec.n7
            8 -> mkStream' Vec.n8
            9 -> mkStream' Vec.n9
            10 -> mkStream' Vec.n10
            11 -> mkStream' Vec.n11
            12 -> mkStream' Vec.n12
            13 -> mkStream' Vec.n13
            14 -> mkStream' Vec.n14
            15 -> mkStream' Vec.n15
            16 -> mkStream' Vec.n16
            17 -> mkStream' Vec.n17
            18 -> mkStream' Vec.n18
            19 -> mkStream' Vec.n19
         -- 20 -> mkStream' Vec.n20 -- To send more than 19 Floats to the GPU with this scheme, we must extend the type-level naturals in Data.Vec. It's probably better to develop a scheme which doesn't rely on a flat vector.

mkStream' :: ( Vec.VecList Float (CPU v) -- fromList
             , Primitive p               -- toStream
             , VertexInput v             -- toStream
             , Vec.Vec n VFloat v        -- mkVec (in withlength)
             , Vec.Fold v VFloat         -- toList
             ) => n -> p -> [Int] -> [[Float]] -> PrimitiveStream p [VFloat]
mkStream' n primcmd indexes = fmap (Vec.toList . withLength n)
                            . toStream indexes primcmd
                            . map Vec.fromList

-- Make an indexed draw call based on the presence of indexes.
toStream :: (Primitive p, VertexInput v) => [Int] -> p -> [CPU v] -> PrimitiveStream p v
toStream []      = toGPUStream
toStream indexes = \p vs -> toIndexedGPUStream p vs indexes

-- Force an ambiguously typed vector to a certain length.
-- Copied from https://github.com/tobbebex/GPipe-Collada/blob/master/src/Graphics/GPipe/Collada/Parse.hs#L525
--             at commit 2a7d4887b30fe3cf5f3e7f83787e06b6ec2964db
withLength :: (Vec.Vec n VFloat v) => n -> v -> v
withLength n v = v `asTypeOf` Vec.mkVec n (undefined :: VFloat)

-- Split up the parts a PrimitiveStream into maps using the given keys.
-- NOTE: The keys and chunk-sizes probably get compiled into the shader.
--       Changing them frequently will cause problems.
mkMap :: [IntMap.Key] -> [Int] -> PrimitiveStream p [VFloat] -> PrimitiveStream p (IntMap [VFloat])
mkMap keys sizes = fmap (IntMap.fromList . zip keys . chunkBy sizes)

-- eof
