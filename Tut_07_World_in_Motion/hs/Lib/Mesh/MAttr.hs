module Lib.Mesh.MAttr where

-- hackage
import Foreign.C.Types
import Text.XML.Light
import Text.Read (readEither)
import Text.Printf (printf)
import Data.List.Split (chunksOf)
import Data.Vec (VecList, fromList)

-- local
import Lib.Mesh.XML
import Lib.Mesh.Util

-- Attributes contain raw data arrays of short vectors.
-- All mesh attributes must contain the same count of vectors.

-- MAttr ---------------------------------------------------------------------

data MAttr = MAttr { maIndex :: MAttrIndex
                   , maSize :: MAttrSize
                   , maType :: MAttrType
                   , maData :: MAttrData
                   } deriving (Show, Read, Eq)

mkMAttr :: Element -> Either String MAttr
mkMAttr e = do
    ai <- extractAttr "index" readMAttrIndex e
    as <- extractAttr "size" readMAttrSize e
    at <- extractAttr "type" readMAttrType e
    ad <- readMAttrData as at $ childText e
    return $ MAttr ai as at ad

lengthMAttr :: MAttr -> Int
lengthMAttr a = (lengthMAttrData.maData $ a) `div` (fromMAttrSize.maSize $ a)

-- FIXME: fromRational may reduce precision?
extractMAttr :: ( Fractional num  -- numeric sub-element from attribute
                , VecList num arg -- [num] to a Vec of same
                ) => MAttr -> [arg]
extractMAttr (MAttr _ s t d) =
    map fromList                              -- Convert lists to Vecs of implicit length.
    . chunksOf (fromMAttrSize s)              -- Split into lists of size s.
    . map (fromRational . extractMAttrType t) -- Apply normalization, if applicable to t.
    . extractMAttrData                        -- Extract a list of rationals from d.
    $ d

-- MAttrIndex --

data MAttrIndex = MAttrIndex CUChar
                  deriving (Show, Read, Eq, Ord)

instance Bounded MAttrIndex where
    minBound = MAttrIndex 0
    maxBound = MAttrIndex 15

readMAttrIndex :: String -> Either String MAttrIndex
readMAttrIndex s = do
    v <- modLeft f . readEither $ s
    ai <- constrainBounds . MAttrIndex $ v
    return ai
    where
        f _ = printf "String \"%s\" doesn't look like an Attribute index" s

fromMAttrIndex :: (Num a) => MAttrIndex -> a
fromMAttrIndex (MAttrIndex s) = fromIntegral s

-- MAttrSize --

data MAttrSize = MAttrSize CUChar
               deriving (Show, Read, Eq, Ord)

instance Bounded MAttrSize where
    minBound = MAttrSize 1
    maxBound = MAttrSize 4

readMAttrSize :: String -> Either String MAttrSize
readMAttrSize s = do
    v <- modLeft f . readEither $ s
    as <- constrainBounds . MAttrSize $ v
    return as
    where
        f _ = printf "String \"%s\" doesn't look like an Attribute size" s

fromMAttrSize :: (Num a) => MAttrSize -> a
fromMAttrSize (MAttrSize s) = fromIntegral s

-- MAttrType --

data MAttrType = MAttrTypeFloat
               | MAttrTypeInt
               | MAttrTypeUInt
               | MAttrTypeNormInt
               | MAttrTypeNormUInt
               | MAttrTypeShort
               | MAttrTypeUShort
               | MAttrTypeNormShort
               | MAttrTypeNormUShort
               | MAttrTypeByte
               | MAttrTypeUByte
               | MAttrTypeNormByte
               | MAttrTypeNormUByte
               deriving (Show, Read, Eq)

readMAttrType :: String -> Either String MAttrType
readMAttrType "float"       = Right MAttrTypeFloat
readMAttrType "int"         = Right MAttrTypeInt
readMAttrType "uint"        = Right MAttrTypeUInt
readMAttrType "norm-int"    = Right MAttrTypeNormInt
readMAttrType "norm-uint"   = Right MAttrTypeNormUInt
readMAttrType "short"       = Right MAttrTypeShort
readMAttrType "ushort"      = Right MAttrTypeUShort
readMAttrType "norm-short"  = Right MAttrTypeNormShort
readMAttrType "norm-ushort" = Right MAttrTypeNormUShort
readMAttrType "byte"        = Right MAttrTypeByte
readMAttrType "ubyte"       = Right MAttrTypeUByte
readMAttrType "norm-byte"   = Right MAttrTypeNormByte
readMAttrType "norm-ubyte"  = Right MAttrTypeNormUByte
readMAttrType s             = Left $ printf "String \"%s\" doesn't look like an Attribute type" s

-- FIXME: This duplicates information about the association between MAttrType(s) and C-Types (formerly only present in readMAttrType)
extractMAttrType :: MAttrType -> Rational -> Rational
extractMAttrType MAttrTypeFloat      = id
extractMAttrType MAttrTypeInt        = id
extractMAttrType MAttrTypeUInt       = id
extractMAttrType MAttrTypeNormInt    = error "norm int?" -- (/ toRational (maxBound :: CInt))
extractMAttrType MAttrTypeNormUInt   = (/ toRational (maxBound :: CUInt))
extractMAttrType MAttrTypeShort      = id
extractMAttrType MAttrTypeUShort     = id
extractMAttrType MAttrTypeNormShort  = error "norm short?" -- (/ toRational (maxBound :: CShort))
extractMAttrType MAttrTypeNormUShort = (/ toRational (maxBound :: CUShort))
extractMAttrType MAttrTypeByte       = id
extractMAttrType MAttrTypeUByte      = id
extractMAttrType MAttrTypeNormByte   = error "norm byte?" -- (/ toRational (maxBound :: CChar))
extractMAttrType MAttrTypeNormUByte  = (/ toRational (maxBound :: CUChar))

-- MAttrData --

data MAttrData = MAttrDataFloat  [CFloat]
               | MAttrDataUInt   [CUInt]
               | MAttrDataInt    [CInt]
               | MAttrDataUShort [CUShort]
               | MAttrDataShort  [CShort]
               | MAttrDataUByte  [CUChar]
               | MAttrDataByte   [CChar]
               deriving (Show, Read, Eq)

readMAttrData :: MAttrSize -> MAttrType -> [String] -> Either String MAttrData
readMAttrData as at ss = do
    guard (n > 0) $ printf "Attribute element must contain whitespace delimited values"
    guard (n `mod` s  == 0) $ printf "Attribute element contains %d values; must be a multiple of size=\"%d\"" n s
    ad <- modLeft f . reader $ ss
    return ad
    where
        n = length ss
        s = fromMAttrSize as
        f _ = printf "Some part <attribute ...>%s</attribute> doesn't look like an %s value" (unwords ss) (show at)
        reader :: [String] -> Either String MAttrData
        reader = case at of
            MAttrTypeFloat      -> fmap MAttrDataFloat  . readManyEither
            MAttrTypeInt        -> fmap MAttrDataInt    . readManyEither
            MAttrTypeUInt       -> fmap MAttrDataUInt   . readManyEither
            MAttrTypeNormInt    -> fmap MAttrDataInt    . readManyEither
            MAttrTypeNormUInt   -> fmap MAttrDataUInt   . readManyEither
            MAttrTypeShort      -> fmap MAttrDataShort  . readManyEither
            MAttrTypeUShort     -> fmap MAttrDataUShort . readManyEither
            MAttrTypeNormShort  -> fmap MAttrDataShort  . readManyEither
            MAttrTypeNormUShort -> fmap MAttrDataUShort . readManyEither
            MAttrTypeByte       -> fmap MAttrDataByte   . readManyEither
            MAttrTypeUByte      -> fmap MAttrDataUByte  . readManyEither
            MAttrTypeNormByte   -> fmap MAttrDataByte   . readManyEither
            MAttrTypeNormUByte  -> fmap MAttrDataUByte  . readManyEither

lengthMAttrData :: MAttrData -> Int
lengthMAttrData (MAttrDataFloat  ns) = length ns
lengthMAttrData (MAttrDataUInt   ns) = length ns
lengthMAttrData (MAttrDataInt    ns) = length ns
lengthMAttrData (MAttrDataUShort ns) = length ns
lengthMAttrData (MAttrDataShort  ns) = length ns
lengthMAttrData (MAttrDataUByte  ns) = length ns
lengthMAttrData (MAttrDataByte   ns) = length ns

extractMAttrData :: MAttrData -> [Rational]
extractMAttrData (MAttrDataFloat  ns) = map toRational ns
extractMAttrData (MAttrDataUInt   ns) = map toRational ns
extractMAttrData (MAttrDataInt    ns) = map toRational ns
extractMAttrData (MAttrDataUShort ns) = map toRational ns
extractMAttrData (MAttrDataShort  ns) = map toRational ns
extractMAttrData (MAttrDataUByte  ns) = map toRational ns
extractMAttrData (MAttrDataByte   ns) = map toRational ns

-- eof
