module Lib.Mesh.MAttr where

-- hackage
import Foreign.C.Types
import Text.XML.Light
import Text.Read (readEither)
import Text.Printf (printf)

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
    ai <- extractAttr e "index" readMAttrIndex
    as <- extractAttr e "size" readMAttrSize
    at <- extractAttr e "type" readMAttrType
    ad <- readMAttrData as at $ childText e
    return $ MAttr ai as at ad

lengthMAttr :: MAttr -> Int
lengthMAttr a = (lengthMAttrData . maData $ a) `div` (fromIntegral s)
    where
        MAttrSize s = maSize a

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
readMAttrType s = case s of
    "float"       -> Right MAttrTypeFloat
    "int"         -> Right MAttrTypeInt
    "uint"        -> Right MAttrTypeUInt
    "norm-int"    -> Right MAttrTypeNormInt
    "norm-uint"   -> Right MAttrTypeNormUInt
    "short"       -> Right MAttrTypeShort
    "ushort"      -> Right MAttrTypeUShort
    "norm-short"  -> Right MAttrTypeNormShort
    "norm-ushort" -> Right MAttrTypeNormUShort
    "byte"        -> Right MAttrTypeByte
    "ubyte"       -> Right MAttrTypeUByte
    "norm-byte"   -> Right MAttrTypeNormByte
    "norm-ubyte"  -> Right MAttrTypeNormUByte
    _             -> Left $ printf "String \"%s\" doesn't look like an Attribute type" s

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
    constrainLength
    ad <- modLeft f . reader $ ss
    return ad
    where
        f _ = printf "Some part of \"%s\" doesn't look like an %s value" (unwords ss) (show at)
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
        constrainLength :: Either String ()
        constrainLength
            | n         == 0 = Left $ printf "Attribute element must contain whitespace delimited values"
            | n `mod` s /= 0 = Left $ printf "Attribute element contains %d values, must be a multiple of size=\"%d\"" s n
            | otherwise      = Right ()
            where
                n = length ss
                s = let MAttrSize cs = as in fromIntegral cs

lengthMAttrData :: MAttrData -> Int
lengthMAttrData (MAttrDataFloat  ns) = length ns
lengthMAttrData (MAttrDataUInt   ns) = length ns
lengthMAttrData (MAttrDataInt    ns) = length ns
lengthMAttrData (MAttrDataUShort ns) = length ns
lengthMAttrData (MAttrDataShort  ns) = length ns
lengthMAttrData (MAttrDataUByte  ns) = length ns
lengthMAttrData (MAttrDataByte   ns) = length ns

-- eof
