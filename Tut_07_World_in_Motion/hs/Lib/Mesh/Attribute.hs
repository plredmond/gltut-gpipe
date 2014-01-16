module Lib.Mesh.Attribute
( mkAttribute
, mkVAO
) where

-- hackage
import Foreign.C.Types (CFloat, CInt, CShort, CChar, CUInt, CUShort, CUChar)
import Text.XML.Light (Element)
import Text.Read (readEither)
import Text.Printf (printf)

-- local
import Lib.Mesh.XML
import Lib.Mesh.Util
import qualified Lib.Mesh.PrimToGPU as PTG

-- Attribute ------------------------------------------------------------------

mkAttribute :: Element -> Either String (Int, PTG.Attr)
mkAttribute el = do
    index   <- extractAttr "index" readIndex el
    stride  <- extractAttr "size"  readSize el
    numtype <- extractAttr "type"  readType el
    let i = fromIntegral $ getIndex index
        s = fromIntegral $ getSize stride
    nums <- extractList (readByType numtype) el
    guard (length nums > 0)
          $ printf "Attribute with index %d must contain some whitespace delimited values" i
    attr <- let errf e = printf "Attribute with index %d: %s" i e
            in modLeft errf . PTG.attr s . map fromRational $ nums
    return (i, attr)

-- Index ----------------------------------------------------------------------

newtype Index = Index {getIndex::CInt} deriving (Show, Read, Eq, Ord)

instance Bounded Index where
    minBound = Index 0
    maxBound = Index 15

readIndex :: String -> Either String Index
readIndex s = do
    num <- let errf _ = printf "String \"%s\" doesn't look like an Attribute Index" s
           in modLeft errf . readEither $ s
    constrainBounds . Index $ num

-- Size -----------------------------------------------------------------------

newtype Size = Size {getSize::CChar} deriving (Show, Read, Eq, Ord)

instance Bounded Size where
    minBound = Size 1
    maxBound = Size 4

readSize :: String -> Either String Size
readSize s = do
    num <- let errf _ = printf "String \"%s\" doesn't look like an Attribute Size" s
           in modLeft errf . readEither $ s
    constrainBounds . Size $ num

-- Type -----------------------------------------------------------------------

data Type = TypeFloat
          | TypeInt   | TypeUInt   | TypeNormInt   | TypeNormUInt
          | TypeShort | TypeUShort | TypeNormShort | TypeNormUShort
          | TypeChar  | TypeUChar  | TypeNormChar  | TypeNormUChar
          deriving (Show, Read, Eq)

readType :: String -> Either String Type
readType "float"       = Right TypeFloat
readType "int"         = Right TypeInt
readType "uint"        = Right TypeUInt
readType "norm-int"    = Right TypeNormInt
readType "norm-uint"   = Right TypeNormUInt
readType "short"       = Right TypeShort
readType "ushort"      = Right TypeUShort
readType "norm-short"  = Right TypeNormShort
readType "norm-ushort" = Right TypeNormUShort
readType "byte"        = Right TypeChar
readType "ubyte"       = Right TypeUChar
readType "norm-byte"   = Right TypeNormChar
readType "norm-ubyte"  = Right TypeNormUChar
readType s             = Left $ printf "String \"%s\" doesn't look like an Attribute Type" s

-- Read the string to the proper C type, convert to a rational, and optionally perform normalization.
-- GPipe doesn't support normalized types or Ints to my knowledge.
readByType :: Type -> String -> Either String Rational
-- float
readByType TypeFloat s = do { n <- readEither s; return $ toRational (n :: CFloat) }
-- 3x int signed
readByType TypeInt   s = do { n <- readEither s; return $ toRational (n :: CInt  ) }
readByType TypeShort s = do { n <- readEither s; return $ toRational (n :: CShort) }
readByType TypeChar  s = do { n <- readEither s; return $ toRational (n :: CChar ) }
-- 3x int unsigned
readByType TypeUInt   s = do { n <- readEither s; return $ toRational (n :: CUInt  ) }
readByType TypeUShort s = do { n <- readEither s; return $ toRational (n :: CUShort) }
readByType TypeUChar  s = do { n <- readEither s; return $ toRational (n :: CUChar ) }
-- 3x norm signed
readByType TypeNormInt   s = do let max = toRational (maxBound :: CInt)
                                    min = toRational (minBound :: CInt)
                                n <- readEither s
                                return $ toRational (n :: CInt) / (max - min)
readByType TypeNormShort s = do let max = toRational (maxBound :: CShort)
                                    min = toRational (minBound :: CShort)
                                n <- readEither s
                                return $ toRational (n :: CShort) / (max - min)
readByType TypeNormChar  s = do let max = toRational (maxBound :: CChar)
                                    min = toRational (minBound :: CChar)
                                n <- readEither s
                                return $ toRational (n :: CChar) / (max - min)
-- 3x norm unsigned
readByType TypeNormUInt   s = do let max = toRational (maxBound :: CUInt)
                                 n <- readEither s
                                 return $ toRational (n :: CUInt) / max
readByType TypeNormUShort s = do let max = toRational (maxBound :: CUShort)
                                 n <- readEither s
                                 return $ toRational (n :: CUShort) / max
readByType TypeNormUChar  s = do let max = toRational (maxBound :: CUChar)
                                 n <- readEither s
                                 return $ toRational (n :: CUChar) / max

-- VAO ------------------------------------------------------------------------

type VAO = (String, [Int])

mkVAO :: Element -> Either String VAO
mkVAO el = do
    name <- requireAttr "name" el
    indexes <- mapM (extractAttr "attrib" readIndex)
                    (rawFindChildren "source" el)
    return (name, map (fromIntegral . getIndex) indexes)

-- eof
