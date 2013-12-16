module Lib.Mesh.MPrim where

-- hackage
import Graphics.GPipe
import Foreign.C.Types
import Text.XML.Light hiding (Line)
import Text.Printf (printf)
import Text.Read (readEither)
import Control.Monad (forM_)
import Data.Maybe (catMaybes)

-- local
import Lib.Mesh.XML
import Lib.Mesh.Util

-- MPrim ----------------------------------------------------------------------

data MPrim = MPrim MPrimCmd MPrimSel
           deriving (Show, Eq)

mkMPrim :: Element -> Either String MPrim
mkMPrim e = do
    cmd <- mkMPrimCmd e
    sel <- mkMPrimSel e
    return $ MPrim cmd sel

extractMPrim :: (VertexInput a) => MPrim -> MPrimLdr a
extractMPrim (MPrim (MPrimCmdTriangle t) sel) = MPrimLdrTriangle $ extractMPrimSel sel t
extractMPrim (MPrim (MPrimCmdLine     l) sel) = MPrimLdrLine     $ extractMPrimSel sel l
extractMPrim (MPrim (MPrimCmdPoint    p) sel) = MPrimLdrPoint    $ extractMPrimSel sel p

-- MPrimLdr -------------------------------------------------------------------

-- Loaders take an extracted attribute-array and put it on the GPU.
data MPrimLdr a = MPrimLdrTriangle ([CPU a] -> PrimitiveStream Triangle a)
                | MPrimLdrLine     ([CPU a] -> PrimitiveStream Line     a)
                | MPrimLdrPoint    ([CPU a] -> PrimitiveStream Point    a)

partitionLoaders :: [MPrimLdr a] -> ( [[CPU a] -> PrimitiveStream Triangle a]
                                    , [[CPU a] -> PrimitiveStream Line     a]
                                    , [[CPU a] -> PrimitiveStream Point    a]
                                    )
partitionLoaders xs = rot3cols . map extractLoader $ xs
    where
        extractLoader (MPrimLdrTriangle f) = (Just f, Nothing, Nothing)
        extractLoader (MPrimLdrLine     f) = (Nothing, Just f, Nothing)
        extractLoader (MPrimLdrPoint    f) = (Nothing, Nothing, Just f)
        rot3cols :: [(Maybe a, Maybe b, Maybe c)] -> ([a], [b], [c])
        rot3cols xs = let (as, bs, cs) = unzip3 xs
                       in (catMaybes as, catMaybes bs, catMaybes cs)

-- MPrimCmd -------------------------------------------------------------------

-- Commands specify how to string together attribute elements for drawing.
data MPrimCmd = MPrimCmdTriangle Triangle
              | MPrimCmdLine     Line
              | MPrimCmdPoint    Point
              deriving (Show, Eq)

mkMPrimCmd :: Element -> Either String MPrimCmd
mkMPrimCmd e = do
    pc <- extractAttr "cmd" readMPrimCmd e
    return pc

readMPrimCmd :: String -> Either String MPrimCmd
readMPrimCmd "triangles"  = Right $ MPrimCmdTriangle TriangleList
readMPrimCmd "tri-strip"  = Right $ MPrimCmdTriangle TriangleStrip
readMPrimCmd "tri-fan"    = Right $ MPrimCmdTriangle TriangleFan
readMPrimCmd "points"     = Right $ MPrimCmdPoint PointList
readMPrimCmd "lines"      = Right $ MPrimCmdLine LineList
readMPrimCmd "line-strip" = Right $ MPrimCmdLine LineStrip
readMPrimCmd "line-loop"  = Left "Line loops aren't supported by GPipe"
readMPrimCmd s            = Left $ printf "String \"%s\" doesn't look like a primitive command" s

-- MPrimSel -------------------------------------------------------------------

-- Selections specify which parts of an attribute-array to draw.
data MPrimSel = MPrimSelArray {mpsStart::CInt, mpsCount::CInt}
              | MPrimSelIndex [CInt]
              deriving (Show, Read, Eq)

mkMPrimSel :: Element -> Either String MPrimSel
mkMPrimSel e | n == "arrays"  = mkMPrimSelArray e
             | n == "indices" = mkMPrimSelIndex e
             | otherwise      = Left $ printf "Element \"%s\" is not a known selection type" n
    where
        n = rawElName e
 
mkMPrimSelArray :: Element -> Either String MPrimSel
mkMPrimSelArray e = do
    i <- extractPosNumAttr "start" e
    n <- extractPosNumAttr "count" e
    return $ MPrimSelArray i n
    where
        extractPosNumAttr s e = do -- see extractAttr
            n <- let f _ = printf "Array element %s attribute must be numeric" s
                     rf = modLeft f . readEither 
                  in extractAttr s rf e
            guard (n > 0) $ printf "Array element %s attribute must be greater than zero" s
            return n

mkMPrimSelIndex :: Element -> Either String MPrimSel
mkMPrimSelIndex e = do
    is <- let ss = childText e
              f _ = printf "Some part of <indices ...>%s</indices> doesn't look like an integral value" (unwords ss)
           in modLeft f . readManyEither $ ss
    guard (length is > 0) "Indices element must contain values"
    forM_ is $ \i -> do
        guard (i > 0) "Indices element must only contain positive values"
    return $ MPrimSelIndex is

extractMPrimSel :: (Primitive p, VertexInput a) => MPrimSel-> p -> [CPU a] -> PrimitiveStream p a
extractMPrimSel (MPrimSelArray ci cn) = \p xs -> toGPUStream p (take n . drop i $ xs) where i = fromIntegral ci; n = fromIntegral cn
extractMPrimSel (MPrimSelIndex cis  ) = \p xs -> toIndexedGPUStream p xs is           where is = map fromIntegral cis

-- eof
