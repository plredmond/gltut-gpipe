module Lib.Mesh.MPrim where

-- hackage
import Graphics.GPipe
import Foreign.C.Types
import Text.XML.Light hiding (Line)
import Text.Printf (printf)
import Text.Read (readEither)

import Data.Maybe (catMaybes)
import Data.Either (partitionEithers)

-- local
import Lib.Mesh.XML
import Lib.Mesh.Util

-- Indices/Arrays specify which parts of an arbitrary attribute to draw.

-- MPrim ---------------------------------------------------------------------

data MPrim = MPrimIndex {mpiCmd::MPrimCmd, mpIndices::[CInt]}
           | MPrimArray {mpaCmd::MPrimCmd, mpStart::CInt, mpCount::CInt}
           deriving (Show, Read, Eq)

mkMPrim :: Element -> Either String MPrim
mkMPrim e
    | n == "arrays"  = mkMPrimArray e
    | n == "indices" = mkMPrimIndex e
    | otherwise      = Left $ printf "Element \"%s\" is not a known primitive" n
    where
        n = rawElName e

mkMPrimArray :: Element -> Either String MPrim
mkMPrimArray e = do
    c <- extractAttr "cmd" readMPrimCmd e
    s <- extractPosNumAttr "start" e
    n <- extractPosNumAttr "count" e
    return $ MPrimArray c s n
    where
        extractPosNumAttr s e = do
            n <- let f _ = printf "Array element %s attribute must be numeric" s
                     r = modLeft f . readEither 
                  in extractAttr s r e
            constrainRange s n
            return n
        constrainRange s n
            | n < 0     = Left $ printf "Array element %s attribute must be greater than zero" s
            | otherwise = Right ()

mkMPrimIndex :: Element -> Either String MPrim
mkMPrimIndex e = do
    c <- extractAttr "cmd" readMPrimCmd e
    is <- let ss = childText e
              f _ = printf "Some part of <indices ...>%s</indices> doesn't look like an integral value" (unwords ss)
           in modLeft f . readManyEither $ ss
    constrainLength is
    mapM_ constrainRange is
    return $ MPrimIndex c is
    where
        constrainLength [] = Left "Indices element must contain values"
        constrainLength _  = Right ()
        constrainRange n
            | n < 0     = Left "Indices element must only contain positive values"
            | otherwise = Right ()

-- MPrimCmd --

data MPrimCmd = MPrimTriangleList
              | MPrimTriangleStrip
              | MPrimTriangleFan
              | MPrimLineList
              | MPrimLineStrip
              | MPrimLineLoop
              | MPrimPointList
              deriving (Show, Read, Eq)

readMPrimCmd :: String -> Either String MPrimCmd
readMPrimCmd "triangles"  = Right MPrimTriangleList
readMPrimCmd "tri-strip"  = Right MPrimTriangleStrip
readMPrimCmd "tri-fan"    = Right MPrimTriangleFan
readMPrimCmd "lines"      = Right MPrimLineList
readMPrimCmd "line-strip" = Right MPrimLineStrip
readMPrimCmd "line-loop"  = Right MPrimLineLoop
readMPrimCmd "points"     = Right MPrimPointList
readMPrimCmd s            = Left $ printf "String \"%s\" doesn't look like a primitive command" s


data MPrimToGPU a = MPrimToGPUTriangle ([CPU a] -> PrimitiveStream Triangle a)
                  | MPrimToGPULine     ([CPU a] -> PrimitiveStream Line     a)
                  | MPrimToGPUPoint    ([CPU a] -> PrimitiveStream Point    a)

extractMPrim' :: (VertexInput a) => MPrim -> Either String (MPrimToGPU a)
extractMPrim' mp = case mpc of
        MPrimTriangleList  -> Right . MPrimToGPUTriangle $ toLoader mp TriangleList
        MPrimTriangleStrip -> Right . MPrimToGPUTriangle $ toLoader mp TriangleStrip
        MPrimTriangleFan   -> Right . MPrimToGPUTriangle $ toLoader mp TriangleFan
        MPrimLineList      -> Right . MPrimToGPULine     $ toLoader mp LineList
        MPrimLineStrip     -> Right . MPrimToGPULine     $ toLoader mp LineStrip
        MPrimLineLoop      -> Left "GPipe does not support LineLoop"
        MPrimPointList     -> Right . MPrimToGPUPoint    $ toLoader mp PointList
    where
        mpc :: MPrimCmd
        mpc = case mp of MPrimIndex x _ -> x
                         MPrimArray x _ _ -> x
        toLoader :: (Primitive p, VertexInput a) => MPrim -> p -> [CPU a] -> PrimitiveStream p a
        toLoader (MPrimIndex _ cis  ) = let is = map fromIntegral cis
                                         in \p xs -> toIndexedGPUStream p xs is
        toLoader (MPrimArray _ ci cn) = let i = fromIntegral ci
                                            n = fromIntegral cn
                                         in \p xs -> toGPUStream p (take n . drop i $ xs)

type MPrimArbToGPU a = ( Maybe ([CPU a] -> PrimitiveStream Triangle a)
                       , Maybe ([CPU a] -> PrimitiveStream Line     a)
                       , Maybe ([CPU a] -> PrimitiveStream Point    a)
                       )

ambiguate :: MPrimToGPU a -> MPrimArbToGPU a
ambiguate (MPrimToGPUTriangle f) = (Just f, Nothing, Nothing)
ambiguate (MPrimToGPULine     f) = (Nothing, Just f, Nothing)
ambiguate (MPrimToGPUPoint    f) = (Nothing, Nothing, Just f)

extractMPrims :: (VertexInput a)
              => [MPrim] -> Either String ( [[CPU a] -> PrimitiveStream Triangle a]
                                          , [[CPU a] -> PrimitiveStream Line     a]
                                          , [[CPU a] -> PrimitiveStream Point    a]
                                          )
extractMPrims mps = do
   fs <- mapM extractMPrim' mps
   let (as, bs, cs) = unzip3 . map ambiguate $ fs
   return (catMaybes as, catMaybes bs, catMaybes cs)

-- eof
