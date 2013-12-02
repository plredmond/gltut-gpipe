module Lib.Mesh.MPrim where

-- hackage
import Text.XML.Light
import Graphics.GPipe
import Foreign.C.Types
import Text.Printf (printf)
import Text.Read (readEither)

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
readMPrimCmd s = case s of
    "triangles"  -> Right MPrimTriangleList
    "tri-strip"  -> Right MPrimTriangleStrip
    "tri-fan"    -> Right MPrimTriangleFan
    "lines"      -> Right MPrimLineList
    "line-strip" -> Right MPrimLineStrip
    "line-loop"  -> Right MPrimLineLoop
    "points"     -> Right MPrimPointList
    _            -> Left $ printf "String \"%s\" doesn't look like a primitive command" s

-- eof
