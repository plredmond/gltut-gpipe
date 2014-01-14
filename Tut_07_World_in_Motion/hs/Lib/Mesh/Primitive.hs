{-# LANGUAGE StandaloneDeriving #-}

module Lib.Mesh.Primitive
( Primitive()
, mkPrimitive
, Command
, Selection
)where

-- hackage
import Graphics.GPipe (Triangle(..), Line(..), Point(..))
import Text.XML.Light (Element)
import Text.Printf (printf)
import Control.Monad (forM_)

-- local
import Lib.Mesh.Util
import Lib.Mesh.XML

-- Primitive ------------------------------------------------------------------

data Primitive = Prim Command Selection
                 deriving (Show, Read, Eq)

mkPrimitive :: Element -> Either String Primitive
mkPrimitive el = do
   cmd <- mkCommand el
   sel <- mkSelection el
   return $ Prim cmd sel

-- Command --------------------------------------------------------------------

deriving instance Read Triangle
deriving instance Read Line
deriving instance Read Point

-- Commands specify how to string together attribute elements for drawing.
type Command = Tri Triangle Line Point

mkCommand :: Element -> Either String Command
mkCommand = extractAttr "cmd" readCommand

readCommand :: String -> Either String Command
readCommand "triangles"  = Right $ Jyan TriangleList
readCommand "tri-strip"  = Right $ Jyan TriangleStrip
readCommand "tri-fan"    = Right $ Jyan TriangleFan
readCommand "lines"      = Right $ Ken LineList
readCommand "line-strip" = Right $ Ken LineStrip
readCommand "line-loop"  = Left "Line loops aren't supported by GPipe"
readCommand "points"     = Right $ Po PointList
readCommand s            = Left $ printf "String \"%s\" doesn't look like a primitive command." s

-- Selection ------------------------------------------------------------------

-- Selections specify which parts of an attribute-array to draw.
type Selection = Either (Int, Int) [Int]
-- Left (start, count) is an array primitive drawing part or all of the array.
-- Right [index]       is an indexed primitive.

mkSelection :: Element -> Either String Selection
mkSelection el | rawElName el == "arrays"  = mkSelectionArray el
               | rawElName el == "indices" = mkSelectionIndex el
               | otherwise                 = Left $ printf "Element \"%s\" is not a known selection type" (rawElName el)
 
mkSelectionArray :: Element -> Either String Selection
mkSelectionArray el = do
    start <- extractPosNumAttr "start"
    count <- extractPosNumAttr "count"
    return $ Left (start, count) -- non-monadic Left; this is an array selection
    where
        extractPosNumAttr name = do
            num <- extractNumAttr name el
            guard (num >= 0)
                  $ printf "Element \"%s\" requires attribute \"%s\" value, %d, to be positive" (rawElName el) name num
            return num

mkSelectionIndex :: Element -> Either String Selection
mkSelectionIndex el = do
    inds <- let strs = childText el
                errf _ = printf "Some part of <indices ...>%s</indices> doesn't look like an integral value" (unwords strs)
            in modLeft errf . readManyEither $ strs
    guard (length inds > 0) "Indices element must contain values"
    forM_ inds $ \ind -> do
        guard (ind >= 0) $ printf "Indices element must contain all positive values; saw %s" (show ind)
    return $ Right inds -- non-monadic Right; this is an index selection

-- eof
