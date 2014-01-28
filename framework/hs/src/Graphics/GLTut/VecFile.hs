module Graphics.GLTut.VecFile where

-- Provide vec-file loading routines for tutorials 5, 6, and others.

-- A vec-file is a file with line-delimited vectors of whitespace delimited numbers.
-- All of the vectors must have the same length.
-- The first half of the vectors is zipped with the second half of the vectors in the final PrimitiveStream.

import Graphics.GPipe
import Data.Vec
import Prelude as P

-- Load a PrimitiveStream from a VecFile
loadStream primcmd path indexes = do
    dat <- readFile path
    return (readStream primcmd dat indexes)

-- Read a PrimitiveStream from a VecFile formatted string
readStream p s [] = toGPUStream        p (readTups s)
readStream p s ns = toIndexedGPUStream p (readTups s) ns

-- read (Vec# ?, Vec# ?) from a string containing numbers
readTups s = zipHalves vecs
    where
        vecs = P.map fromList $ readNums s

-- read lines of whitespace-delimited numbers from a string
readNums :: (Read a, Num a) => String -> [[a]]
readNums s = filter (not . null) lolon
    where
        los = lines s
        lolos = P.map words los
        lolon = P.map (P.map read) lolos

-- zip the two halves of a list (like a,b,c,d,e,f -> ad,be,cf)
-- if the length of the list is odd, the last elemet is omitted
zipHalves :: [a] -> [(a, a)]
zipHalves xs = zip ps qs
    where
        half = div (P.length xs) 2
        (ps, qs) = splitAt half xs

-- eof
