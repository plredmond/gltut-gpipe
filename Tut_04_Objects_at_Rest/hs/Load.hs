module Load where

import Graphics.GPipe
import Data.Vec
import Prelude as P

-- read a trangle-list from a string containing first vertex data and later color data
readStream s = toGPUStream TriangleList $ zipHalves lov
    where
        lov = P.map fromList $ readNums s

-- read lines of whitespace-delimited numbers from a string
readNums :: (Read a, Num a) => String -> [[a]]
readNums s = filter (not . null) lolon
    where
        los = lines s
        lolos = P.map words los
        lolon = P.map (P.map read) lolos

-- zip the two halves of a list
-- if the length of the list is odd, the last elemet is omitted
zipHalves :: [a] -> [(a, a)]
zipHalves xs = zip ps qs
    where
        half = div (P.length xs) 2
        (ps, qs) = splitAt half xs

-- eof
