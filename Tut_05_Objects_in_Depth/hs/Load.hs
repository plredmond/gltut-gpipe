module Load where

import Prelude as P
import Data.Vec
import Graphics.GPipe
-- local
import Mesh

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Model loading routines

projPlane :: Mesh
projPlane = MeshPos
    ( MeshCfg
        { mcOffset = vec 0
        , mcBaseCol = 0.7:.0.5:.0.5:.1:.()
        , mcWinding = Clockwise } )
    ( toGPUStream TriangleFan
        [ (-1):.( 1):.(0):.(1):.()
        , (-1):.(-1):.(0):.(1):.()
        , ( 1):.(-1):.(0):.(1):.()
        , ( 1):.( 1):.(0):.(1):.() ] )

-- (0)   (3)
--  | /\
--  |   \
--  v    \
-- (1)-->(2)

loadMeshPosCol :: String -> [Int] -> IO Mesh
loadMeshPosCol s xs = do
    ps <- loadTriPosCol s xs
    return $ MeshPosCol
        ( MeshCfg
            { mcOffset = vec 0
            , mcBaseCol = vec 1
            , mcWinding = CounterClockwise } )
        ps

-- Load a model from a file containing first vertex data and later color data
-- IO: Call loadNums
loadTriPosCol :: String -> [Int] -> IO (PrimitiveStream Triangle (PosV, ColV))
loadTriPosCol path [] = runloader path $ toGPUStream TriangleList
loadTriPosCol path xs = runloader path $ (flip $ toIndexedGPUStream TriangleList) xs

runloader path toGPUfn = do
    lolon <- loadNums path
    return $ toGPUfn $ zipHalves $ P.map fromList lolon

-- Zip the two halfs of a list (like abcdef -> adbecf)
zipHalves :: [a] -> [(a, a)]
zipHalves xs = zip ps qs
    where
        half = div (P.length xs) 2
        (ps, qs) = splitAt half xs

-- Load file with line-delimited groups of whitespace-delimited numbers
-- IO: Read a file
loadNums :: (Read a, Num a) => String -> IO ([[a]])
loadNums fn = do
    s <- readFile fn
    return $ P.map (P.map read) $ filter (not . null) $ P.map words $ lines s        

-- eof
