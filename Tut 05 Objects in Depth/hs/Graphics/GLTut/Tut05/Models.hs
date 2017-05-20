module Graphics.GLTut.Tut05.Models
( load_horizwedge
, load_vertiwedge
, PrimStream
) where

import qualified Graphics.GLTut.VecFile as VecFile
import qualified Paths_gltut_tut05 as Paths

import Graphics.GPipe (PrimitiveStream(), Triangle(TriangleList), Vertex())
import Data.Vec (Vec4())

type PrimStream = PrimitiveStream Triangle ( Vec4 (Vertex Float)
                                           , Vec4 (Vertex Float) )

load_horizwedge :: IO PrimStream
load_horizwedge = do
    p <- Paths.getDataFileName "horizwedge.vec4"
    VecFile.loadStream TriangleList p indexes

load_vertiwedge :: IO PrimStream
load_vertiwedge = do
    p <- Paths.getDataFileName "vertiwedge.vec4"
    VecFile.loadStream TriangleList p indexes

indexes :: [Int]
indexes = [ 0, 2, 1
          , 3, 2, 0
          , 4, 5, 6
          , 6, 7, 4
          , 8, 9, 10
          , 11, 13, 12
          , 14, 16, 15
          , 17, 16, 14 ]

