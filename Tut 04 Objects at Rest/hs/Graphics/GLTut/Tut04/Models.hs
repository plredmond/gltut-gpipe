module Graphics.GLTut.Tut04.Models
( load_cube
, PrimStream
) where

import qualified Graphics.GLTut.VecFile as VecFile
import qualified Paths_gltut_tut04 as Paths

import Graphics.GPipe (PrimitiveStream(), Triangle(TriangleList), Vertex())
import Data.Vec (Vec4())

type PrimStream = PrimitiveStream Triangle ( Vec4 (Vertex Float)
                                           , Vec4 (Vertex Float) )

load_cube :: IO PrimStream
load_cube = do
    p <- Paths.getDataFileName "cube.vec4"
    VecFile.loadStream TriangleList p []

