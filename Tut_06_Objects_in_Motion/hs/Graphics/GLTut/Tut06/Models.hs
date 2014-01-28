module Graphics.GLTut.Tut06.Models
( load_tetrahedron
, load_cube
, PrimStream
) where

import qualified Graphics.GLTut.VecFile as VecFile
import qualified Paths_gltut_tut06 as Paths

import Graphics.GPipe (PrimitiveStream(), Triangle(TriangleList), Vertex())
import Data.Vec (Vec3())

type PrimStream = PrimitiveStream Triangle ( Vec3 (Vertex Float)
                                           , Vec3 (Vertex Float) )

load_tetrahedron :: IO PrimStream
load_tetrahedron = do
    p <- Paths.getDataFileName "tetrahedron.vec3"
    VecFile.loadStream TriangleList p
        -- tetrahedron a
        [ 0, 1, 2
        , 1, 0, 3
        , 2, 3, 0
        , 3, 2, 1
        -- tetrahedron b
        , 5, 4, 6
        , 4, 5, 7
        , 7, 6, 4
        , 6, 7, 5
        ]

load_cube :: IO PrimStream
load_cube = do
    p <- Paths.getDataFileName "cube.vec3"
    VecFile.loadStream TriangleList p
        --Front
        [ 0, 1, 2
        , 2, 3, 0
        --Top
        , 4, 5, 6
        , 6, 7, 4
        --Left
        , 8, 9, 10
        , 10, 11, 8
        --Back
        , 12, 13, 14
        , 14, 15, 12
        --Bottom
        , 16, 17, 18
        , 18, 19, 16
        --Right
        , 20, 21, 22
        , 22, 23, 20
        ]

-- eof
