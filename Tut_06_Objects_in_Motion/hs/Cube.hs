module Cube
( CubeStream
, cube
) where

import Data.Vec
import Graphics.GPipe

col_red     = 1:.0:.0:.()
col_green   = 0:.1:.0:.()
col_blue    = 0:.0:.1:.()
col_yellow  = 1:.1:.0:.()
col_cyan    = 0:.1:.1:.()
col_magenta = 1:.0:.1:.()

type CubeStream = PrimitiveStream Triangle (Vec3 (Vertex Float),
                                            Vec3 (Vertex Float))

cube :: CubeStream
cube = toIndexedGPUStream TriangleList
    -- Vertex Data
    (let n = -1 in
        --Front
        [ (1:.1:.1:.(), col_green)
        , (1:.n:.1:.(), col_green)
        , (n:.n:.1:.(), col_green)
        , (n:.1:.1:.(), col_green)
        --Top
        , (1:.1:.1:.(), col_blue)
        , (n:.1:.1:.(), col_blue)
        , (n:.1:.n:.(), col_blue)
        , (1:.1:.n:.(), col_blue)
        --Left
        , (1:.1:.1:.(), col_red)
        , (1:.1:.n:.(), col_red)
        , (1:.n:.n:.(), col_red)
        , (1:.n:.1:.(), col_red)
        --Back
        , (1:.1:.n:.(), col_yellow)
        , (n:.1:.n:.(), col_yellow)
        , (n:.n:.n:.(), col_yellow)
        , (1:.n:.n:.(), col_yellow)
        --Bottom
        , (1:.n:.1:.(), col_cyan)
        , (1:.n:.n:.(), col_cyan)
        , (n:.n:.n:.(), col_cyan)
        , (n:.n:.1:.(), col_cyan)
        --Right
        , (n:.1:.1:.(), col_magenta)
        , (n:.n:.1:.(), col_magenta)
        , (n:.n:.n:.(), col_magenta)
        , (n:.1:.n:.(), col_magenta)
    ])
    -- Indexes
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
