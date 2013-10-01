module Tetrahedron
( tetrahedron
) where

import Data.Vec
import Graphics.GPipe

col_green = (0  ):.(1  ):.(0  ):.()
col_blue  = (0  ):.(0  ):.(1  ):.()
col_red   = (1  ):.(0  ):.(0  ):.()
col_brown = (0.5):.(0.5):.(0  ):.()

type TetrahedronStream = PrimitiveStream Triangle (Vec3 (Vertex Float),
                                                   Vec3 (Vertex Float))

tetrahedron :: TetrahedronStream
tetrahedron = toIndexedGPUStream
            TriangleList
            -- Vertex Data
            [ -- tetrahedron a
              (( 1):.( 1):.( 1):.(), col_green),
              ((-1):.(-1):.( 1):.(), col_blue ),
              ((-1):.( 1):.(-1):.(), col_red  ),
              (( 1):.(-1):.(-1):.(), col_brown),
              -- tetrahedron b
              ((-1):.(-1):.(-1):.(), col_green),
              (( 1):.( 1):.(-1):.(), col_blue ),
              (( 1):.(-1):.( 1):.(), col_red  ),
              ((-1):.( 1):.( 1):.(), col_brown) ]
            -- Indexes
            [ -- tetrahedron a
              0, 1, 2,
              1, 0, 3,
              2, 3, 0,
              3, 2, 1,
              -- tetrahedron b
              5, 4, 6,
              4, 5, 7,
              7, 6, 4,
              6, 7, 5 ]

-- eof
