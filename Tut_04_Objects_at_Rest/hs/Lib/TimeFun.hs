module Lib.TimeFun where

import Graphics.GPipe
import Data.Vec

-- A value ranging up and down over [0, 1] during the duration.
computeCycle :: Float -> Float -> Float
computeCycle dur sec = 0.5 * (1 + (sin $ curTime * scale))
    where
        scale = pi * 2 / dur
        curTime = mod' sec dur

-- Return the position of a variable perspective plane, and its FragmentStream.
projectionPlane :: Float -> (Vec4 Float, FragmentStream (Color RGBAFormat (Fragment Float)))
projectionPlane sec = (pos, frags)
    where
        pos = (  0.5  - computeCycle 2 sec):.       -- [-0.5,  0.5]
              (  0.5  - computeCycle 4 sec):.       -- [-0.5,  0.5]
              ((-0.5) - computeCycle 8 sec):.1:.()  -- [-0.5, -1.5]
        pos_unif = toGPU pos
        frags = fmap (\() -> RGBA (vec 1) 1)
              $ rasterizeFront
              $ fmap (\disp -> (pos_unif + disp, ()))
              $ toGPUStream LineStrip
              [ (-1):.(-1):.0:.0:.()
              , (-1):.( 1):.0:.0:.()
              , ( 1):.( 1):.0:.0:.()
              , ( 1):.(-1):.0:.0:.()
              , (-1):.(-1):.0:.0:.()
              ]

-- eof
