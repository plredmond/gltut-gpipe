module Graphics.GLTut.Tut04.VarProjectionPlane
( vpp
) where

import Graphics.GLTut.Easing as Easing

import Graphics.GPipe
import Data.Vec as V

-- Return the position of the center of a variable perspective plane, and its FragmentStream.
-- This is basically a custom easing function which renders the stream in this module.
vpp :: Float -> (Vec4 Float, FragmentStream (Color RGBFormat (Fragment Float)))
vpp sec = (offset, frags)
    where
        offset = (Easing.easeThereAndBack sec 2 `Easing.onRange` (-0.5,  0.5)):.
                 (Easing.easeThereAndBack sec 4 `Easing.onRange` (-0.5,  0.5)):.
                 (Easing.easeThereAndBack sec 8 `Easing.onRange` (-0.5, -1.5)):.
                 1:.()
        offset_unif = toGPU offset
        frags = fmap (\() -> RGB $ vec 1)
              $ rasterizeFront
              $ fmap (\pos -> (pos + offset_unif, ()))
              stream

stream :: PrimitiveStream Line (Vec4 (Vertex Float))
stream = toGPUStream LineStrip
    [ (-1):.(-1):.0:.0:.()
    , (-1):.( 1):.0:.0:.()
    , ( 1):.( 1):.0:.0:.()
    , ( 1):.(-1):.0:.0:.()
    , (-1):.(-1):.0:.0:.()
    ]

-- eof
