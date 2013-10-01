module Util where

import Graphics.GPipe

degToRad :: Float -> Float
degToRad = (deg *)
    where
        tau = pi * 2
        deg = tau / 360

-- Output a value [0, 1) where n traces a cycle each max through  0..1..0..-1..repeat
cycle f max n = f $ full * curr
    where
        full = pi * 2 / max -- full is a full cycle scaled to max
        curr = mod' n max   -- curr is in [0, max)

-- EOF
