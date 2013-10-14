module Lib.Perspective where

import Data.Vec

-- Perspective matrix.
m :: Float -> Float -> Float -> Mat44 Float
m frustrumScale zNear zFar = 
        (scx:.  0:.  0:.  0:.()):.
        (  0:.scy:.  0:.  0:.()):.
        (  0:.  0:.pr1:.pr2:.()):.
        (  0:.  0:.pr3:.  0:.()):.
        ()
    where
        scx = frustrumScale
        scy = frustrumScale
        pr1 = (zNear + zFar) / (zNear - zFar)
        pr2 = (2 * zNear * zFar) / (zNear - zFar)
        pr3 = -1

-- Perspective matrix with a fixed aspect ratio.
m_ar :: Float -> Float -> Float -> Vec2 Float -> Mat44 Float
m_ar frustrumScale zNear zFar (w:.h:.()) = 
        (scx:.  0:.  0:.  0:.()):.
        (  0:.scy:.  0:.  0:.()):.
        (  0:.  0:.pr1:.pr2:.()):.
        (  0:.  0:.pr3:.  0:.()):.
        ()
    where
        scx = frustrumScale / (w / h)
        scy = frustrumScale
        pr1 = (zNear + zFar) / (zNear - zFar)
        pr2 = (2 * zNear * zFar) / (zNear - zFar)
        pr3 = -1

-- Perspective matrix with a fixed aspect ratio and a variable perspective plane location.
m_ar_pp :: Float -> Float -> Float -> Vec2 Float -> Vec3 Float -> Mat44 Float
m_ar_pp frustrumScale zNear zFar (w:.h:.()) (ppx:.ppy:.ppz:.()) = 
        (scx:.  0:.  0:.ppx:.()):.
        (  0:.scy:.  0:.ppy:.()):.
        (  0:.  0:.pr1:.pr2:.()):.
        (  0:.  0:.pr3:.  0:.()):.
        ()
    where
        scx = frustrumScale / (w / h)
        scy = frustrumScale
        pr1 = (zNear + zFar) / (zNear - zFar)
        pr2 = (2 * zNear * zFar) / (zNear - zFar)
        pr3 = -1 / (abs ppz)

-- eof
