module Graphics.GLTut.Perspective where
-- Perspective projection matrices for tutorials 4, 5, 6, and others.

import Linear

-- | Perspective matrix with specified frustrum-scale, and z-near & z-far
-- planes.
m :: Fractional a => a -> (a, a) -> M44 a
m frustrumScale (zNear, zFar) = V4
    (V4 scx   0   0   0)
    (V4   0 scy   0   0)
    (V4   0   0 pr1 pr2)
    (V4   0   0 pr3   0)
  where
    scx = frustrumScale
    scy = frustrumScale
    pr1 = (zNear + zFar) / (zNear - zFar)
    pr2 = (2 * zNear * zFar) / (zNear - zFar)
    pr3 = -1

-- | Perspective matrix with specified frustrum-scale, z-near & z-far planes,
-- and aspect ratio.
m_ar :: Fractional a => a -> (a, a) -> V2 a -> M44 a
m_ar frustrumScale (zNear, zFar) (V2 w h) = V4
    (V4 scx   0   0   0)
    (V4   0 scy   0   0)
    (V4   0   0 pr1 pr2)
    (V4   0   0 pr3   0)
  where
    scx = frustrumScale / (w / h)
    scy = frustrumScale
    pr1 = (zNear + zFar) / (zNear - zFar)
    pr2 = (2 * zNear * zFar) / (zNear - zFar)
    pr3 = -1

-- | Perspective matrix with specified frustrum-scale, z-near & z-far planes,
-- aspect ratio, and perspective plane location.
m_ar_pp :: Fractional a => a -> (a, a) -> V2 a-> V3 a -> M44 a
m_ar_pp frustrumScale (zNear, zFar) (V2 w h) (V3 ppx ppy ppz) = V4
    (V4 scx   0   0 ppx)
    (V4   0 scy   0 ppy)
    (V4   0   0 pr1 pr2)
    (V4   0   0 pr3   0)
  where
    scx = frustrumScale / (w / h)
    scy = frustrumScale
    pr1 = (zNear + zFar) / (zNear - zFar)
    pr2 = (2 * zNear * zFar) / (zNear - zFar)
    pr3 = -1 / (abs ppz)
