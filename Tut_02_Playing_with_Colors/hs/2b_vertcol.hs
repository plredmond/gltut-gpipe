import qualified Graphics.UI.GLUT as GLUT
import qualified Data.Vec as Vec
import Graphics.GPipe
import Data.Vec

-- vertex shader types
type Int_v = Vertex Int
type Int2v = Vec2 Int_v
type Int3v = Vec3 Int_v
type Int4v = Vec4 Int_v
type Float_v = Vertex Float
type Float2v = Vec2 Float_v
type Float3v = Vec3 Float_v
type Float4v = Vec4 Float_v
-- fragment shader types
type Int_f = Fragment Int
type Int2f = Vec2 Int_f
type Int3f = Vec3 Int_f
type Int4f = Vec4 Int_f
type Float_f = Fragment Float
type Float2f = Vec2 Float_f
type Float3f = Vec3 Float_f
type Float4f = Vec4 Float_f
-- my types
type Posv = Float4v
type Auxv = Float4v
type Auxf = Float4f
type Stream a b = PrimitiveStream Triangle (a, b)

main :: IO ()
main = do
    GLUT.getArgsAndInitialize
    newWindow
        "window title"
        (101 :. 100 :. ())  -- desired position
        (800 :. 600 :. ())  -- desired size
        (renderFrame m)
        (initWindow)
    GLUT.mainLoop
    where
        m = vs_loadmodel model

initWindow :: GLUT.Window -> IO ()
initWindow w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)

renderFrame :: Stream Posv Auxv -> Vec2 Int -> IO (FrameBuffer RGBAFormat () ())
renderFrame m size = do
    return $ drawFrame m size

drawFrame :: Stream Posv Auxv -> Vec2 Int -> FrameBuffer RGBAFormat () ()
drawFrame m size = draw fragments cleared
    where
        -- cleared -- a solid color framebuffer
        cleared :: FrameBuffer RGBAFormat () ()
        cleared = newFrameBufferColor $ RGBA (0:.0:.0:.()) 1.0
        -- fragment stream
        fragments :: FragmentStream (Color RGBAFormat Float_f)
        fragments = fmap fs_passthru
                  $ rasterizeBack
                  $ fmap (vs_applypos $ letterbox size)
                  m
        -- draw -- curry blending mode and boolean color mask onto paintColor
        draw :: FragmentStream (Color RGBAFormat Float_f)
                -> FrameBuffer RGBAFormat () ()
                -> FrameBuffer RGBAFormat () ()
        draw = paintColor NoBlending (RGBA (vec True) True)

model :: [Vec4 Float]
model = [  0.0 :.  0.5   :.0.0:.1.0:.()
        ,  0.5 :.(-0.366):.0.0:.1.0:.()
        ,(-0.5):.(-0.366):.0.0:.1.0:.()
        ,  1.0 :.  0.0   :.0.0:.1.0:.()
        ,  0.0 :.  1.0   :.0.0:.1.0:.()
        ,  0.0 :.  0.0   :.1.0:.1.0:.()
        ]

-- Load a single list of Vec4 as both vertexes and colors
vs_loadmodel :: [Vec4 Float] -> Stream Posv Auxv
vs_loadmodel m = toGPUStream TriangleList $ zip vs cs
    where
        (vs, cs) = splitAt (div (Prelude.length m) 2) m

-- Output the given color for the fragment
fs_passthru :: Auxf -> Color RGBAFormat Float_f
fs_passthru rgba = RGBA (Vec.take n3 rgba) (Vec.last rgba)

-- Apply a function to pos without touching the rest of the tuple
-- http://stackoverflow.com/questions/4812633/making-a-a-a-functor
vs_applypos :: (Posv -> Posv) -> (Posv, Auxv) -> (Posv, Auxv)
vs_applypos fn (pos, aux) = (fn pos, aux)

-- Apply a scale to one component of a vector
scaleComponent :: (Nat n, Num a, Access n a v) => n -> a -> v -> v
scaleComponent n a v = set n (get n v * a) v

-- Scale the y component by the ratio of width:height
letterbox :: Vec2 Int -> Posv -> Posv
letterbox (w:.h:.()) pos = scaleComponent n1 wh pos
    where wh = fromIntegral w / fromIntegral h

-- Scale the x component by the ratio of height:width
pillarbox :: Vec2 Int -> Posv -> Posv
pillarbox (w:.h:.()) pos = scaleComponent n0 hw pos
    where hw = fromIntegral h / fromIntegral w

