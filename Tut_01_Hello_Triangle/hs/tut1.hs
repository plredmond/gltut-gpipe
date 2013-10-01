import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec

main :: IO ()
main = do
    GLUT.getArgsAndInitialize
    newWindow
        "window title"
        (100 :. 100 :. ())  -- desired position
        (800 :. 600 :. ())  -- desired size
        (renderFrame)
        (initWindow)
    GLUT.mainLoop

initWindow :: GLUT.Window -> IO ()
initWindow w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)

renderFrame :: Vec2 Int -> IO (FrameBuffer RGBFormat () ())
renderFrame size = do
          return $ drawFrame size

type Int_v = Vertex Int
type Int2v = Vec2 Int_v
type Int3v = Vec3 Int_v
type Int4v = Vec4 Int_v
type Float_v = Vertex Float
type Float2v = Vec2 Float_v
type Float3v = Vec3 Float_v
type Float4v = Vec4 Float_v
type Int_f = Fragment Int
type Int2f = Vec2 Int_f
type Int3f = Vec3 Int_f
type Int4f = Vec4 Int_f
type Float_f = Fragment Float
type Float2f = Vec2 Float_f
type Float3f = Vec3 Float_f
type Float4f = Vec4 Float_f

drawFrame :: Vec2 Int -> FrameBuffer RGBFormat () ()
drawFrame size = draw fragments cleared
    where
        -- cleared -- a solid color framebuffer
        cleared :: FrameBuffer RGBFormat () ()
        cleared = newFrameBufferColor $ RGB (1:.0:.0.4:.())
        -- fragment stream
        fragments :: FragmentStream (Color RGBFormat Float_f)
        fragments = fmap fs
                  $ rasterizeBack
                  -- $ fmap (vs_applypos $ letterbox size)
                  $ fmap vs
                  model
        -- draw -- curry blending mode and boolean color mask onto paintColor
        draw :: FragmentStream (Color RGBFormat Float_f)
                -> FrameBuffer RGBFormat () ()
                -> FrameBuffer RGBFormat () ()
        draw = paintColor NoBlending (RGB $ vec True)

model :: PrimitiveStream Triangle Float4v
model = toGPUStream TriangleList $
    [   0.75  :.   0.75  :.  0.00  :.  1.00  :.  ()
    ,   0.75  :. (-0.75) :.  0.00  :.  1.00  :.  ()
    , (-0.75) :. (-0.75) :.  0.00  :.  1.00  :.  ()
    ]

-- Assign a color to each vertex
vs :: Float4v -> (Float4v, Float3v)
vs pos@(x:.y:.z:.w:.()) = ifB (y >* 0)
                              (pos, (1:.1:.1:.()))
                              (pos, (0:.0:.0:.()))

-- Output the given color for the fragment
fs :: Float3f -> Color RGBFormat Float_f
fs col = RGB $ col

-- Apply a function to pos without touching the rest of the tuple
-- http://stackoverflow.com/questions/4812633/making-a-a-a-functor
vs_applypos :: (Float4v -> Float4v) -> (Float4v, Float3v) -> (Float4v, Float3v)
vs_applypos fn (pos, aux) = (fn pos, aux)

-- Apply a scale to one component of a vector
scaleComponent :: (Nat n, Num a, Access n a v) => n -> a -> v -> v
scaleComponent n a v = set n (get n v * a) v

letterbox :: Vec2 Int -> Float4v -> Float4v
letterbox (w:.h:.()) pos = scaleComponent n1 wh pos
    where wh = fromIntegral w / fromIntegral h

pillarbox :: Vec2 Int -> Float4v -> Float4v
pillarbox (w:.h:.()) pos = scaleComponent n0 hw pos
    where hw = fromIntegral h / fromIntegral w

