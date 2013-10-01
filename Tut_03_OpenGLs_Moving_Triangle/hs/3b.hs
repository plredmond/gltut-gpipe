import qualified Graphics.UI.GLUT as GLUT
import qualified Control.Concurrent.MVar as MVar
import Graphics.GPipe
import Prelude as P
import Data.Vec as Vec
import System.Environment (getArgs, getProgName)

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
type Sizev = Float2v
type Matrixc = Mat44 Float
type Matrixv = Mat44 Float_v
type Posv = Float4v
type Stream a = PrimitiveStream Triangle a

main :: IO ()
main = do
    name <- getProgName
    args <- getArgs
    if P.length args /= 3 then do
        putStrLn $ "USAGE: " ++ name ++ " scale x y"
    else do
        let [scale, x, y] = P.map read args in
            setup scale (x:.y:.()) name

setup :: Float -> Vec2 Float -> String -> IO ()
setup s t n = do
    GLUT.getArgsAndInitialize
    newWindow n (vec 8) (vec 512)
        (let mdl = toGPUStream TriangleList $ load model
             mat = let tv = snoc (Vec.map negate t) 0
                       tm = translate tv identity
                       sm = scale (vec s) identity
                   in toGPU $ tm `multmm` sm
         in renderFrame mat mdl)
        (initWindow)
    GLUT.mainLoop

initWindow :: GLUT.Window -> IO ()
initWindow w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)

-- Put everything else on the GPU
renderFrame :: Matrixv -> Stream (Posv, Float4v)
               -> Vec2 Int -> IO (FrameBuffer RGBAFormat () ())
renderFrame mat mdl size = do
    time <- GLUT.get GLUT.elapsedTime
    return $ let siz = toGPU $ Vec.map fromIntegral size
                 tim = fromIntegral time / 1000
             in  drawFrame mat mdl tim siz

-- Draw with GPU values
drawFrame :: Matrixv -> Stream (Posv, Float4v)
             -> Float -> Sizev -> FrameBuffer RGBAFormat () ()
drawFrame mat mdl time size = draw fragments cleared
    where
        -- cleared -- a solid color framebuffer
        cleared :: FrameBuffer RGBAFormat () ()
        cleared = newFrameBufferColor $ RGBA (0:.0:.0:.()) 1.0
        -- fragment stream
        fragments :: FragmentStream (Color RGBAFormat Float_f)
        fragments = fmap fs_mkcol
                  $ fmap (\c -> c * (fs_timecolor 10 $ toGPU time))
                  $ fmap (\(p, c) -> c * fs_fragpos p) -- mult col by fragment y-loc
                  $ rasterizeBack
                  $ fmap (\(p, c) -> (p, (p, c)))
                  $ fmap (vs_applypos $ letterbox size)
                  $ fmap (vs_applypos $ vs_morepowertotheshaders 5 $ toGPU time)
                  $ fmap (vs_applypos $ (\p -> mat `multmv` p))
                  mdl
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

-- Pair up a single list of Vec4 as both vertexes and colors
load :: [Vec4 Float] -> [(Vec4 Float, Vec4 Float)]
load dat = zip vs cs
    where
        (vs, cs) = splitAt (div (P.length dat) 2) dat

vs_morepowertotheshaders :: Float_v -> Float_v -> Posv -> Posv
vs_morepowertotheshaders duration time pos = pos + offset
    where
        t = mod' time duration
        ts = pi * 2 / duration
        offset = (0.5 * cos (t * ts)) :.
                 (0.5 * sin (t * ts)) :.
                 0.0 :. 0.0 :. ()

mix4 (x1:.y1:.z1:.w1:.()) (x2:.y2:.z2:.w2:.()) lerp =
    mix x1 x2 lerp :. mix y1 y2 lerp :. mix z1 z2 lerp :. mix w1 w2 lerp :. ()

fs_timecolor :: Float_f -> Float_f -> Float4f
fs_timecolor duration time = mix4 col1 col2 lerp
    where
        lerp = mod' time duration / duration
        col1 = 1.0 :. 1.0 :. 1.0 :. 1.0 :. ()
        col2 = 0.0 :. 1.0 :. 0.0 :. 1.0 :. ()

-- Output a vector lerp'd according to the y-position of the fragment
fs_fragpos :: Float4f -> Float4f
fs_fragpos pos = vec $ mix 1.0 0.2 lerp
    where
        lerp = (1 + get n1 pos) / 2.0

-- Output the given vector as a color
fs_mkcol :: Float4f -> Color RGBAFormat Float_f
fs_mkcol rgba = RGBA (Vec.take n3 rgba) (Vec.last rgba)

-- Apply a function to pos without touching the rest of the tuple
-- http://stackoverflow.com/questions/4812633/making-a-a-a-functor
vs_applypos :: (Posv -> Posv) -> (Posv, a) -> (Posv, a)
vs_applypos fn (pos, aux) = (fn pos, aux)

-- Apply a scale to one component of a vector
scaleComponent :: (Nat n, Num a, Access n a v) => n -> a -> v -> v
scaleComponent n a v = set n (get n v * a) v

-- Scale the y component by the ratio of width:height
letterbox :: Sizev -> Posv -> Posv
letterbox (w:.h:.()) pos = scaleComponent n1 (w / h) pos

-- Scale the x component by the ratio of height:width
pillarbox :: Sizev -> Posv -> Posv
pillarbox (w:.h:.()) pos = scaleComponent n0 (h / w) pos

