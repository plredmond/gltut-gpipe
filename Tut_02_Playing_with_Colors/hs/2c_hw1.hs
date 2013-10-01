import Data.Vec as Vec
import Graphics.GPipe
import Prelude as P
import System.Environment (getArgs, getProgName)
import qualified Graphics.UI.GLUT as GLUT

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
type Auxv = Float4v
type Auxf = Float4f
type Stream a b = PrimitiveStream Triangle (a, b)

main :: IO ()
main = do
    name <- getProgName
    args <- getArgs
    if P.length args /= 2 then do
        putStrLn $ "USAGE: " ++ name ++ " x y"
    else do
        let tgt = fromList $ P.map read args in
            setup tgt name

setup :: Vec2 Float -> String -> IO ()
setup tgt name = do
    GLUT.getArgsAndInitialize
    newWindow
        name
        (0 :. 0 :. ())  -- desired position
        (0 :. 0 :. ())  -- desired size
        (let mdl = toGPUStream TriangleList $ load model
             mat = toGPU $ translate (snoc (Vec.map negate tgt) 0) identity
         in renderFrame mat mdl)
        (initWindow)
    GLUT.mainLoop

initWindow :: GLUT.Window -> IO ()
initWindow w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)

-- Put everything else on the GPU
renderFrame :: Matrixv -> Stream Posv Auxv
               -> Vec2 Int -> IO (FrameBuffer RGBAFormat () ())
renderFrame mat mdl size = do
    return $ let siz = toGPU $ Vec.map fromIntegral size
             in  drawFrame mat mdl siz

-- Draw with the GPU values
drawFrame :: Matrixv -> Stream Posv Auxv
             -> Sizev -> FrameBuffer RGBAFormat () ()
drawFrame mat mdl size = draw fragments cleared
    where
        -- cleared -- a solid color framebuffer
        cleared :: FrameBuffer RGBAFormat () ()
        cleared = newFrameBufferColor $ RGBA (0:.0:.0:.()) 1.0
        -- fragment stream
        fragments :: FragmentStream (Color RGBAFormat Float_f)
        fragments = fmap fs_passthru
                  $ rasterizeBack
                  $ fmap (vs_applypos $ letterbox size)
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
letterbox :: Sizev -> Posv -> Posv
letterbox (w:.h:.()) pos = scaleComponent n1 (w / h) pos

-- Scale the x component by the ratio of height:width
pillarbox :: Sizev -> Posv -> Posv
pillarbox (w:.h:.()) pos = scaleComponent n0 (h / w) pos

