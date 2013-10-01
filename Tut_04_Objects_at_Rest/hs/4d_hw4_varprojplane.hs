import qualified Graphics.UI.GLUT as GLUT
import qualified Control.Concurrent.MVar as MVar
import Graphics.GPipe
import Prelude as P
import Data.Vec as Vec
import System.Environment (getArgs, getProgName)
-- for saving images
import qualified Graphics.Rendering.OpenGL as GL
import qualified Foreign.Marshal.Array as Array

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main = do
    name <- getProgName
    Just (fn) <- parseArgs name
    model <- loadmodel fn
    setupGL name model
    mainloop

setupGL :: String -> Stream (Posv, Colv) -> IO ()
setupGL name model = do
    GLUT.getArgsAndInitialize
    newWindow name (vec 8) (384:.512:.())
        (renderFrame model)
        setupWindow

mainloop = do
    GLUT.mainLoop

parseArgs :: String -> IO (Maybe (String))
parseArgs name = do
    argv <- getArgs
    if P.length argv /= 1
    then do
        putStrLn $ "USAGE: ./" ++ name ++ " model-file"
        return Nothing
    else do
        let [fn] = argv
        return $ Just (fn)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- load a model from a file containing first vertex data and later color data
loadmodel :: String -> IO (Stream (Posv, Colv))
loadmodel fn = do
    lolon <- loadnums fn
    return $ toGPUStream TriangleList $ ziphalves (P.map fromList lolon)

-- zip the two halfs of a list
ziphalves :: [a] -> [(a, a)]
ziphalves xs = zip ps qs
    where
        half = div (P.length xs) 2
        (ps, qs) = splitAt half xs

-- load lines of numbers from a file
loadnums :: (Read a, Num a) => String -> IO ([[a]])
loadnums fn = do
    s <- readFile fn
    return $ P.map (P.map read) $ filter (not . null) $ P.map words $ lines s        

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

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
-- type Matrixc = Mat44 Float
type Matrixv = Mat44 Float_v
type Timv = Float_v
type Sizv = Float2v
type Posv = Float4v
type Colv = Float4v
type Stream a = PrimitiveStream Triangle a
type Colf = Float4f

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

setupWindow :: GLUT.Window -> IO ()
setupWindow w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)

-- Send current data to the GPU and call drawFrame
renderFrame :: Stream (Posv, Colv) ->
            Vec2 Int -> IO (FrameBuffer RGBAFormat () ())
renderFrame mdl size = do
    time <- GLUT.get GLUT.elapsedTime
    let gpusize = toGPU $ Vec.map fromIntegral size
        gputime = toGPU $ fromIntegral time / 1000
        projplane = (Main.cycle cos 5 gputime):.(Main.cycle sin 5 gputime):.(1 + 0.25 * Main.cycle sin 10 gputime):.()
--        projplane = 0:.0:.1:.()
        frame = drawFrame mdl gputime gpusize projplane
        (w:.h:.()) = size
    --
--    let buffer = Array.mallocArray (w * h) -- :: Foreign.C.Types.CFloat
--    GL.readPixels (GL.Position 0 0)
--                  (GL.Size w h)
--                  (GL.PixelData GL.RGB GL.Float buffer)
    --
    let ppmdl = toGPUStream TriangleFan [
            (-1):.( 1):.(-1   ):.1:.(),
            (-1):.(-1):.(-1   ):.1:.(),
            ( 1):.(-1):.(-1   ):.1:.(),
            ( 1):.( 1):.(-1   ):.1:.()  ] :: PrimitiveStream Triangle Float4v
        ppfrg = fmap (\_ -> RGBA (vec 1) 0.5)
              $ rasterizeFront
              $ fmap (\p -> (p, ()))
              $ fmap (vs_matrixpersp $ vs_projmat projplane (Vec.map fromIntegral size) 1.0 0.5 3.0)
              ppmdl
    return $ paintColor (BlendLogicOp Xor) (RGBA (vec True) True) ppfrg frame

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Draw with GPU values
drawFrame :: Stream (Posv, Colv) ->
             Timv -> Sizv -> Float3v -> FrameBuffer RGBAFormat () ()
drawFrame mdl time size projplane = draw fragments cleared
    where
        -- cleared -- a solid color framebuffer
        cleared :: FrameBuffer RGBAFormat () ()
        cleared = newFrameBufferColor $ RGBA (0:.0:.0:.()) 1.0
        -- fragment stream after applying shaders
        fragments :: FragmentStream (Color RGBAFormat Float_f)
        fragments = fmap fs_mkcol
                  -- 0 nogreen; 5 green; 10 nogreen 
--                  $ fmap (\c -> c * (fs_timecolor 10 $ toGPU time))
                  -- little red at top; little blue at bottom
--                  $ fmap (\(p, c) -> c * fs_fragpos p)
                  $ rasterizeBack -- only gives snd of tup to fshdrs
--                  $ fmap (\(p, c) -> (p, (p, c)))
--                  $ fmap (vs_applypos $ letterbox size)
--                  $ fmap (vs_applypos $ (\p -> mat `multmv` p))
                  $ fmap (vs_applypos $ vs_matrixpersp mat)
                  $ fmap (vs_applypos $ vs_morepowertotheshaders 5 time)
                  $ fmap (vs_applypos $ (\p -> p + offset))
                  mdl
        -- draw -- curry blending mode and boolean color mask onto paintColor
        draw :: FragmentStream (Color RGBAFormat Float_f)
                -> FrameBuffer RGBAFormat () ()
                -> FrameBuffer RGBAFormat () ()
        draw = paintColor NoBlending (RGBA (vec True) True)
--        -- pop 0.1..2.0 over 20s
--        pop :: Float_v
--        pop = ((mod' time 20) + 1) * 0.1
        -- mat
        mat = vs_projmat projplane size 1.0 0.5 3.0

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Output the given vector as a color
fs_mkcol :: Colf -> Color RGBAFormat Float_f
fs_mkcol rgba = RGBA (Vec.take n3 rgba) (Vec.last rgba)

-- Apply a function to pos without touching the rest of the tuple
vs_applypos :: (Posv -> Posv) -> (Posv, a) -> (Posv, a)
vs_applypos fn (pos, aux) = (fn pos, aux)

-- uniforms
offset :: Float4v
offset = 0.5:.0.5:.(-2):.0:.()

vs_projmat :: Float3v -> Sizv -> Float_v -> Float_v -> Float_v -> Matrixv -- Mat44 Float_v
vs_projmat (plx:.ply:.plz:.()) size scale zNear zFar = 
    ((scx:. 0 :. 0 :.plx:.()):.
     ( 0 :.scy:. 0 :.ply:.()):.
     ( 0 :. 0 :.pr1:.pr2:.()):.
     ( 0 :. 0 :.neg:. 0 :.()):.
     ())
    where
        arx:.ary:.() = letterbox size
        scx = arx * scale 
        scy = ary * scale
        pr1 = (zNear + zFar) / (zNear - zFar)
        pr2 = (2 * zNear * zFar) / (zNear - zFar)
        neg = negate 1 / plz

-- CAMERA SPACE -> CLIP SPACE
vs_matrixpersp :: Matrixv -> Posv -> Posv
vs_matrixpersp projMat camPos = multmv projMat camPos

-- Animate the x and y components of pos according to time
vs_morepowertotheshaders :: Float_v -> Float_v -> Posv -> Posv
vs_morepowertotheshaders duration time pos = pos + offset
    where
        offset = (0.5 * Main.cycle cos duration time) :.
                 (0.5 * Main.cycle sin duration time) :.
                 0.0 :. 0.0 :. ()

-- Output a value [0, 1) where n traces a cycle each max through  0..1..0..-1..repeat
cycle f max n = f $ full * curr
    where
        full = pi * 2 / max -- full is a full cycle scaled to max
        curr = mod' n max   -- curr is in [0, max)

--mix4 (x1:.y1:.z1:.w1:.()) (x2:.y2:.z2:.w2:.()) lerp =
--    mix x1 x2 lerp :. mix y1 y2 lerp :. mix z1 z2 lerp :. mix w1 w2 lerp :. ()
--
--fs_timecolor :: Float_f -> Float_f -> Float4f
--fs_timecolor duration time = mix4 col1 col2 sinlerp
--    where
--        dur = 2 * duration
--        longpoplerp = mod' time duration / duration
--        sinlerp = sin (pi * longpoplerp)
--        col1 = 1.0 :. 0.2 :. 1.0 :. 1.0 :. ()
--        col2 = 0.2 :. 1.0 :. 0.2 :. 1.0 :. ()
--
---- Output a vector lerp'd according to the y-position of the fragment
--fs_fragpos :: Float4f -> Float4f
--fs_fragpos pos = mix4 dncol upcol lerp
--    where
--        lerp = (1 + get n1 pos) / 2.0
--        upcol = 0.2 :. 1.0 :. 1.0 :. 1.0 :. ()
--        dncol = 1.0 :. 1.0 :. 0.2 :. 1.0 :. ()
--
---- Apply a scale to one component of a vector
--scaleComponent :: (Nat n, Num a, Access n a v) => n -> a -> v -> v
--scaleComponent n a v = set n (get n v * a) v

-- Scale y by the ratio of width:height
letterbox :: Fractional n => Vec2 n -> Vec2 n
letterbox (w:.h:.()) = 1.0 :. (w / h) :. ()

-- Scale x by the ratio of height:width
pillarbox :: Fractional n => Vec2 n -> Vec2 n
pillarbox (w:.h:.()) = (h / w) :. 1.0 :. ()

-- eof
