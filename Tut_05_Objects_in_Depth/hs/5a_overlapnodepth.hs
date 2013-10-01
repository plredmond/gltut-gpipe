import System.Environment (getProgName)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec as V
import Prelude as P
import Data.IORef
-- local
import Util
import Mesh
import Load

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Main routines

data Axis = X | Y | Z deriving Show
type World = IORef ((Axis, Axis), [Mesh])

indexes =
    [ 0, 2, 1
    , 3, 2, 0
    , 4, 5, 6
    , 6, 7, 4
    , 8, 9, 10
    , 11, 13, 12
    , 14, 16, 15
    , 17, 16, 14 ]

main :: IO ()
main = do
    GLUT.getArgsAndInitialize
    n <- getProgName
    m1 <- loadMeshPosCol "obj1.vec4" indexes
    m2 <- loadMeshPosCol "obj2.vec4" indexes
    let scene = [m1, meSetOffset m2 (0:.0:.(-1):.())]
    x <- newIORef ((X, Y), scene)
    newWindow n (vec 8) (512:.512:.()) (eachFrameIO x) (prepWindow x)
    GLUT.mainLoop
--    where
--        tweak = \m -> meSetOffset m (0:.0:.(-2):.())

-- Set up the window before entering the GLUT mainloop
-- IO: Set GLUT.idleCallback
prepWindow :: World -> GLUT.Window -> IO ()
prepWindow x w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)
    GLUT.keyboardMouseCallback GLUT.$= Just (onKeyMouse x)

onKeyMouse :: World -> GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
onKeyMouse x k GLUT.Down _ _ = do
    case k of
        GLUT.SpecialKey GLUT.KeyRight -> modifyIORef x $ moveSceneHead fst   0.1
        GLUT.SpecialKey GLUT.KeyLeft  -> modifyIORef x $ moveSceneHead fst (-0.1)
        GLUT.SpecialKey GLUT.KeyUp    -> modifyIORef x $ moveSceneHead snd   0.1
        GLUT.SpecialKey GLUT.KeyDown  -> modifyIORef x $ moveSceneHead snd (-0.1)
        GLUT.Char '\r'                -> modifyIORef x nextAxis
        GLUT.Char ' '                 -> modifyIORef x nextObject
        _                             -> print "No action"
    (axes, scene) <- readIORef x
    putStrLn $ "Key: " ++ show k
        ++ "; Axes: " ++ show axes
        ++ "; Loc: " ++ (show $ mcOffset $ meConfig $ P.head scene)
    where
        nextObject (axes, h:t) = (axes, t ++ [h])
        nextAxis ((a, Y), scene) = ((a, Z), scene)
        nextAxis ((a, Z), scene) = ((a, Y), scene)
        moveSceneHead f n (axes, scene) = let a = f axes
                                              h:t = scene
                                              scene' = addOffset h (zeroExcept a n):t
                                          in (axes, scene')
        -- moveSceneHead helpers
        addOffset m d = let p = mcOffset $ meConfig m
                        in meSetOffset m $ p + d
        zeroExcept a n = case a of
            X -> n:.0:.0:.()
            Y -> 0:.n:.0:.()
            Z -> 0:.0:.n:.()
onKeyMouse _ _ GLUT.Up _ _ = do
    return ()

-- Produce a framebuffer of animation, provided the size of the window
-- IO: Call makeRenderState
--eachFrameIO :: Scene ?? -> Vec2 Int -> IO (FrameBuffer RGBAFormat () ())
eachFrameIO x size = do
    rs <- makeRenderState size
    modifyIORef x $ id --rs -- give rs to the *real* update function
    (axes, scene) <- readIORef x
    return $ eachFrame scene rs

eachFrame scene rs = paintFStreams -- fold frags onto a canvas
                   $ pulse p
                   $ normFStreams -- convert frags to a ColorFormat c => FragmentStream (Color c (Fragment Float))
                   $ rastMeshes -- rasterize the things to frags
--                 $ postProj
                   $ projMeshes matrix
                   $ preProj
                   $ scene -- some functor over things to render
    where 
        p = toGPU $ Util.cycle cos 1 $ rsSeconds rs
        matrix = toGPU $ perspective 1 3 (degToRad 90) (rsAspectRatio rs)

preProj xs = fmap preProjOne xs
-- send mesh offset toGPU and apply offset to each mesh position
preProjOne (MeshPos    mc ps) = MeshPos    mc $ fmap (\ p     ->  p + (snoc (toGPU $ mcOffset mc) 0)    ) ps
preProjOne (MeshPosCol mc ps) = MeshPosCol mc $ fmap (\(p, x) -> (p + (snoc (toGPU $ mcOffset mc) 0), x)) ps

projMeshes mat xs = fmap (projOneMesh mat) xs
-- apply on-GPU camera->clip matrix to each mesh position
projOneMesh m (MeshPos    mc ps) = MeshPos    mc $ fmap (\ p     ->  multmv m p    ) ps
projOneMesh m (MeshPosCol mc ps) = MeshPosCol mc $ fmap (\(p, x) -> (multmv m p, x)) ps

rastMeshes xs = fmap rastOneMesh xs
rastOneMesh (MeshPos    mc ps) = FragUnit (toGPU $ mcBaseCol mc) $ fmap (\() -> (toGPU True, ())) $ rastFn mc $ fmap (\ p    -> (p,())) ps
rastOneMesh (MeshPosCol mc ps) = FragCol                         $ fmap (\c  -> (toGPU True, c )) $ rastFn mc ps

rastFn mc = case (mcWinding mc) of
    Clockwise        -> rasterizeFront
    CounterClockwise -> rasterizeBack
    Both             -> rasterizeBack --rasterizeFrontAndBack

normFStreams xs = fmap normOneFStream xs
normOneFStream (FragUnit v fs) = NFragRGBAf $ fmap (\_     -> makeColor v) fs
normOneFStream (FragCol    fs) = NFragRGBAf $ fmap (\(_,v) -> makeColor v) fs

-- Output the given vector as a color
makeColor :: ColF -> Color RGBAFormat FloatF
makeColor rgba = RGBA (V.take n3 rgba) (V.last rgba)

pulse _ [] = []
pulse p (x:xs) = pulseOne p x : xs
pulseOne p (NFragRGBAf fs) = NFragRGBAf $ fmap pulseCol fs
    where
        pulseCol (RGBA rgb a) = RGBA (V.map (tame . tweak) rgb) a
        tame n = clamp n 0 1
        tweak n = n + p / 10

paintFStreams xs = P.foldl paintOneFStream cleared xs
paintOneFStream fb (NFragRGBAf x) = drawOn x fb

-- Curry blending-mode and boolean color-mask onto paintColor
drawOn :: FragmentStream (Color RGBAFormat FloatF) -> FrameBuffer RGBAFormat () () -> FrameBuffer RGBAFormat () ()
drawOn = paintColor NoBlending (RGBA (vec True) True)

-- A solid-color framebuffer
cleared :: FrameBuffer RGBAFormat () ()
cleared = newFrameBufferColor $ RGBA (0:.0:.0:.()) 1.0

-- Apply a function to pos without touching the rest of the tuple
applyPos :: (PosV -> PosV) -> (PosV, a) -> (PosV, a)
applyPos fn (pos, aux) = (fn pos, aux)

---- Do the work of drawing each famebuffer of animation
--eachFrame :: (VertexOutput a) => PrimitiveStream p (PosV, a) -> RenderState -> FrameBuffer RGBAFormat () ()
--eachFrame m rs = drawOn frags cleared
--    where
--        pp = fmap (\p -> (p, 1:.1:.1:.1:.())) projPlane
--        matrix = let w:.h:.() = rsSiz rs
--                 in toGPU (perspective 1 3 (degToRad 47) (w / h))
--        frags = P.foldl mappend mempty
--            [ renderModel matrix (0.5:.0.5:.(-2):.()) m
--            , renderModel matrix (0.4:.0.6:.(-3):.()) m
--            , renderModel matrix (0  :.0  :.(-3):.()) pp ]
--
--
---- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
---- Primitive Streams
--
--renderModel :: (VertexOutput a) => Mat44 FloatV -> Vec3 FloatV -> PrimitiveStream p (PosV, a) -> FragmentStream (Color RGBAFormat FloatF)
--renderModel matrix offset m =
--    fmap makeColor
--    $ rasterizeFront -- only gives snd of tup to fragment shaders
--    $ fmap (applyPos $ \p -> multmv matrix p) -- camera->clip
----  $ fmap (applyPos $ v_morepowertotheshaders 5 time)
--    $ fmap (applyPos $ (\p -> p + (snoc offset 0)))
--    m


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Utilities

---- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
---- Fragment Shaders 
--
--
---- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
---- Vertex Shaders
--

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
---- Draw with GPU values
--drawFrame :: Stream (Posv, Colv) ->
--             Timv -> Sizv -> Float3v -> FrameBuffer RGBAFormat () ()
--drawFrame mdl time size projplane = draw fragments cleared
--    where
--        -- fragment stream after applying shaders
--        fragments = fmap fs_mkcol
--                  -- 0 nogreen; 5 green; 10 nogreen 
----                  $ fmap (\c -> c * (fs_timecolor 10 $ toGPU time))
--                  -- little red at top; little blue at bottom
----                  $ fmap (\(p, c) -> c * fs_fragpos p)
--                  $ rasterizeBack -- only gives snd of tup to fshdrs
----                  $ fmap (\(p, c) -> (p, (p, c)))
----                  $ fmap (vs_applypos $ letterbox size)
----                  $ fmap (vs_applypos $ (\p -> mat `multmv` p))
--                  $ fmap (vs_applypos $ vs_matrixpersp mat)
--                  $ fmap (vs_applypos $ vs_morepowertotheshaders 5 time)
--                  $ fmap (vs_applypos $ (\p -> p + offset))
--                  mdl
----        -- pop 0.1..2.0 over 20s
----        pop :: FloatV
----        pop = ((mod' time 20) + 1) * 0.1
--        -- mat
--        mat = vs_projmat projplane size 1.0 0.5 3.0

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--yy--  
--yy--  -- Apply a function to pos without touching the rest of the tuple
--yy--  v_applypos :: (Posv -> Posv) -> (Posv, a) -> (Posv, a)
--yy--  v_applypos fn (pos, aux) = (fn pos, aux)
--yy--  
--yy--  v_projmat :: Float3v -> Sizv -> FloatV -> FloatV -> FloatV -> Matrixv -- Mat44 FloatV
--yy--  v_projmat (plx:.ply:.plz:.()) size scale zNear zFar = 
--yy--      ((scx:. 0 :. 0 :.plx:.()):.
--yy--       ( 0 :.scy:. 0 :.ply:.()):.
--yy--       ( 0 :. 0 :.pr1:.pr2:.()):.
--yy--       ( 0 :. 0 :.neg:. 0 :.()):.
--yy--       ())
--yy--      where
--yy--          arx:.ary:.() = letterbox size
--yy--          scx = arx * scale 
--yy--          scy = ary * scale
--yy--          pr1 = (zNear + zFar) / (zNear - zFar)
--yy--          pr2 = (2 * zNear * zFar) / (zNear - zFar)
--yy--          neg = negate 1 / plz
--yy--  
--yy--  
--yy--  -- Animate the x and y components of pos according to time
--yy--  vs_morepowertotheshaders :: FloatV -> FloatV -> Posv -> Posv
--yy--  vs_morepowertotheshaders duration time pos = pos + offset
--yy--      where
--yy--          offset = (0.5 * Main.cycle cos duration time) :.
--yy--                   (0.5 * Main.cycle sin duration time) :.
--yy--                   0.0 :. 0.0 :. ()
--yy--  
--yy--  
--yy--  --mix4 (x1:.y1:.z1:.w1:.()) (x2:.y2:.z2:.w2:.()) lerp =
--yy--  --    mix x1 x2 lerp :. mix y1 y2 lerp :. mix z1 z2 lerp :. mix w1 w2 lerp :. ()
--yy--  --
--yy--  --fs_timecolor :: FloatF -> FloatF -> Float4f
--yy--  --fs_timecolor duration time = mix4 col1 col2 sinlerp
--yy--  --    where
--yy--  --        dur = 2 * duration
--yy--  --        longpoplerp = mod' time duration / duration
--yy--  --        sinlerp = sin (pi * longpoplerp)
--yy--  --        col1 = 1.0 :. 0.2 :. 1.0 :. 1.0 :. ()
--yy--  --        col2 = 0.2 :. 1.0 :. 0.2 :. 1.0 :. ()
--yy--  --
--yy--  ---- Output a vector lerp'd according to the y-position of the fragment
--yy--  --fs_fragpos :: Float4f -> Float4f
--yy--  --fs_fragpos pos = mix4 dncol upcol lerp
--yy--  --    where
--yy--  --        lerp = (1 + get n1 pos) / 2.0
--yy--  --        upcol = 0.2 :. 1.0 :. 1.0 :. 1.0 :. ()
--yy--  --        dncol = 1.0 :. 1.0 :. 0.2 :. 1.0 :. ()
--yy--  --
--yy--  ---- Apply a scale to one component of a vector
--yy--  --scaleComponent :: (Nat n, Num a, Access n a v) => n -> a -> v -> v
--yy--  --scaleComponent n a v = set n (get n v * a) v
--yy--  
--yy--  -- Scale y by the ratio of width:height
--yy--  letterbox :: Fractional n => Vec2 n -> Vec2 n
--yy--  letterbox (w:.h:.()) = 1.0 :. (w / h) :. ()
--yy--  
--yy--  -- Scale x by the ratio of height:width
--yy--  pillarbox :: Fractional n => Vec2 n -> Vec2 n
--yy--  pillarbox (w:.h:.()) = (h / w) :. 1.0 :. ()
--yy--  
--yy--  -- eof
