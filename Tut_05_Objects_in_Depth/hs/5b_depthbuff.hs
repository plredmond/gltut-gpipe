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
    let scene = [ meSetOffset projPlane (0:.0:.(-3):.())
                , m1
                , meSetOffset m2 (0:.0:.(-1):.()) ]
    x <- newIORef ((X, Y), scene)
    newWindow n (vec 8) (512:.512:.()) (eachFrameIO x) (prepWindow x)
    GLUT.mainLoop

-- Set up the window before entering the GLUT mainloop
-- IO: Set GLUT.idleCallback
prepWindow :: World -> GLUT.Window -> IO ()
prepWindow x w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)
    GLUT.keyboardMouseCallback GLUT.$= Just (onKeyMouse x)
    -- depthClamp: How should zNear & zFar be enforced?
    --     Enabled: Clamp the depth of fragments.
    --     Disabled: Clip triangles.
    GLUT.depthClamp GLUT.$= GLUT.Enabled
    -- x <- GLUT.get GLUT.depthClamp
    -- print $ "depthClamp: " ++ show x

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

--eachFrame scene rs = cleared
eachFrame scene rs = paintFStreams      -- fold frags onto a canvas
--                 $ postNorm    --hook4-- tweak first-in-scene's color
                   $ normFStreams       -- convert frags to a ColorFormat c => FragmentStream (Color c (Fragment Float))
                   $ postRast p  --hook3--
                   $ rastMeshes True False   -- rasterize the things to frags
                        -- don't include Pos in frags
                        -- don't manually set depth information
--                 $ postProj    --hook2--
                   $ projMeshes matrix  -- project from camera-space into clip-space
                   $ preProj     --hook1-- apply mesh offsets to objects
                   $ scene              -- some functor over things to render
    where 
        p = toGPU $ Util.cycle cos 1 $ rsSeconds rs
        matrix = toGPU $ perspective 1 3 (degToRad 90) (rsAspectRatio rs)

preProj xs = fmap preProjOne xs
-- send mesh offset toGPU and apply offset to each mesh position
preProjOne (MeshPos    mc ps) = MeshPos    mc $ fmap (\ p     ->  p + (snoc (toGPU $ mcOffset mc) 0)    ) ps
preProjOne (MeshPosCol mc ps) = MeshPosCol mc $ fmap (\(p, c) -> (p + (snoc (toGPU $ mcOffset mc) 0), c)) ps

projMeshes mat xs = fmap (projOneMesh mat) xs
-- apply on-GPU camera->clip matrix to each mesh position
projOneMesh m (MeshPos    mc ps) = MeshPos    mc $ fmap (\ p     ->  multmv m p    ) ps
projOneMesh m (MeshPosCol mc ps) = MeshPosCol mc $ fmap (\(p, c) -> (multmv m p, c)) ps

rastMeshes b0 b1 xs = fmap (rastOneMesh b0 b1) xs
rastOneMesh _ False (MeshPos    mc ps) = FragUnit   (toGPU $ mcBaseCol mc) $ rastFn mc $ fmap (\ p     -> (p,  ()          )) ps
rastOneMesh p False (MeshPosCol mc ps) = case p of
                                False -> FragCol                           $ rastFn mc $ fmap (\(p, c) -> (p,  c           )) ps
                                True  -> FragPosCol                        $ rastFn mc $ fmap (\(p, c) -> (p, (p, c)       )) ps
rastOneMesh _ True  (MeshPos    mc ps) = FragDep    (toGPU $ mcBaseCol mc) $ rastFn mc $ fmap (\ p     -> (p,     get n2 p )) ps
rastOneMesh _ True  (MeshPosCol mc ps) = FragColDep                        $ rastFn mc $ fmap (\(p, c) -> (p, (c, get n2 p))) ps

rastFn mc = case (mcWinding mc) of
    Clockwise        -> fmap withBool . rasterizeFront
    CounterClockwise -> fmap withBool . rasterizeBack
    Both             -> rasterizeFrontAndBack
    where
        b = toGPU True :: BoolF
        withBool = \x -> (b, x)

postRast _ [] = []
postRast v (x:xs) = pulseOne v x : xs
-- pulse the color of an object
pulseOne v (FragUnit   c fs) = FragUnit                                  (pulse  v c)      fs
pulseOne v (FragCol      fs) = FragCol    $ fmap (\(b,  c    ) -> (b,     pulse  v c    )) fs
pulseOne v (FragPosCol   fs) = FragPosCol $ fmap (\(b, (p, c)) -> (b, (p, pulse' c p   ))) fs
    where pulse' c' (x:.y:._:._:.()) = let magic = sin $ (x*10) * (y*10) in pulse (v + magic) c'
pulseOne v (FragDep    c fs) = FragDep                                   (pulse  v c)      fs
pulseOne v (FragColDep   fs) = FragColDep $ fmap (\(b, (c, d)) -> (b, (   pulse  v c, d))) fs
--
pulse v c = snoc (V.map tame $ rgb + adjustment) a
    where
        rgb = V.take n3 c
        a = V.last c
        adjustment = vec $ v / 9
        tame n = clamp n 0 1

normFStreams xs = fmap normOneFStream xs
normOneFStream (FragUnit   c fs) = NFragRGBAf   $ fmap (\(f,  ()   ) ->  makeColor c    ) fs
normOneFStream (FragCol      fs) = NFragRGBAf   $ fmap (\(f,  c    ) ->  makeColor c    ) fs
normOneFStream (FragPosCol   fs) = NFragRGBAf   $ fmap (\(f, (p, c)) ->  makeColor c    ) fs
normOneFStream (FragDep    c fs) = NFragRGBAfDf $ fmap (\(f,     d ) -> (makeColor c, d)) fs
normOneFStream (FragColDep   fs) = NFragRGBAfDf $ fmap (\(f, (c, d)) -> (makeColor c, d)) fs

-- Output the given vector as a color
makeColor :: ColF -> Color RGBAFormat FloatF
makeColor rgba = RGBA (V.take n3 rgba) (V.last rgba)

paintFStreams xs = P.foldl paintOneFStream cleared xs
paintOneFStream fb (NFragRGBAf   x) = paintC  x fb
paintOneFStream fb (NFragRGBAfDf x) = paintCD x fb
-- paint
paintC  = paintColorRastDepth Lequal True NoBlending (RGBA (vec True) True)
paintCD = paintColorDepth     Lequal True NoBlending (RGBA (vec True) True)

-- A solid-color framebuffer
cleared :: FrameBuffer RGBAFormat DepthFormat ()
cleared = newFrameBufferColorDepth c d
    where
        c = RGBA (vec 0) 0
        d = 1

-- Apply a function to pos without touching the rest of the tuple
applyPos :: (PosV -> PosV) -> (PosV, a) -> (PosV, a)
applyPos fn (pos, aux) = (fn pos, aux)

-- eof
