-- qualified

import qualified Control.Monad as Monad
import qualified Data.Traversable as T
--import qualified Data.Monoid as Monoid
import qualified Text.Printf as Pf
import qualified System.Environment as Env
import qualified System.FilePath as Path
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.GLTut.Framework as Framework
import qualified Graphics.GLTut.MatrixStack as MS
import qualified Paths_gltut_tut07 as Paths

-- mixed

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.IORef as R
import Data.IORef (IORef)

import qualified Graphics.GLTut.RenderState as RenderState
import Graphics.GLTut.RenderState (RenderState)

import qualified Graphics.GLTut.Mesh as Mesh
import Graphics.GLTut.Mesh (Mesh)

import qualified Graphics.GLTut.Tut07.Angle as Angle
import Graphics.GLTut.Tut07.Angle (Angle)

import qualified Graphics.GLTut.Tut07.SphereCoord as SphereCoord
import Graphics.GLTut.Tut07.SphereCoord (SphereCoord)

-- unqualified

import Prelude as P
import Data.Vec as V hiding (Map)
import Graphics.GPipe


-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
-- Jason's tutorial converts spherical coordinates to cartesian coordinates
-- such that the zenith direction is swapped with the 90 degree meridian when
-- compared with my implementation. He also stores inclination on the range
-- [-90,90] with -90 being equal to the zenith direction.

-- Swap zenith and 90 degree meridian.
toCartesian :: Floating a => SphereCoord a -> Vec3 a
toCartesian = (\(x, y, z) -> x:.z:.y:.()) . SphereCoord.toCartesian

-- Shift inclination from [-90, 90] to [0, 180].
sphereCoord :: (Real a, Floating a) => a -> Angle a -> Angle a -> SphereCoord a
sphereCoord r aA_ iA_ = SphereCoord.sphereCoord r aA_ iA
    where
        a = Angle.toRad aA_
        i = Angle.toRad iA_
        iA = Angle.fromRad $ i + pi/2

-- Dynamic Information --- --- --- --- --- --- --- --- --- --- --- --- --- ---

data State = State
    { drawLookatPoint :: Bool
    , camTarget       :: Vec3 Float
    , sphereCamRelPos :: SphereCoord Float
    } deriving(Show)
        
initialState :: State
initialState = State
    { drawLookatPoint = False
    , camTarget       = 0:.0.4:.0:.()
    , sphereCamRelPos = sphereCoord 150 (Angle.fromDeg 67.5) (Angle.fromDeg $ -46)
    }

-- Static Information --- --- --- --- --- --- --- --- --- --- --- --- --- ---

meshFiles :: Map String String
meshFiles = Map.fromList [ ("cone", "UnitConeTint.xml")
                         , ("cylinder", "UnitCylinderTint.xml")
                         , ("cubeTint", "UnitCubeTint.xml")
                         , ("cubeColor", "UnitCubeColor.xml")
                         , ("plane", "UnitPlane.xml")
                         ]

data Tree a = Tree { xPos :: a
                   , zPos :: a
                   , trunkHeight :: a
                   , coneHeight :: a
                   } deriving (Show)

data Program a = UniformColor {baseColor :: Vec4 a}
               | UniformColorTint {baseColor :: Vec4 a}
               deriving (Show)

data Component a = Component { mesh :: String
                             , meshVAO :: Maybe String
                             , transformations :: [MS.Transformation a]
                             , program :: Program a
                             }
                             deriving (Show)

template :: Component Float
template = Component { mesh = undefined
                     , meshVAO = Nothing
                     , transformations = []
                     , program = UniformColor {baseColor = 1:.0:.0.4:.1:.()}
                     }

static_scene :: [Component Float]
static_scene =
    [ template { mesh = "plane"
               , transformations = [MS.ScaleAll $ 100:.1:.100:.()]
               , program = UniformColor {baseColor = 0.302:.0.416:.0.0589:.1.0:.()}
               }
    -- Continue with line 507 of the cpp, "Draw the Building"
    ]

treeComponents :: (Floating a) => Tree a -> [Component a]
treeComponents (Tree x z trunk cone) =
    [ template { mesh = "cylinder"
               , transformations = both
               , program = UniformColorTint {baseColor = 0.694:.0.4:.0.106:.1.0:.()}
               }
    , template { mesh = "cone"
               , transformations = both ++
                                   [ MS.Translate MS.Y trunk
                                   , MS.ScaleAll $ 3:.cone:.3:.()
                                   ]
               , program = UniformColorTint {baseColor = 0:.1:.0:.1:.()}
               }
    ]
    where
        both = [ MS.TranslateAll $ x:.0:.z:.()
               , MS.Scale MS.Y trunk
               , MS.Translate MS.Y 0.5
               ]

type StaticState = [Component Float]

-- Main --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

usage :: IO [String]
usage = do
    argv <- Env.getArgs
    Monad.unless (P.length argv == 1) $ do
        n <- Env.getProgName
        Pf.printf "usage: ./%s [path to \"Tut 07 World in Motion/data\"]\n" n
        fail "argument error"
    return argv

loadTrees :: String -> IO [Tree Float]
loadTrees path = do
    s <- readFile path
    return . P.map (\[a,b,c,d] -> Tree a b c d)
           . P.map (P.map (read :: String -> Float))
           . P.filter (not . null)
           . P.map words
           . lines
           $ s

loadMeshes :: String -> IO (Map String Mesh)
loadMeshes path = T.forM meshFiles $ \xml -> do
    s <- readFile $ Path.joinPath [path, xml]
    return $ either (\err -> error $ Pf.printf "%s: %s" xml err) id (Mesh.readMesh s)

main :: IO ()
main = do
    [meshPath] <- usage
    trees <- Paths.getDataFileName "forest.vec4" >>= loadTrees 
    meshm <- loadMeshes meshPath
    -- build the rest of the scene
    -- convert meshes to streams and put them in the scene
    print (Map.size meshm)
    print (Map.lookup "plane" meshm)
    -- let _ = prim -- try to def the first mesh as a gpu type after primtogpu or smth
    -- build the scene out of meshes loaded on the gpu once?

    -- enter common mainloop
    ref <- R.newIORef initialState
    Framework.main (keyboard ref)
                   (displayIO ref (undefined, trees))
                   initialize

-- Set up the window.
initialize :: GLUT.Window -> IO ()
initialize w = GLUT.idleCallback GLUT.$= (Just . GLUT.postRedisplay . Just $ w)

-- Handle keyboard events.
keyboard :: IORef State -> Char -> GLUT.Position -> IO ()
keyboard _ '\ESC' _ = GLUT.leaveMainLoop
keyboard r 'a'    _ = R.modifyIORef r id >> blurt r
keyboard _ _      _ = return ()

-- displayIO :: IORef State -> StaticState -> Vec2 Int -> IO (FrameBuffer RGBFormat DepthFormat ())
displayIO ref sst size = do
    rst <- RenderState.new size
    gst <- R.readIORef ref
--  return $ display rst gst sst
    return $ newFrameBufferColorDepth (RGB $ vec 0) 1


--display :: RenderState Float -> State -> StaticState -> FrameBuffer RGBFormat DepthFormat ()
--display rst gst sst = draw fragments cleared
--    where
--        draw = paintColorRastDepth Lequal True NoBlending (RGB $ vec True)
--        cleared = newFrameBufferColorDepth (RGB $ vec 0) 1
--        fragments = mconcat
--                  . P.map (mkFragments world2clip)
--                  . ...
--                  $ let (meshes, trees) = sst
--                    in makeScene meshes, trees
--        tgt = 
--        cam = 
--
--        camTarget       = fromJust $ fromDynamic (gst ! "camTarget")       :: Vec3 Float
--        sphereCamRelPos = fromJust $ fromDynamic (gst ! "sphereCamRelPos") :: SphereCoords
--
--        camPos = let
--
--        world2cam = let up = 0:.1:.0:.()
--                        tgt = 0:.0.05:.0:.()
--                        in multmm (transpose $ rotationLookAt up cam tgt) (translation $ -cam)
--                        -- multmm (translation $ -cam) (rotationLookAt up cam tgt)
--
--        cam2clip = let zNear = 1
--                       zFar = 1000
--                       fovDeg = 45
--                       in perspective zNear zFar (deg2rad fovDeg) (rsAspectRatio rst)
--        world2clip = toGPU $ multmm cam2clip world2cam

-- -- r sin 
-- 
--resolveCamPosition :: Vec3 Float -> SphereCoords -> Vec3 Float
--resolveCamPosition tgt relpos = ...
--    r = scdRadius sphereCamRelPos 
--    p = deg2rad $ scdAzimuth sphereCamRelPos 
--    t = deg2rad $ 90 + scdInclination sphereCamRelPos 
--    toCam = (sin t * cos p):.(cos t):.(sin t * sin p):.()
--    in (toCam * r) + camTarget
-- 
-- mkFragments :: Mat44 (Vertex Float) -> (Mat44 Float, PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float))) -> FragmentStream (Color RGBFormat (Fragment Float))
-- mkFragments cam2clip (model2cam, mesh) = fmap fs
--                                        $ rasterizeBack
--                                        $ fmap (vs (toGPU model2cam, cam2clip))
--                                        $ fmap vs0 mesh
-- 
-- -- Make the stream use Vec4 for pos and col.
-- vs0 :: (Vec3 (Vertex Float), Vec3 (Vertex Float)) -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
-- vs0 (pos3, rgb) = (homPoint pos3, snoc rgb 1)
-- 
-- -- Offset the position. Perform projection using the provided matrix.
-- vs :: (Mat44 (Vertex Float), Mat44 (Vertex Float)) -> (Vec4 (Vertex Float), Vec4 (Vertex Float)) -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
-- vs (modelToCameraMatrix, cameraToClipMatrix) (pos, col) = (clipPos, col)
--     where
--         camPos = multmv modelToCameraMatrix pos
--         clipPos = multmv cameraToClipMatrix camPos
-- 
-- fs :: Vec4 (Fragment Float) -> Color RGBFormat (Fragment Float)
-- fs col = RGB $ V.take n3 col

blurt :: IORef State -> IO ()
blurt r = do
    state <- R.readIORef r
    Pf.printf "blurt: %s\n" $ show state
    return () -- when printf is the last line, something screwy happens

-- eof
