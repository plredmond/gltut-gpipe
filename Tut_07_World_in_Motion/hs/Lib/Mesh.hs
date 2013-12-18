module Lib.Mesh
( Mesh ()
, mkMesh
) where

-- hackage
import Graphics.GPipe hiding (transpose)
import Text.XML.Light hiding (Line)
import qualified Data.Vec as V hiding (transpose)
import Data.Function (on)
import Data.List (nub, (\\), sortBy, find)
import Data.Maybe (listToMaybe)
import Data.Monoid (mconcat)
import Text.Printf (printf)

-- local
import Lib.Mesh.Util
import Lib.Mesh.XML
import Lib.Mesh.MAttr
import Lib.Mesh.MVao
import Lib.Mesh.MPrim

test :: IO Mesh
test = do
    s <- readFile "TestFile.xml"
    let m = either error id $ readMesh s
    return m

-- Mesh -----------------------------------------------------------------------

data Mesh = Mesh { mAttr :: [MAttr]
                 , mVao :: [MVao]
                 , mPrim :: [MPrim]
                 } deriving (Show, Read, Eq)

readMesh :: String -> Either String Mesh
readMesh s = do
    let xml = parseXML s
    el <- lookupMeshElement xml
    m <- mkMesh el
    return m

mkMesh :: Element -> Either String Mesh
mkMesh el = do
    mattrs <- mapM mkMAttr $ rawFindChildren "attribute" el
    mvaos  <- mapM mkMVao  $ rawFindChildren "vao"       el
    mprims <- mapM mkMPrim $ rawFindChildren "indices"   el
                          ++ rawFindChildren "arrays"    el
    let maIndexes = map (fromMAttrIndex . maIndex) mattrs
        maIndexDups = maIndexes \\ nub maIndexes -- xs - unique(xs)
        mvIndexes' = map fromMAttrIndex . concatMap mvIndexes $ mvaos
        mvInvalidIndexes = nub mvIndexes' \\ nub maIndexes -- unique(xs) minus unique(ys)
    guard (not . null $ mattrs)
          "Mesh element must contain at least one Attribute element"
    guard (null maIndexDups)
          $ printf "Attribute index=\"%d\" appears multiple times" (head maIndexDups :: Integer)
    guardAttrLen mattrs
    guard (null mvInvalidIndexes)
          $ printf "VAO mentioned nonexistant Attribute index=\"%d\"" (head mvInvalidIndexes :: Integer)
    return $ Mesh mattrs mvaos mprims

lookupMeshElement :: [Content] -> Either String Element
lookupMeshElement cs = case rawFilterByName "mesh" . onlyElems $ cs of
    [e] -> Right e
    []  -> Left "Found no mesh element"
    _   -> Left "Found too many mesh elements"

guardAttrLen as = guard ((1 ==) . length . nub . map lengthMAttr $ as)
    $ printf "Attribute arrays must all contain %d*size values" (lengthMAttr . head $ as)

-- meshToGPU :: (VertexInput a) => Mesh -> Maybe String -- Name of VAO, otherwise use all attrs
--     -> Either String ( PrimitiveStream Triangle a -- One stream for each of these types
--                      , PrimitiveStream Line     a
--                      , PrimitiveStream Point    a
--                      )
-- meshToGPU m@(Mesh as vs ps) s = do
--     argss <- prepAttrs m s
--     -- Extract loaders for the primitives in this mesh and group them by type.
--     let (tfs, lfs, pfs) = partitionLoaders . map extractMPrim $ ps
--     -- Apply attribute data to the loaders and concat the PrimitiveStreams.
--     let f = mconcat . fmap ($ argss) :: [[CPU a] -> PrimitiveStream p a] -> PrimitiveStream p a
--     return (f tfs, f lfs, f pfs)

-- Extract attribute data for the specified VAO and rotate it into Vecs.

-- prepAttrs :: ( V.VecList attr attrv        -- [MAttr] to Vec of same
--              , Fractional num              -- see extractMAttr
--              , V.VecList num arg           -- see extractMAttr
--              , V.Map attr args attrv argsv -- extract each attr to a list in the vector
--              , V.Map args args argsv argsv -- see transpose
--              , V.Map args arg  argsv argv  -- see transpose
--              , V.Head argsv args           -- see transpose
--              ) => Mesh -> Maybe String -> Either String [argv]
prepAttrs m s = do
    attrs <- meshAttrs m s
    guardAttrLen attrs
    return (flip transpose []              -- get [Vec1-16 (Vec1-4 a)]
           . V.map extractMAttr            -- get (Vec1-16 [Vec1-4 a])
           . V.fromList                    -- get (Vec1-16 MAttr) of same length
           . sortBy (compare `on` maIndex) -- get sorted [MAttr] of same length
           $ attrs)                        -- unsorted [MAttr] with length 16 or fewer

-- transpose :: ( V.Map args args argsv argsv -- tail of each list in the vector
--              , V.Map args arg  argsv argv  -- head of each list in the vector
--              , V.Head argsv args           -- head of the vector
--              ) => argsv -> [arg] -> [arg]  -- :: Vec1-16 [a] -> [] -> [Vec1-16 a]
transpose xsv xvs
    | null $ V.head xsv = xvs
    | otherwise = let curr = V.map head xsv
                      xsv' = V.map tail xsv
                  in transpose xsv' (curr:xvs)

-- Fetch the list of attributes associated with the named VAO.
meshAttrs :: Mesh -> Maybe String -> Either String [MAttr]
meshAttrs m@(Mesh as _ _) s = do
    is <- meshIndexes m s
    as' <- mapM get is
    return as'
    where
        get i = m2e (printf "No Attr with %s" . show $ i)
                    (let p = (i ==) . maIndex
                      in find p as)

-- Fetch the list of attribute indexes associated with the named VAO.
meshIndexes :: Mesh -> Maybe String -> Either String [MAttrIndex]
meshIndexes (Mesh as _ _) Nothing = return . map maIndex $ as
meshIndexes (Mesh _ vs _) (Just s) = do
    v <- m2e (printf "No such VAO \"%s\"" s)
             (let p = (s ==) . mvName
               in find p vs)
    return . mvIndexes $ v

-- eof
