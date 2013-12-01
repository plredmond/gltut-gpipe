module Lib.Mesh
( Mesh ()
, mkMesh
) where

-- hackage
import Text.XML.Light
import Text.Printf (printf)
import Data.List (nub, (\\))
import Data.Maybe (listToMaybe)

-- local
import Lib.Mesh.XML
import Lib.Mesh.MAttr
import Lib.Mesh.MVao
import Lib.Mesh.MPrim

-- index specifies which parts of an arbitrary attribute to use and some extra props
-- render thunk renders with all attributes
-- render :: String -> ... renders with named VAO that specifies a limited set of attributes

-- Mesh ----------------------------------------------------------------------

test :: IO ()
test = do
    s <- readFile "TestFile.xml"
    let m = either error id $ readMesh s
    print m

-- Mesh --

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
    mattrsUniqInd mattrs
    mattrsLengthEq mattrs
    mvaosRefAttrs mvaos mattrs
    return $ Mesh mattrs mvaos mprims
    where
        -- Disallow attributes from sharing the same index
        mattrsUniqInd :: [MAttr] -> Either String ()
        mattrsUniqInd as = maybe
            (Right ())
            (Left . (printf "Attribute index=\"%s\" appears multiple times") . show . \(MAttrIndex i) -> i)
            (let inds = map maIndex as
                 dups = nub $ inds \\ nub inds
              in listToMaybe dups)
        -- Disallow a mesh with no attributes
        -- Disallow attributes with a value count not divisible by attribute size
        mattrsLengthEq :: [MAttr] -> Either String ()
        mattrsLengthEq []             = Left "Mesh must contain at least one Attribute element"
        mattrsLengthEq as | b         = Right ()
                          | otherwise = Left $ printf "Attribute arrays must each contain %d*size values" n
            where
                n = lengthMAttr . head $ as
                b = ((n ==) . lengthMAttr) `all` as
        -- Disallow VAOs which reference attributes that don't exist
        mvaosRefAttrs :: [MVao] -> [MAttr] -> Either String ()
        mvaosRefAttrs vs as = maybe
            (Right ())
            (Left . (printf "VAO mentioned nonexistant Attribute index=\"%s\"") . show . \(MAttrIndex i) -> i)
            (let ais = map maIndex as
                 vis = concatMap mvIndexes vs
                 unref = nub vis \\ nub ais
              in listToMaybe unref)
                
lookupMeshElement :: [Content] -> Either String Element
lookupMeshElement cs = case rawFilterByName "mesh" . onlyElems $ cs of
    [e] -> Right e
    []  -> Left "Found no mesh element"
    _   -> Left "Found too many mesh elements"

-- eof
