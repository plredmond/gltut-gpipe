module Lib.Mesh
( Mesh ()
, mkMesh
) where

-- hackage
import Text.XML.Light
import Text.Printf (printf)

-- local
import Lib.Mesh.XML
import Lib.Mesh.MAttr
--import Lib.Mesh.MVAO

-- index specifies which parts of an arbitrary attribute to use and some extra props
-- render thunk renders with all attributes
-- render :: String -> ... renders with named vao that specifies a limited set of attributes

-- Mesh ----------------------------------------------------------------------

test :: IO ()
test = do
    s <- readFile "TestFile.xml"
    let m = either error id $ readMesh s
    print m

-- Mesh --

data Mesh = Mesh { mAttr :: [MAttr]
                 --, mVAO :: [MVAO]
                 } deriving (Show, Read, Eq)

readMesh :: String -> Either String Mesh
readMesh s = do
    let xml = parseXML s
    el <- lookupMeshElement xml
    m <- mkMesh el
    return m

mkMesh :: Element -> Either String Mesh
mkMesh el = do
    mattrs      <- mapM mkMAttr     $ rawFindChildren "attribute" el
--    mvaos       <- mapM mkVAO       $ rawFindChildren "vao"       el
--    mprimitives <- mapM mkPrimitive $ rawFindChildren "indices"   el
    mattrsLengthEq mattrs
    return $ Mesh mattrs
    where
        mattrsLengthEq :: [MAttr] -> Either String ()
        mattrsLengthEq []             = Left "Mesh must contain at least one Attribute element"
        mattrsLengthEq as | b         = Right ()
                          | otherwise = Left $ printf "Attribute arrays must each contain %d*size values" n
            where
                n = lengthMAttr . head $ as
                b = ((n ==) . lengthMAttr) `all` as

lookupMeshElement :: [Content] -> Either String Element
lookupMeshElement cs = case rawFilterByName "mesh" . onlyElems $ cs of
    [e] -> Right e
    []  -> Left "Found no mesh element"
    _   -> Left "Found too many mesh elements"

-- eof
