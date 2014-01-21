module Graphics.GLTut.Mesh
( Mesh()
, readMesh
, mkMesh
, meshToGPU
) where

-- hackage
import Text.XML.Light hiding (Line)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.HashMap.Lazy as HashMap
import Graphics.GPipe (PrimitiveStream, Triangle(), Line(), Point(), Vertex())
import Data.Traversable (forM)
import Data.IntMap.Lazy (IntMap)
import Data.HashMap.Lazy (HashMap)
import Data.List ((\\))
import Text.Printf (printf)

-- local
import Graphics.GLTut.Tri
import Graphics.GLTut.Mesh.Util
import Graphics.GLTut.Mesh.XML
import Graphics.GLTut.Mesh.Attribute
import Graphics.GLTut.Mesh.Primitive
import qualified Graphics.GLTut.Mesh.PrimToGPU as PTG

-- Mesh -----------------------------------------------------------------------

type Attrm = IntMap PTG.Attr
type VAOm = HashMap String [Int]
data Mesh = Mesh Attrm VAOm [Primitive] deriving (Show, Eq)

readMesh :: String -> Either String Mesh
readMesh s = do
    el <- lookupMeshElement (parseXML s)
    m <- mkMesh el
    return m

lookupMeshElement :: [Content] -> Either String Element
lookupMeshElement cs = case rawFilterByName "mesh" . onlyElems $ cs of
    [e] -> Right e
    []  -> Left "Found no mesh element"
    _   -> Left "Found too many mesh elements"

mkMesh :: Element -> Either String Mesh
mkMesh el = do
    attrm <- allAttrm         $ rawFindChildren "attribute" el
    vaom  <- allVAOm          $ rawFindChildren "vao"       el
    prims <- mapM mkPrimitive $ rawFindChildren "indices"   el
                             ++ rawFindChildren "arrays"    el
    guard (not $ IntMap.null attrm) "There must be at least one Attribute element"
    guard (not $ null prims) "There must be at least one Indices or Arrays element"
    return $ Mesh attrm vaom prims

-- Convert Attribute elements to a map Int->Attr if their indexes are unique.
allAttrm :: [Element] -> Either String Attrm
allAttrm els = do
    aa <- mapM mkAttribute els
    let am = IntMap.fromList aa
        repeats = map fst aa \\ IntMap.keys am
    guard (null repeats)
          $ printf "Attribute indexes must be unique. These repeat: %s" (unwords $ map show repeats)
    return am

-- Convert VAO elements to a map String->Int->Attr if their names are unique.
allVAOm :: [Element] -> Either String VAOm
allVAOm els = do
    va <- mapM mkVAO els
    let vm = HashMap.fromList va
        repeats = map fst va \\ HashMap.keys vm
    guard (null repeats)
          $ printf "VAO names must be unique. These repeat: %s" (unwords repeats)
    return vm

meshToGPU :: Mesh -> Maybe String -> Either String [Tri (PrimitiveStream Triangle (IntMap [Vertex Float]))
                                                        (PrimitiveStream Line     (IntMap [Vertex Float]))
                                                        (PrimitiveStream Point    (IntMap [Vertex Float]))]
meshToGPU (Mesh allattrm vaom prims) name = do
    -- select the attributes indicated by the vao
    fullattrm <- case name of Nothing -> return allattrm
                              Just s -> selectAttrm allattrm vaom s
    -- for each primitive...
    forM prims $ \(cmd, sel) -> do
        -- extract the data indexes and slice the attribute data
        let (indexes, attrm) = case sel of Left (start, count) -> ([], IntMap.map (PTG.attrSlice start count) fullattrm)
                                           Right indexes -> (indexes, fullattrm)
            -- Curry args onto primToGPU and apply a constructor inside the resultant Either.
            f c p = fmap c $ PTG.primToGPU p indexes attrm
        -- Apply the primitive command to make the stream.
        tri (f Jyan) (f Ken) (f Po) cmd

-- Select attributes based on the set up attribute indexes associated with the named VAO.
selectAttrm :: Attrm -> VAOm -> String -> Either String Attrm
selectAttrm attrm vaom name = do
    indexes <- lookupEitherHM vaom (printf "VAO \"%s\" doesn't exist" name) name
    let indexm = IntMap.fromList $ zip indexes (repeat ())
        invalids = IntMap.difference indexm attrm
    guard (IntMap.null invalids)
          $ printf "VAO \"%s\" references nonexistent Attribute indexes: %s" name (unwords . map show . IntMap.keys $ invalids)
    return (IntMap.intersection attrm indexm)

-- eof
