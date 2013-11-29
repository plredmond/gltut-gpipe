module Lib.Mesh.MVao where

-- hackage
import Text.XML.Light

-- local
import Lib.Mesh.MAttr
import Lib.Mesh.XML

-- A VAO specifies which sets of attributes to use with shaders.
-- This information is not saved explicitly in the C++ code, but in the GL state.
-- GPipe doesn't support VAOs to my knowledge. Here goes nothing.

-- MVao ----------------------------------------------------------------------

data MVao = MVao { mvName :: String
                 , mvIndexes :: [MAttrIndex]
                 } deriving (Show, Read, Eq)

mkMVao :: Element -> Either String MVao
mkMVao e = do
    n <- requireAttr "name" e
    is <- mapM (extractAttr "attrib" readMAttrIndex)
               (rawFindChildren "source" e)
    return $ MVao n is

-- eof
