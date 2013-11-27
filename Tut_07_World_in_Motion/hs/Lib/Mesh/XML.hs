module Lib.Mesh.XML where

-- hackage
import Text.XML.Light
import Text.Printf (printf)

-- local
import Lib.Mesh.Util

-- XML functions -------------------------------------------------------------

-- Get words of string data from all immediate children.
childText :: Element -> [String]
childText = concat . map (words . cdData) . onlyText . elContent

-- Get the named attribute from the element and parse it.
extractAttr :: Element -> String -> (String -> Either String b) -> Either String b
extractAttr e n r = do
    s <- requireAttr n e
    v <- r s
    return v

-- Right with the value of the attribute. Left if it isn't present.
requireAttr :: String -> Element -> Either String String
requireAttr s e = m2e msg $ findAttrBy ((s ==) . qName) e
    where
        msg = printf "Element \"%s\" requires attribute \"%s\"" (rawElName e) s

-- filterChildrenName :: pred -> el -> [el]     -- of children of the given element
-- filterChildName    :: pred -> el -> Maybe el -- first of ^
-- filterElementsName :: pred -> el -> [el]     -- rec
-- filterElementName  :: pred -> el -> Maybe el -- first of ^
-- findAttrBy

rawFilterByName :: String -> [Element] -> [Element]
rawFilterByName s = filter $ (s ==) . rawElName

-- Look up Elements by name from among the children of a given Element.
rawFindChildren :: String -> Element -> [Element]
rawFindChildren s = filterChildrenName $ (s ==) . qName

rawElName :: Element -> String
rawElName = qName . elName

-- eof
