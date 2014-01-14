module Lib.Mesh.XML where

-- hackage
import Text.XML.Light
import Text.Printf (printf)
import Text.Read (readEither)

-- local
import Lib.Mesh.Util

-- XML functions --------------------------------------------------------------

-- Get words of string data from all immediate children.
childText :: Element -> [String]
childText = concat . map (words . cdData) . onlyText . elContent

-- Get the named numeric attribute from the element and parse it.
-- The only thing that really makes it numeric is the typeclass Num...
extractNumAttr :: (Read b, Num b) => String -> Element -> Either String b
extractNumAttr name el = let errf _ = printf "Element \"%s\" requires attribute \"%s\" to be numeric" (rawElName el) name
                         in extractAttr name
                                        (modLeft errf . readEither)
                                        el

-- Get the named attribute from the element and parse it.
extractAttr :: String -> (String -> Either String b) -> Element -> Either String b
extractAttr name readf el = do
    str <- requireAttr name el
    val <- readf str
    return val

-- Right with the value of the attribute. Left if it isn't present.
requireAttr :: String -> Element -> Either String String
requireAttr name el = m2e err $ findAttrBy ((name ==) . qName) el
    where
        err = printf "Element \"%s\" requires attribute \"%s\"" (rawElName el) name

-- Filter a list of elements to those for which their raw name is equal to a given string.
rawFilterByName :: String -> [Element] -> [Element]
rawFilterByName name = filter $ (name ==) . rawElName

-- Look up Elements by name from among the children of a given Element.
rawFindChildren :: String -> Element -> [Element]
rawFindChildren name = filterChildrenName $ (name ==) . qName

rawElName :: Element -> String
rawElName = qName . elName

-- eof
