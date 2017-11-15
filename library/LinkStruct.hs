module LinkStruct ( linkStruct
                  , linkStructSimple
                  , index
                  , href
                  , text
                  , isBasicStruct
                  , isLinkAndImgStruct
                  , isLinkAndMixedStruct
                  ) where

-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Exception.html
import Control.Exception ( assert )
-- https://hackage.haskell.org/package/tagsoup
-- https://github.com/ndmitchell/tagsoup
import Text.HTML.TagSoup
import Data.Char
-- https://hackage.haskell.org/package/MissingH-1.4.0.1/docs/Data-String-Utils.html
import Data.String.Utils ( replace )
-- https://hackage.haskell.org/package/regex-posix
-- https://hackage.haskell.org/package/regex-posix-0.95.2/docs/Text-Regex-Posix.html
-- https://wiki.haskell.org/Regex_Posix
import Text.Regex.Posix

data LinkStruct = LinkStruct { index :: Int
                             , href :: String
                             , text :: String
                             }

linkStruct :: Int -> String -> String -> LinkStruct
linkStruct i h t = assert (i >= 0) $ LinkStruct i (cleanup h) (cleanup t)

linkStructSimple :: String -> String -> LinkStruct
linkStructSimple = linkStruct 0

instance Show LinkStruct where
    show (LinkStruct i h t) = "[" ++ show i ++ "]:[[" ++ h ++ "][" ++ t ++"]]"

cleanup :: String -> String
cleanup = trim . replace "\\" "" . replace "\\\"" "" . replace "\\n" "" . dequote . show . stripChars "\r\t" . unwords . words

stripChars :: String -> String -> String
stripChars = filter . flip notElem

dequote :: String -> String
dequote ('\"':xs) | last xs == '\"' = init xs
dequote ('\"':xs) = xs
dequote x = x

trim :: String -> String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail :: String -> String -> String
dropSpaceTail _ "" = ""
dropSpaceTail maybeS (x:xs)
        | isSpace x = dropSpaceTail (x:maybeS) xs
        | null maybeS = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeS ++ x : dropSpaceTail "" xs

isAnExternalLink :: String -> Bool
isAnExternalLink = (=~) ("^https?://" :: String)

isHrefWithProtocol :: Tag String -> Bool
isHrefWithProtocol = isAnExternalLink . take 5 . fromAttrib "href"

isBasicStruct :: Tag String -> Tag String -> Tag String -> Bool
isBasicStruct tO tT tC = isTagOpenName "a" tO && isTagText tT && isTagCloseName "a" tC

isLinkAndImgStruct :: Tag String -> Tag String -> Bool
isLinkAndImgStruct tOa tOi = isTagOpenName "a" tOa && isTagOpenName "img" tOi

isLinkAndMixedStruct :: Tag String -> [Tag String] -> Bool
isLinkAndMixedStruct tO tgs = isTagOpenName "a" tO && any isTagText tgs
