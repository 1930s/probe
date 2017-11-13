
module LinkStruct ( LinkStruct
                  , linkStruct
                  , linkStructSimple
                  , index
                  , href
                  , text
                  ) where

-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Exception.html
import Control.Exception ( assert )

data LinkStruct = LinkStruct { index :: Int
                             , href :: String
                             , text :: String
                             }

linkStruct :: Int -> String -> String -> LinkStruct
linkStruct i h = assert (i >= 0) $ LinkStruct i (cleanup h)

linkStructSimple :: String -> String -> LinkStruct
linkStructSimple = linkStruct 0

instance Show LinkStruct where
    show (LinkStruct _ h t) = "[[" ++ h ++ "][" ++ t ++"]]"

cleanup :: String -> String
cleanup = unwords . words . stripChars "\n\r\t"

stripChars :: String -> String -> String
stripChars = filter . flip notElem

dequote :: String -> String
dequote ('\"':xs) | last xs == '\"' = init xs
dequote x = x
