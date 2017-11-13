
module LinkStruct ( LinkStruct
                  , linkStruct
                  , linkStructSimple
                  , index
                  , href
                  , text
                  ) where

data LinkStruct = LinkStruct { index :: Int
                             , href :: String
                             , text :: String
                             }

linkStruct :: Int -> String -> String -> LinkStruct
linkStruct i h = LinkStruct i (cleanup h)

linkStructSimple :: String -> String -> LinkStruct
linkStructSimple = LinkStruct 0

instance Show LinkStruct where
    show (LinkStruct _ h t) = "[[" ++ h ++ "][" ++ t ++"]]"

cleanup :: String -> String
cleanup = unwords . words . stripChars "\n\r\t"

stripChars :: String -> String -> String
stripChars = filter . flip notElem
