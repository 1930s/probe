
module LinkStruct ( LinkStruct(..)
                  , linkStructSimple
                  ) where

data LinkStruct = LinkStruct { index :: Int
                             , href :: String
                             , text :: String
                             }

linkStructSimple :: String -> String -> LinkStruct
linkStructSimple = LinkStruct 0

instance Show LinkStruct where
    show (LinkStruct _ h t) = "[[" ++ h ++ "][" ++ t ++"]]"
