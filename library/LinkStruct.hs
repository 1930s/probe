
module LinkStruct ( LinkStruct(..)
                  , linkStructSimple
                  ) where

data LinkStruct = LinkStruct { index :: Int
                             , href :: String
                             , text :: String
                             , tagOpenA :: Bool
                             , tagCloseA :: Bool
                             }

linkStructSimple :: String -> String -> LinkStruct
linkStructSimple h t = LinkStruct 0 h t True True

instance Show LinkStruct where
    show (LinkStruct _ h t _ _) = "[[" ++ h ++ "][" ++ t ++"]]"
