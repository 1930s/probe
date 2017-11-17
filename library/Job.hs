{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Job ( Job(..)
           , JobState(..)
           , URL
           , Task(..)
           , execJob
           , printJobState
           ) where

-- http://hackage.haskell.org/package/containers
-- http://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Set.html
import qualified Data.Set as S
-- http://hackage.haskell.org/package/stm
-- http://hackage.haskell.org/package/stm-2.4.4.1/docs/Control-Concurrent-STM.html
-- http://hackage.haskell.org/package/stm-2.4.4.1/docs/Control-Concurrent-STM-TChan.html
import Control.Concurrent.STM
-- https://hackage.haskell.org/package/bytestring
-- https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Lazy.html
import qualified Data.ByteString.Lazy.Char8 as L8
-- http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State.html
import Control.Monad.State

type URL = L8.ByteString

data Task = Check URL | Done
  deriving (Show)

data JobState = JobState { linksSeen :: S.Set URL
                         , linksFound :: Int
                         , linkQueue :: TChan Task
                         }

printJobState :: StateT JobState IO ()
printJobState = do
    liftIO $ putStrLn "printing JobState"
    ls <- gets linksSeen
    lf <- gets linksFound
    liftIO (print $ "linksSeen: " ++ show ls ++ " linksFound: " ++ show lf)

newtype Job a = Job { runJob :: StateT JobState IO a }
    deriving (Functor, Applicative, Monad, MonadState JobState, MonadIO)

execJob :: Job a -> JobState -> IO JobState
execJob = execStateT . runJob
