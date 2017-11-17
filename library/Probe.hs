{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Probe - concurrent workers for URLs (parallel) checking
module Probe (main) where

import Network
import Options
import Job
import Printing
import LinkStruct

-- import Control.Arrow
import Control.Concurrent ( forkIO
                          -- , threadDelay
                          )
-- http://hackage.haskell.org/package/stm
-- http://hackage.haskell.org/package/stm-2.4.4.1/docs/Control-Concurrent-STM.html
-- http://hackage.haskell.org/package/stm-2.4.4.1/docs/Control-Concurrent-STM-TChan.html
import Control.Concurrent.STM
import Control.Exception ( finally
                         , catch
                         , SomeException
                         )
-- https://hackage.haskell.org/package/base
-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Monad.html
-- when :: Applicative f => Bool -> f () -> f ()
import Control.Monad ( when )
-- http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html
import Control.Monad.Except
-- http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State.html
import Control.Monad.State
-- https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html
import Control.Monad.Reader
import Data.Char ( isControl )
import Network.URI
import Prelude
-- import System.Exit ( exitSuccess , ExitCode(..) , exitWith)
import Text.Printf (printf)
-- http://hackage.haskell.org/package/containers
-- http://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Set.html
import qualified Data.Set as S
-- https://hackage.haskell.org/package/bytestring
-- https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Lazy.html
import qualified Data.ByteString.Lazy.Char8 as L8
-- https://hackage.haskell.org/package/regex-posix
-- https://hackage.haskell.org/package/regex-posix-0.95.2/docs/Text-Regex-Posix.html
-- https://wiki.haskell.org/Regex_Posix
import Text.Regex.Posix

main :: IO ()
{-# ANN main ("HLint: ignore Use let" :: String) #-}
main = do
    -- runReader :: Reader r a -> r -> a
    -- Runs a Reader and extracts the final value from it. (The inverse of reader.)
    runReaderT printReaderContent "Concurrent URL checker"
    opts <- parseArgs
    let n = length $ optFiles opts
    runReaderT (printOrNot ("working on " ++ show n ++ " files")) opts
    -- mapM_ putStrLn $ optFiles opts
    -- count of broken links
    badCount <- newTVarIO (0 :: Int)
    -- for reporting broken links
    badLinks <- newTChanIO
    -- for reporting good links
    goodLinks <- newTChanIO
    -- for sending jobs to workers
    jobs <- newTChanIO
    -- the number of workers currently running
    k <- optWorkers opts
    workers <- newTVarIO k
    -- one thread reports bad links to stdout
    _ <- ($) forkIO (printBadLinks badLinks)
    -- one thread reports good links to stdout
    _ <- ($) forkIO (printGoodLinks goodLinks)
    runReaderT (printOrNot ("start " ++ show k ++ " worker threads")) opts
    forkTimes k workers (worker badLinks goodLinks jobs badCount opts)
    runReaderT (printOrNot "threads started") opts
    runReaderT (printOrNot "read links from files") opts
    when (optVerbose opts) $ mapM_ printURLs (optFiles opts)
    runReaderT (printOrNot "and enqueue them as jobs") opts
    urls <- return (mapM_ (\s -> checkURLs s (optVerbose opts)) (optFiles opts))
    _ <- liftIO $ return $ runJob urls >> printJobState
    stats <- execJob urls (JobState S.empty 0 jobs)
    unless (optAuto opts) $ do
        runReaderT (printOrNot "enqueue 'please finish' messages") opts
        atomically $ replicateM_ k (writeTChan jobs Done)
    runReaderT (printOrNot "waiting for workers") opts
    waitFor workers
    broken <- atomically $ readTVar badCount
    printf fmt broken
               (linksFound stats)
               (S.size (linksSeen stats))
               n
    putStrLn "bye"
  where
    fmt = "Found %d broken links. Checked %d links (%d unique) in %d files.\n"

modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ tv f = readTVar tv >>= writeTVar tv . f

forkTimes :: Int -> TVar Int -> IO () -> IO ()
forkTimes k alive act =
  replicateM_ k . forkIO $
    act
    `finally`
    atomically (modifyTVar_ alive (subtract 1))

-- waitFor function uses 'check', which calls 'retry' if its argument evaluates
-- to False.
waitFor :: TVar Int -> IO ()
waitFor alive = atomically $ do
  count <- readTVar alive
  check (count == 0)

worker :: TChan String -> TChan String -> TChan Task -> TVar Int -> Options -> IO ()
worker badLinks goodLinks jobQueue badCount opts = loop
  where
    -- Consume jobs until we are told to exit.
    loop = do
        job <- atomically $ readTChan jobQueue
        case job of
            Done  -> return ()
            Check x -> checkOne (L8.unpack x) >> loop
    -- Check a single link.
    checkOne url = case parseURI url of
        Just uri -> do
            when (optVerbose opts) $ putStrLn $ "checking " ++ show uri
            code <- runReaderT (getBodyE (show uri)) opts `catch` (\e -> return $ Left (show (e :: SomeException)))
            case code of
                -- Right 200 -> return ()
                Right n   -> atomically $ writeTChan goodLinks (show n)
                Left err  -> report err
        _ -> report "invalid URL"
        where report s = atomically $ do
                           modifyTVar_ badCount (+1)
                           writeTChan badLinks (url ++ " " ++ s)

checkURLs :: FilePath -> Bool -> Job ()
checkURLs f v = do
    when v $ liftIO $ putStrLn ("checkURLs from " ++ show f)
    src <- liftIO $ L8.readFile f
    let urls = parseLinks src
    sendURLsAsJob urls

sendURLsAsJob :: [URL] -> Job ()
sendURLsAsJob urls = do
    filterM seenURI urls >>= sendJobs
    updateStats (length urls)

sendURLAsJob :: String -> Job ()
sendURLAsJob url = sendURLsAsJob [L8.pack url :: URL]

updateStats :: Int -> Job ()
updateStats a = modify $ \s ->
    s { linksFound = linksFound s + a }

-- | Add a link to the set we have seen.
insertURI :: URL -> Job ()
insertURI c = modify $ \s ->
    s { linksSeen = S.insert c (linksSeen s) }

-- | If we have seen a link, return False. Otherwise, record that we
-- have seen it, and return True.
seenURI :: URL -> Job Bool
seenURI url = do
    seen <- (not . S.member url) `liftM` gets linksSeen
    insertURI url
    return seen

sendJobs :: [URL] -> Job ()
sendJobs js = do
    c <- gets linkQueue
    liftIO . atomically $ mapM_ (writeTChan c . Check) js

parseLinks :: L8.ByteString -> [URL]
parseLinks = concatMap uris . L8.lines
  where uris s      = filter looksOkay (L8.splitWith isDelim s)
        isDelim c   = isControl c || c `elem` (" <>\"{}|\\^[]`" :: String)
        looksOkay s = s =~ ("https?://" :: String):: Bool

printURLs :: FilePath -> IO ()
printURLs f = do
    putStrLn ("printURLs from " ++ show f)
    src <- liftIO $ L8.readFile f
    let urls = parseLinks src
    mapM_ (\s -> putStrLn $ " - " ++ show s) urls
