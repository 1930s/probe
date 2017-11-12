{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Probe - concurrent workers for URLs (parallel) checking
module Probe (main) where

import LinkStruct
import Network
import Options

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
-- http://hackage.haskell.org/package/base-4.8.2.0/docs/Data-List.html
-- import Data.List ( intersperse
--                  , nub
--                  )
import Data.Char ( isControl )
-- http://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Maybe.html#v:fromJust
import Data.Maybe (fromJust)
import Data.List ( find )
import Network.URI
import Prelude
import System.Exit ( exitSuccess
                   -- , ExitCode(..)
                   -- , exitWith
                   )
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Printf (printf)
-- http://hackage.haskell.org/package/containers
-- http://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Set.html
import qualified Data.Set as S
-- https://hackage.haskell.org/package/bytestring
-- https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Lazy.html
import qualified Data.ByteString.Lazy.Char8 as L8
-- https://hackage.haskell.org/package/bytestring-0.10.8.1/docs/Data-ByteString.html
import qualified Data.ByteString as BS
-- http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Lazy-Char8.html#v:pack
-- import qualified Data.ByteString.Lazy.Char8 as C
-- http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Char8.html#v:pack
import qualified Data.ByteString.Char8 as C
-- http://hackage.haskell.org/package/http-client
-- http://hackage.haskell.org/package/http-client-tls
-- https://github.com/snoyberg/http-client
import Network.HTTP.Simple ( Response )
import Network.HTTP.Client ( responseStatus
                           , responseBody
                           , Manager
                           , httpLbs
                           , newManager
                           , managerSetProxy
                           , noProxy
                           , responseHeaders
                           , cookie_domain
                           , cookie_name
                           , destroyCookieJar
                           , responseCookieJar
                           )
import Network.HTTP.Client.TLS
-- http://hackage.haskell.org/package/http-types-0.9.1/docs/Network-HTTP-Types-Header.html#t:ResponseHeaders
-- http://hackage.haskell.org/package/http-types-0.9.1/docs/Network-HTTP-Types-Header.html#t:Header
import Network.HTTP.Types.Header
-- import qualified Network.HTTP.Client.TLS as T
-- https://hackage.haskell.org/package/http-types
import Network.HTTP.Types.Status (statusCode)
-- https://hackage.haskell.org/package/regex-posix
-- https://hackage.haskell.org/package/regex-posix-0.95.2/docs/Text-Regex-Posix.html
-- https://wiki.haskell.org/Regex_Posix
import Text.Regex.Posix

-- https://hackage.haskell.org/package/tagsoup
-- https://github.com/ndmitchell/tagsoup
import Text.HTML.TagSoup

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

leftWithError :: (MonadError IOError m, MonadIO m) => String -> m b
leftWithError ll = do
    liftIO $ putStrLn ("Something went wrong: " ++ show ll :: String)
    throwError $ userError ll

-- currentHaskellPapersFilter :: Text.StringLike.StringLike str => [Tag str] -> [[Tag str]]
--
-- drop until it gets the "current" section, it takes until it gets the
-- "haskell""
currentHaskellPapersFilter :: [Tag String] -> [[Tag String]]
currentHaskellPapersFilter tags =
      sections (~== ("<a>" :: String))
    -- TagOpen "li" [("id","lastmod")] equivalent to "<li id=lastmod>"
    $ takeWhile (~/= ("<a name=haskell>" :: String))
    $ drop 5
    $ dropWhile (~/= ("<a name=current>" :: String)) tags

-- λ> takeWhile (~/= TagClose ("title" :: String)) $ dropWhile (~/= TagOpen ("title" :: String) []) ttt
-- [TagOpen "title" [],TagText "tagsoup/Sample.hs at master \183 ndmitchell/tagsoup"]

-- λ> sections (~== ("<title>" :: String)) ttt
-- [[TagOpen "title" [],TagText "tagsoup/Sample.hs at master ... ,TagClose "title",TagText " ",TagOpen "link"
-- ... ion+xml"),("href","/opensearch.xml"),("title","GitHub")],
titlesFilter :: [Tag String] -> [Tag String]
titlesFilter tags = takeWhile (~/= TagClose ("title" :: String))
                  $ dropWhile (~/= TagOpen ("title" :: String) []) tags

-- let ttt = readFile "./parseThis.html"
-- let t = fmap parseTags ttt
-- fmap (filter (~== TagOpen "a" [("href", "")])) t
linksFilter :: [Tag String] -> [[Tag String]]
-- linksFilter tags = filter (~== TagOpen ("a" :: String) [("href", "")]) tags
-- (take 3) is only the canonical link
-- linksFilter tags = map (take 3) (sections (~== ("<a>" :: String)) tags)
linksFilter tags = map (takeWhile (~/= TagClose ("a" :: String))) (sections (~== ("<a>" :: String)) tags)

-- λ> map (\t ->  fromTagText (t !! 1)) (sections (~== "<a>") s)
-- ["here","there"]
-- λ> map (\t -> (isTagOpenName "a" (t !! 0), (fromAttrib "href" (t !! 0)), fromTagText (t !! 1), isTagCloseName "a" (t !! 2))) (sections (~== "<a>") s)
-- [(True,"http://haskell.org","here",True),(True,"http://wiki.haskell.org","there",True)]
-- λ> map (\t -> LinkStruct (isTagOpenName "a" (t !! 0)) (fromAttrib "href" (t !! 0)) (fromTagText (t !! 1)) (isTagCloseName "a" (t !! 2))) (sections (~== "<a>") s)
-- [[[http://haskell.org][here]],[[http://wiki.haskell.org][there]]]

dequote :: String -> String
dequote ('\"':xs) | last xs == '\"' = init xs
dequote x = x

-- The 'unwords . words' deletes all multiple spaces, replaces
-- tabs and newlines with spaces and trims the front and back
--
-- Let vs. where
-- https://wiki.haskell.org/Let_vs._Where
extractTitles :: (Show body) => Response body -> IO String
extractTitles r = do
    let tags = parseTags $ show (responseBody r)
    let contents = map f $ filter isTagText $ titlesFilter tags
          -- this where clause is part of the "contents" definition (thus indented)
          where f :: Tag String -> String
                f = unwords . words . fromTagText
    return $ unwords contents

extractLinks :: (Show body) => Response body -> IO [String]
extractLinks r = do
    let tags = parseTags $ show (responseBody r)
    let contents = map f $ linksFilter tags
          where f :: [Tag String] -> String
                f [tOpen, tText, tClose] | goodStruct tOpen tText tClose =
                                           show $ LinkStruct 0
                                                (fromAttrib "href" tOpen)
                                                (fromTagText tText)
                                                (isTagOpenName "a" tOpen)
                                                (isTagCloseName "a" tClose)
                -- ViewPatterns
                -- f (hd:(reverse -> (tl:_))) | isTagOpenName "a" hd && isTagText tl
                -- Head&Last
                -- f (h:tgs) | isTagOpenName "a" h && isTagText (last tgs) =
                -- Finding a tagText
                f (h:tgs) | isTagOpenName "a" h && any isTagText tgs =
                        show $ LinkStruct 0
                             (fromAttrib "href" h)
                             (fromTagText (fromJust (find isTagText tgs)))
                             (isTagOpenName "a" h)
                             (isTagCloseName "a" (last tgs))
                f raw = "ERROR: cannot parse " ++ show raw

                goodStruct :: Tag String -> Tag String -> Tag String -> Bool
                goodStruct tO tT tC = isTagOpenName "a" tO && isTagText tT && isTagCloseName "a" tC

    return contents

-- λ> :m +Text.HTML.TagSoup
-- λ> :m +System.IO
-- λ> (fmap lines $ readFile "./parseThis.html") >>= \c -> mapM_ putStrLn c
-- λ> fmap parseTags (readFile "./parseThis.html")
-- λ> fmap (sections (~== ("<a>" :: String)) . parseTags) (readFile "./parseThis.html")
-- λ> map (take 3) (sections (~== "<a>") (parseTags t))
-- [[TagOpen "a" [("href","src/Text-HTML-TagSoup.html")],TagText "Source",TagClose "a"],[TagOpen "a" [("href","/package/tagsoup-0.14.1")],TagText "Contents",TagClose "a"],[TagOpen "a" [("href","doc-index.html")],TagText "Index",TagClose "a"]]
-- λ> fmap (map (take 3)) (fmap (sections (~== ("<a>" :: String)) . parseTags) (readFile "./parseThis.html"))
-- λ> fmap (map (\(a:b:c) -> LinkStruct 0 (fromAttrib "href" a) (fromTagText b) True True)) tt

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
                Right n   -> atomically $ writeTChan goodLinks n
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
    filterM seenURI urls >>= sendJobs
    updateStats (length urls)

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
