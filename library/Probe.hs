{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Probe - concurrent workers for URLs (parallel) checking
module Probe (main) where

-- main :: IO ()
-- main = return ()

-- TODO: add index
-- TODO: fix default index (0) in LinkStruct
-- TODO: add more patterns (<a> with <img> and "alt" property inside)
-- DONE: more comprehensive/relaxed patterns (randomly placed tagText, pick the first available)
-- DONE: add the --errors option
-- DONE: print only errors in parsing (use option)
-- TODO: filter internal/external links
-- TODO: pick the next external URLs randomly (autoseeding)
-- TODO: split this file because emacs is lagging
-- TODO: add "error only" option in args
-- TODO: move LinkStuct validation (goodStruct) to the data itself
-- TODO: add support for multiple files in --files options
-- TODO: move the LinkStruct check automatically in the creation
-- TODO: better guess in extractLinks: how to pick the right text
-- TODO: cleanup the LinkText
-- TODO: pasteBin
-- DONE: print also goodLinks

-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/pattern-and-guard-extensions#viewpatterns
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE PatternGuards #-}

-- | Result
-- http://hackage.haskell.org/package/HTTP-4000.3.7/docs/Network-Stream.html#t:Result
-- type Result a = Either ConnError a
-- This is the type returned by many exported network functions.
-- -- | This is the type returned by many exported network functions.
-- type Result a = Either ConnError   {- error  -}
--                        a           {- result -}
-- lift always lifts from the "previous" layer.
-- liftIO always lifts from the IO layer

-- | ParseRequest
-- Convert a URL into a Request.
-- parseRequest :: MonadThrow m => String -> m Request
-- http://hackage.haskell.org/package/http-client-0.5.7.0/docs/Network-HTTP-Client.html#v:parseRequest
--
-- > request' <- parseRequest "POST http://httpbin.org/post"
-- >     let request
-- >             = setRequestMethod "PUT"
-- >             $ setRequestPath "/put"
-- >             $ setRequestQueryString [("hello", Just "world")]
-- >             $ setRequestBodyLBS "This is my request body"
-- >             $ setRequestSecure True
-- >             $ setRequestPort 443
-- >             $ request'
-- >     response <- httpJSON request
-- | FindHeader
-- https://hackage.haskell.org/package/HTTP-4000.3.7/docs/Network-HTTP-Headers.html
-- findHeader :: HasHeaders a => HeaderName -> a -> Maybe String
-- findHeader hdrNm x looks up hdrNm in x, returning the first
-- header that matches, if any

-- | Left
-- This function is defined in Control.Arrow.
-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Arrow.html
-- https://www.haskell.org/arrows/
-- Feed marked inputs through the argument arrow, passing the rest through
-- unchanged to the output.
-- left :: (a -> c) -> Either a b -> Either c b
-- left f (Left x)  = Left (f x)
-- left _ (Right x) = Right x

-- | Either
-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Either.html
-- either :: (a -> c) -> (b -> c) -> Either a b -> c
-- either f _ (Left x)     =  f x
-- either _ g (Right y)    =  g y
--
-- Case analysis for the Either type. If the value is Left a, apply the first
-- function to a; if it is Right b, apply the second function to b.
--
-- We create two values of type Either String Int, one using the Left
-- constructor and another using the Right constructor. Then we apply "either"
-- the length function (if we have a String) or the "times-two" function (if
-- we have an Int):
--
-- let s = Left "foo" :: Either String Int
-- let n = Right 3 :: Either String Int
-- either length (*2) s
-- >> 3
-- either length (*2) n
-- >> 6

-- | Maybe
-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Maybe.html
--   maybe :: b -> (a -> b) -> Maybe a -> b
--   maybe n _ Nothing  = n
--   maybe _ f (Just x) = f x
--
-- The maybe function takes a default value, a function, and a Maybe value. If
-- the Maybe value is Nothing, the function returns the default value.
-- Otherwise, it applies the function to the value inside the Just and returns
-- the result.
--
-- >> maybe False odd (Just 3)
-- True
-- >> maybe False odd Nothing
-- False
--
-- Read an integer from a string using readMaybe. If we succeed, return twice
-- the integer; that is, apply (*2) to it. If instead we fail to parse an
-- integer, return 0 by default:
--
-- >> import Text.Read ( readMaybe )
-- >> maybe 0 (*2) (readMaybe "5")
-- 10
-- >>> maybe 0 (*2) (readMaybe "")
-- 0
--
-- Apply show to a Maybe Int. If we have Just n, we want to show the
-- underlying Int n. But if we have Nothing, we return the empty string
-- instead of (for example) "Nothing":
--
-- >> maybe "" show (Just 5)
-- "5"
-- >> maybe "" show Nothing
-- ""

-- | TChan
--- TChan is an abstract type representing an unbounded FIFO channel:
--  https://hackage.haskell.org/package/stm-2.4.4.1/docs/Control-Concurrent-STM-TChan.html
--- A TQueue is like a TChan, with two important differences:
--  - it has faster throughput than both TChan and Chan (although the costs are
--    amortised, so the cost of individual operations can vary a lot).
--  - it does not provide equivalents of the dupTChan and cloneTChan
--    operations.
--  The implementation is based on the traditional purely-functional queue
--  representation that uses two lists to obtain amortised O(1) enqueue and
--  dequeue operations.
--  https://hackage.haskell.org/package/stm-2.4.4.1/docs/Control-Concurrent-STM-TQueue.html

-- | ($)
-- ($) has loosest fixity, infixr 0

-- | Left
-- left is in Control.Arrow
-- left f (Left x) = Left $ f x
-- left   (Right x) = Right x

-- | Finally
-- finally has default tight fixity, infixl 9
-- forkTimes k alive act =
--   (replicateM_ k . forkIO) $ (act `finally` (atomically $ (modifyTVar_ alive (subtract 1))))

-- | Forever
-- ghci> :m +Control.Monad
-- ghci> :type forever
-- forever :: (Monad m) => m a -> m ()

-- | SimpleHTTP
-- http://hackage.haskell.org/package/HTTP-4000.3.7/docs/Network-HTTP.html
-- simpleHTTP :: HStream ty => Request ty -> IO (Result (Response ty))
-- simpleHTTP req transmits the Request req by opening a direct, non-persistent
-- connection to the HTTP server that req is destined for, followed by
-- transmitting it and gathering up the response as a Result. Prior to sending
-- the request, it is normalized (via normalizeRequest). If you have to mediate
-- the request via an HTTP proxy, you will have to normalize the request
-- yourself. Or switch to using Browser instead.
-- Examples:
-- simpleHTTP (getRequest "http://hackage.haskell.org/")
-- simpleHTTP (getRequest "http://hackage.haskell.org:8012/")

-- | ErrorT monad transformer
-- ExceptT monad transformer can be used to add error handling to another monad.
-- newtype ExceptT e m a :: * -> (* -> *) -> * -> * #
--
-- A monad transformer that adds exceptions to other monads.
-- ExceptT constructs a monad parameterized over two things:
-- e - The exception type.
-- m - The inner monad.
-- The return function yields a computation that produces the given value,
-- while >>= sequences two subcomputations, exiting on the first exception.
-- Constructors
-- ExceptT (m (Either e a))

-- | Control Monad Except
-- import Control.Monad.Except
--
-- -- An IO monad which can return String failure.
-- -- It is convenient to define the monad type of the combined monad,
-- -- especially if we combine more monad transformers.
-- type LengthMonad = ExceptT String IO
--
-- main = do
--   -- runExceptT removes the ExceptT wrapper
--   r <- runExceptT calculateLength
--   reportResult r
--
-- -- Asks user for a non-empty string and returns its length.
-- -- Throws an error if user enters an empty string.
-- calculateLength :: LengthMonad Int
-- calculateLength = do
--   -- all the IO operations have to be lifted to the IO monad in the monad stack
--   liftIO $ putStrLn "Please enter a non-empty string: "
--   s <- liftIO getLine
--   if null s
--     then throwError "The string was empty!"
--     else return $ length s
--
-- -- Prints result of the string length calculation.
-- reportResult :: Either String Int -> IO ()
-- reportResult (Right len) = putStrLn ("The length of the string is " ++ (show len))
-- reportResult (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))

-- | Nub
-- http://hackage.haskell.org/package/base-4.10.0.0/docs/Data-List.html#v:nub
--   /O(n^2)/. The 'nub' function removes duplicate elements from a list.
-- In particular, it keeps only the first occurrence of each element.
-- (The name 'nub' means \`essence\'.)
-- It is a special case of 'nubBy', which allows the programmer to supply
-- their own equality test.
-- nub :: (Eq a) => [a] -> [a]
-- nub =  nubBy (==)
--
-- | The 'nubBy' function behaves just like 'nub', except it uses a
-- user-supplied equality predicate instead of the overloaded '=='
-- function.
-- nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
-- -- #ifdef USE_REPORT_PRELUDE
-- nubBy eq []             =  []
-- nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)
-- -- #else
-- -- stolen from HBC
-- nubBy eq l              = nubBy' l []
--   where
--     nubBy' [] _         = []
--     nubBy' (y:ys) xs
--        | elem_by eq y xs = nubBy' ys xs
--        | otherwise       = y : nubBy' ys (y:xs)

-- | ThreadScope
-- https://wiki.haskell.org/ThreadScope
-- stack ghc -- -O2 -threaded --make src/UrlChecker.hs

-- | GetOpt
-- http://hackage.haskell.org/package/base-4.10.0.0/docs/System-Console-GetOpt.html
-- data ArgOrder a
-- What to do with options following non-options
-- Constructors
-- RequireOrder	               no option processing after first non-option
-- Permute                     freely intersperse options and non-options
-- ReturnInOrder (String -> a) wrap non-options into options

-- import Control.Arrow
import Control.Concurrent ( forkIO
                          -- , threadDelay
                          )
import Control.Concurrent.STM
import Control.Exception ( finally
                         , catch
                         , SomeException
                         )
-- https://hackage.haskell.org/package/base
-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Monad.html
-- when :: Applicative f => Bool -> f () -> f ()
import Control.Monad ( when )
import Control.Monad.Except
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
import System.Console.GetOpt
import System.Environment ( getArgs
                          , getProgName
                          )
import System.Exit ( exitSuccess
                   -- , ExitCode(..)
                   -- , exitWith
                   )
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Printf (printf)
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Set as S
-- | The HTTP package
-- http://hackage.haskell.org/package/HTTP
-- https://github.com/haskell/HTTP
-- http://hackage.haskell.org/package/HTTP-4000.3.7/docs/Network-HTTP.html
-- http://hackage.haskell.org/package/HTTP-4000.3.7/docs/Network-Stream.html#t:Result
-- import Network.HTTP hiding (Done)
-- import Network.Stream ( Result )
--
-- | The http-streams package
-- http://hackage.haskell.org/package/http-streams
-- https://github.com/afcowie/http-streams/
-- http://hackage.haskell.org/package/http-streams-0.8.5.3/docs/Network-Http-Client.html
--
-- | The http-client package
-- http://hackage.haskell.org/package/http-client
-- http://hackage.haskell.org/package/http-client-tls
-- https://github.com/snoyberg/http-client
-- Tutorial:
-- https://haskell-lang.org/library/http-client
import Network.HTTP.Simple
-- https://hackage.haskell.org/package/http-client-0.5.7.0/docs/Network-HTTP-Client.html#v:responseStatus
--
-- httpLbs :: Request -> Manager -> IO (Response ByteString)
-- A convenience wrapper around withResponse which reads in the entire response
-- body and immediately closes the connection. Note that this function performs
-- fully strict I/O, and only uses a lazy ByteString in its response for memory
-- efficiency. If you are anticipating a large response body, you are encouraged
-- to use withResponse and brRead instead.
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
-- import Data.Aeson (Value)
-- import qualified Data.ByteString.Char8 as S8
-- import qualified Data.Yaml             as Yaml
-- import qualified Network.HTTP as H
-- import qualified Network.HTTP.Base as N
-- import Network.HTTP.Headers
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

-- https://hackage.haskell.org/package/bytestring-0.10.8.1/docs/Data-ByteString.html
import qualified Data.ByteString as BS

-- http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Lazy-Char8.html#v:pack
-- import qualified Data.ByteString.Lazy.Char8 as C
-- http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Char8.html#v:pack
import qualified Data.ByteString.Char8 as C

-- https://hackage.haskell.org/package/tagsoup
-- https://github.com/ndmitchell/tagsoup
import Text.HTML.TagSoup

type URL = L8.ByteString

data Task = Check URL | Done
  deriving (Show)

printReaderContent :: ReaderT String IO ()
printReaderContent = do
    content <- ask
    liftIO $ putStrLn ("The URL Checker: " ++ content)

-- $ stack exec urlChecker -- -h
-- The URL Checker: Concurrent URL checker
-- urlChecker
--   -o FILE     --output=FILE          Output file
--   -f FILE     --files=FILE           Output file
--   -v          --verbose              Enable verbose messages
--   -n WORKERS  --concurrency=WORKERS  Number of concurrent connections (default 16)
--   -V          --version              Print version
--   -h          --help                 Show help
--
-- $ stack exec urlChecker -- -V
-- The URL Checker: Concurrent URL checker
-- Version 0.0.1

-- printOrNot :: (MonadIO m, MonadReader Options m) => String -> m ()
printOrNot :: String -> ReaderT Options IO ()
printOrNot msg = do
    opts <- ask
    when (optVerbose opts) $ liftIO $ putStrLn msg

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

printBadLinks :: TChan String -> IO ()
printBadLinks c =
  forever $ atomically (readTChan c) >>= (\s -> putStrLn $ "badLink: " ++ s) >> hFlush stdout

printGoodLinks :: TChan String -> IO ()
printGoodLinks c =
  forever $ atomically (readTChan c) >>= (\s -> putStrLn $ "- " ++ s) >> hFlush stdout

-- waitFor function uses 'check', which calls 'retry' if its argument evaluates
-- to False.
waitFor :: TVar Int -> IO ()
waitFor alive = atomically $ do
  count <- readTVar alive
  check (count == 0)

type ResponseContent = Response L8.ByteString
type ResponseMonad = ReaderT Options (ExceptT String IO)

-- λ> getHead . fromJust $ parseURI "http://haskell.org"
-- Right HTTP/1.1 301 Moved Permanently
--
-- runExceptT $ runReaderT (getHeadHTTPS "https://haskell.org") startOptions
getHeadHTTPS :: String -> ResponseMonad ResponseContent
getHeadHTTPS = getSomethingHTTPS "HEAD"

-- runExceptT $ runReaderT (getGetHTTPS "https://haskell.org") startOptions
-- Right (Response {resp ... responseBody = "<!DOCTYPE HTML><html><head><ti ...
getGetHTTPS :: String -> ResponseMonad ResponseContent
getGetHTTPS = getSomethingHTTPS "GET"

-- https://www.stackage.org/haddock/lts-9.5/http-client-0.5.7.0/Network-HTTP-Client.html
-- http://hackage.haskell.org/package/http-client-0.5.7.0/docs/Network-HTTP-Client.html#v:parseRequest
--
getSomethingHTTPS :: String -> String -> ResponseMonad ResponseContent
getSomethingHTTPS verb uri =
    if verb `elem` ["HEAD", "GET", "POST", "PUT", "DELETE"]
    then do
        opts <- ask
        case (parseRequest uri :: Either SomeException Request) of
            Left m ->
                liftIO (putStrLn ("Error in request: " ++ show m :: String)) >> throwError (show m)
            Right r -> do
                let request = setRequestMethod (C.pack verb)
                              $ setRequestBodyLBS ""
                              $ setRequestSecure True
                              $ setRequestPort 443
                              $ setRequestHeaders [] r
                let manager = newManager $ managerSetProxy noProxy tlsManagerSettings
                when (optVerbose opts) $ liftIO $ putStrLn "preparing request"
                liftIO $ runReaderT (runManager request manager) opts
    else do
        let m = "Invalid HTTP verb passed"
        liftIO (putStrLn m) >> throwError (show m)

runManager :: Request -> IO Manager -> ReaderT Options IO (Response L8.ByteString)
runManager request ioManager = do
    opts <- ask
    when (optVerbose opts) $ liftIO $ putStrLn "running the manager"
    manager <- lift ioManager
    lift $ Network.HTTP.Client.httpLbs request manager

-- type ResponseContent = Response L8.ByteString
-- type ResponseMonad = ExceptT String IO
-- getHeadHTTPS :: String -> ResponseMonad ResponseContent
-- getHeadHTTPS :: String -> ExceptT String IO ResponsContent
-- getHeadHTTPS :: String -> ExceptT String IO Response L8.ByteString
-- either :: (a -> c) -> (b -> c) -> Either a b -> c
-- runExceptT :: ExceptT e m a -> m (Either e a)
-- runExceptT $ getHeadHTTPS u :: IO (Either String ResponseContent)
-- responseStatus :: Response body -> Status
-- responseStatus :: Response body -> Network.HTTP.Types.Status.Status
-- responseBody :: Response body -> body
--
-- getStatusE :: MonadReader Options m => String -> m IO (Either String Int)
getStatusE :: String -> ReaderT Options IO (Either String Int)
getStatusE s = do
  opts <- ask
  lift (runExceptT $ chaseStatus (5 :: Int) s opts)

getBodyE :: String -> ReaderT Options IO (Either String String)
getBodyE s = do
  opts <- ask
  lift (runExceptT $ chaseBody (5 :: Int) s opts)

chaseStatus :: Int -> String -> Options -> ExceptT String IO Int
chaseStatus 0 _ _ = throwError "too many redirects"
chaseStatus n u o = do
    when (optVerbose o) $ liftIO $ putStrLn ("chasing " ++ u ++ "..." :: String)
    ExceptT $ runExceptT (runReaderT (getHeadHTTPS u) o) >>= \case
        Left ll -> leftWithError ll
        Right r -> rightWithStatusCode r u n o

chaseBody :: Int -> String -> Options -> ExceptT String IO String
chaseBody 0 _ _ = throwError "too many redirects"
chaseBody n u o = do
    when (optVerbose o) $ liftIO $ putStrLn ("chasing " ++ u ++ "..." :: String)
    ExceptT $ runExceptT (runReaderT (getGetHTTPS u) o) >>= \case
        Left ll -> leftWithError ll
        Right r -> rightWithBody r u n o

rightWithStatusCode :: (Show a) => Response body -> a -> Int -> Options -> IO (Either String Int)
rightWithStatusCode r u n o = do
    maybePrintSomething u r Nothing o
    case statusCode (responseStatus r) `div` 100 :: Int of
        3 -> do
            let u' = return $ lookup hLocation (responseHeaders r)
            url <- embedMaybe (userError "bad URL") u'
            runExceptT $ chaseStatus (n-1) (show url) o
        a -> return (Right a)

rightWithBody :: (Show a, Show body) => Response body -> a -> Int -> Options -> IO (Either String String)
rightWithBody r u n o =
    case statusCode (responseStatus r) `div` 100 :: Int of
        3 -> do
            let u' = return $ lookup hLocation (responseHeaders r)
            url <- embedMaybe (userError "bad URL") u'
            runExceptT $ chaseBody (n-1) (show url) o
        4 -> return $ Left "ERROR: 4** nope"
        5 -> return $ Left "ERROR: 5** dead"
        _ -> do
            extractLinks r >>= \tl -> printLinksOrgMode u r tl o
            extractTitles r >>= \ts -> do
                maybePrintSomething u r (Just ts) o
                return $ Right (serverLine u (getServer r) (Just ts))

printLinksOrgMode :: (Foldable t1) => t3 -> t2 -> t1 String -> Options -> IO ()
printLinksOrgMode _u _r tl o =
    mapM_ pf tl
      where pf :: String -> IO ()
            pf s = if optErrors o
                     then when (isError $ take 6 s) $ putStrLn s
                   else putStrLn s

errorPattern :: L8.ByteString
errorPattern = "^ERROR:" :: L8.ByteString

isError :: String -> Bool
isError = flip (=~) errorPattern

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

maybePrintSomething :: (Show a, MonadIO f) => a -> Response body -> Maybe String -> Options -> f ()
maybePrintSomething u r t o =
    when (optVerbose o) (
        maybePrintServer u (getServer r) t >>
            liftIO (maybePrintCookies u r) >>
                maybePrintContentLength (getContentLength r))

-- https://hackage.haskell.org/package/http-client-0.5.7.0/docs/Network-HTTP-Client.html#t:CookieJar
maybePrintCookies :: (Show a) => a -> Response body -> IO ()
maybePrintCookies u r =
    mapM_
    (\c -> putStrLn $ " - "
           ++ show u
           ++ " : cookieName: "
           ++ show (cookie_name c)
           ++ ", cookieDomain; "
           ++ show (cookie_domain c))
    (destroyCookieJar (responseCookieJar r))

getContentLength :: Response body -> Maybe BS.ByteString
getContentLength r = lookup hContentLength (responseHeaders r)

getServer :: Response body -> Maybe BS.ByteString
getServer r = lookup hServer (responseHeaders r)

serverLine :: (Show a1, Show a2 ) => a1 -> Maybe a2 -> Maybe String -> String
serverLine u m (Just t) =
    case m of
        Just sr -> "[" ++ show u ++ "] with title \"" ++ t ++ "\" is served by " ++ show sr ++ "."
        Nothing -> "[" ++ show u ++ "] with title \"" ++ t ++ "\" is served by an anonymous server"
serverLine u m Nothing =
    case m of
        Just sr -> "[" ++ show u ++ "] is served by " ++ show sr ++ "."
        Nothing -> "[" ++ show u ++ "] is served by an anonymous server"

maybePrintServer :: (Show a, Show b, MonadIO m) => a -> Maybe b -> Maybe String -> m ()
maybePrintServer u m t = liftIO $ putStrLn (serverLine u m t)

contentLengthLine :: Show a => Maybe a -> String
contentLengthLine m =
    case m of
        Just cl -> "Found something: " ++ show cl ++ " (content length)"
        Nothing -> "Found something but without contentLength"

maybePrintContentLength :: (Show a, MonadIO m) => Maybe a -> m ()
maybePrintContentLength = liftIO . putStrLn . contentLengthLine

-- Some handy embedding functions.
embedEither :: (MonadError e m) => (s -> e) -> Either s a -> m a
-- either :: (a -> c) -> (b -> c) -> Either a b -> c
-- embedEither f esa = either (throwError . f) return esa
embedEither f = either (throwError . f) return

embedMaybe :: (MonadError e m) => e -> Maybe a -> m a
embedMaybe err = maybe (throwError err) return

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

data JobState = JobState { linksSeen :: S.Set URL
                         , linksFound :: Int
                         , linkQueue :: TChan Task
                         }

newtype Job a = Job { runJob :: StateT JobState IO a }
    deriving (Functor, Applicative, Monad, MonadState JobState, MonadIO)

printJobState :: StateT JobState IO ()
printJobState = do
    liftIO $ putStrLn "printing JobState"
    ls <- gets linksSeen
    lf <- gets linksFound
    liftIO (print $ "linksSeen: " ++ show ls ++ " linksFound: " ++ show lf)

execJob :: Job a -> JobState -> IO JobState
execJob = execStateT . runJob

printURLs :: FilePath -> IO ()
printURLs f = do
    putStrLn ("printURLs from " ++ show f)
    src <- liftIO $ L8.readFile f
    let urls = parseLinks src
    mapM_ (\s -> putStrLn $ " - " ++ show s) urls

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

data Options = Options  { optFiles :: [String]
                        , optVerbose :: Bool
                        , optErrors :: Bool
                        , optWorkers :: IO Int
                        , optOutput :: String -> IO ()
                        }

startOptions :: Options
startOptions = Options  { optFiles = []
                        , optVerbose = False
                        , optErrors = False
                        , optWorkers = return 8
                        , optOutput = putStr
                        }

parseArgs :: IO Options
parseArgs = do
    args <- getArgs
    let (actions, _nonOptions, _errors) = getOpt Permute options args
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optFiles = files
                , optVerbose = verbose
                , optWorkers = workers
                , optOutput = _output } = opts
    when verbose (hPutStrLn stderr "Hey!")
    when verbose (hPutStrLn stderr $ "files are: " ++ show files)
    when verbose (workers >>= \w -> hPutStrLn stderr $ "workers are: " ++ show w)
    return opts

options :: [ OptDescr (Options -> IO Options) ]
options = [ Option "o" ["output"]
              (ReqArg (\arg opt -> return opt { optOutput = writeFile arg }) "FILE")
              "Output file"
          , Option "f" ["files"]
              (ReqArg (\arg opt -> return opt { optFiles = [arg] }) "FILE")
              "Output file"
          , Option "e" ["errors"]
              (NoArg (\opt -> return opt { optErrors = True }))
              "Print only error messages"
          , Option "v" ["verbose"]
              (NoArg (\opt -> return opt { optVerbose = True }))
              "Enable verbose messages"
          , Option ['n'] ["concurrency"]
              (ReqArg (\arg opt -> return opt { optWorkers = return (read arg :: Int) }) "WORKERS")
               "Number of concurrent connections (default 16)"
          , Option "V" ["version"]
              (NoArg (\_ -> do
                      hPutStrLn stderr "Version 0.0.1"
                      exitSuccess))
              "Print version"
          , Option "h" ["help"]
              (NoArg (\_ -> do
                      prg <- getProgName
                      hPutStrLn stderr (usageInfo prg options)
                      exitSuccess))
              "Show help"
          ]
