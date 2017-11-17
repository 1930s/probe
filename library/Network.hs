-- https://prime.haskell.org/wiki/FlexibleContexts
{-# LANGUAGE FlexibleContexts #-}
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#lambdacase
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Network where

import Options
import Utils
import Printing
import Filters
import LinkStruct

-- https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html
import Control.Monad.Reader
-- http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html
import Control.Monad.Except
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
import Network.HTTP.Simple ( Response
                           , Request
                           , parseRequest
                           , setRequestMethod
                           , setRequestBodyLBS
                           , setRequestSecure
                           , setRequestPort
                           , setRequestHeaders
                           )
-- http://hackage.haskell.org/package/http-client
-- http://hackage.haskell.org/package/http-client-tls
-- https://github.com/snoyberg/http-client
import Network.HTTP.Client ( responseStatus
                           , Manager
                           , httpLbs
                           , newManager
                           , managerSetProxy
                           , noProxy
                           , responseHeaders
                           , destroyCookieJar
                           , responseCookieJar
                           )
import Network.HTTP.Client.TLS
-- https://hackage.haskell.org/package/base
-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Monad.html
-- when :: Applicative f => Bool -> f () -> f ()
import Control.Monad ( when )
-- http://hackage.haskell.org/package/http-types-0.9.1/docs/Network-HTTP-Types-Header.html#t:ResponseHeaders
-- http://hackage.haskell.org/package/http-types-0.9.1/docs/Network-HTTP-Types-Header.html#t:Header
import Network.HTTP.Types.Header
-- import qualified Network.HTTP.Client.TLS as T
-- https://hackage.haskell.org/package/http-types
import Network.HTTP.Types.Status (statusCode)
-- https://github.com/haskell/random
-- https://hackage.haskell.org/package/random
import System.Random (randomRIO)
-- http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Lazy-Char8.html#v:pack
-- import qualified Data.ByteString.Lazy.Char8 as C
-- http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Char8.html#v:pack
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L8
-- https://hackage.haskell.org/package/bytestring-0.10.8.1/docs/Data-ByteString.html
import qualified Data.ByteString as BS
-- https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Exception.html
import Control.Exception ( SomeException) -- finally, catch)

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

getBodyE :: String -> ReaderT Options IO (Either String LinkStruct)
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

chaseBody :: Int -> String -> Options -> ExceptT String IO LinkStruct
chaseBody 0 _ _ = throwError "too many redirects"
chaseBody n u o = do
    when (optVerbose o) $ liftIO $ putStrLn ("chasing " ++ u ++ "..." :: String)
    ExceptT $ runExceptT (runReaderT (getGetHTTPS u) o) >>= \case
        Left ll -> leftWithError ll
        Right r -> rightWithBody r u n o

rightWithStatusCode :: (Show a) => Response body -> a -> Int -> Options -> IO (Either String Int)
rightWithStatusCode r u n o = do
    maybePrintSomething u (getServer r) (getContentLength r) (destroyCookieJar (responseCookieJar r)) Nothing o
    case statusCode (responseStatus r) `div` 100 :: Int of
        3 -> do
            let u' = return $ lookup hLocation (responseHeaders r)
            url <- embedMaybe (userError "bad URL") u'
            runExceptT $ chaseStatus (n-1) (show url) o
        a -> return (Right a)

rightWithBody :: (Show a, Show body) => Response body -> a -> Int -> Options -> IO (Either String LinkStruct)
rightWithBody r u n o =
    case statusCode (responseStatus r) `div` 100 :: Int of
        3 -> do
            let u' = return $ lookup hLocation (responseHeaders r)
            url <- embedMaybe (userError "bad URL") u'
            runExceptT $ chaseBody (n-1) (show url) o
        4 -> return $ Left "ERROR: 4** nope"
        5 -> return $ Left "ERROR: 5** dead"
        _ -> do
                extractTitles r >>= \ts -> do
                    maybePrintSomething u (getServer r) (getContentLength r) (destroyCookieJar (responseCookieJar r)) (Just ts) o
                    putStrLn (serverLine u (getServer r) (Just ts))
                    -- to exit here with the serverLine
                    -- return $ Right (serverLine u (getServer r) (Just ts))
                extractLinks (show u) r o >>= \tl -> do
                    printLinksOrgMode u r tl o
                    when (optVerbose o) $ do
                      putStrLn "picking random url among"
                      print tl
                    if not (null tl) then
                      pick tl >>= \next -> return $ Right next
                    else return $ Left "no links"

pick :: [a] -> IO a
{-# ANN pick ("HLint: ignore Use <$>" :: String) #-}
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

getServer :: Response body -> Maybe BS.ByteString
getServer r = lookup hServer (responseHeaders r)

getContentLength :: Response body -> Maybe BS.ByteString
getContentLength r = lookup hContentLength (responseHeaders r)

leftWithError :: (MonadError IOError m, MonadIO m) => String -> m b
leftWithError ll = do
    liftIO $ putStrLn ("Something went wrong: " ++ show ll :: String)
    throwError $ userError ll
