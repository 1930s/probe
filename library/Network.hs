-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#lambdacase
{-# LANGUAGE LambdaCase #-}

module Network where

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
import Network.HTTP.Simple ( Response )

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
