module Options where

-- http://hackage.haskell.org/package/base-4.10.0.0/docs/System-Console-GetOpt.html#v:getOpt
import System.Console.GetOpt
import System.IO ( hPutStrLn, stderr)
-- https://hackage.haskell.org/package/base
-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Monad.html
-- when :: Applicative f => Bool -> f () -> f ()
import Control.Monad ( when )
import System.Environment ( getArgs
                          , getProgName
                          )
import System.Exit ( exitSuccess
                   -- , ExitCode(..)
                   -- , exitWith
                   )

data Options = Options  { optFiles :: [String]
                        , optVerbose :: Bool
                        , optErrors :: Bool
                        , optExternal :: Bool
                        , optWorkers :: IO Int
                        , optOutput :: String -> IO ()
                        }

startOptions :: Options
startOptions = Options  { optFiles = []
                        , optVerbose = False
                        , optErrors = False
                        , optExternal = False
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
                , optErrors = errors
                , optExternal = external
                , optOutput = _output } = opts
    when verbose (hPutStrLn stderr $ "Verbose is " ++ show verbose)
    when verbose (hPutStrLn stderr $ "Only errors is " ++ show errors)
    when verbose (hPutStrLn stderr $ "Only external is " ++ show external)
    when verbose (hPutStrLn stderr $ "Files are: " ++ show files)
    when verbose (workers >>= \w -> hPutStrLn stderr $ "Workers are: " ++ show w)
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
          , Option "x" ["external"]
              (NoArg (\opt -> return opt { optExternal = True }))
              "Print only externals links (naive)"
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
