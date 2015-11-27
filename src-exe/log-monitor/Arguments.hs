module Arguments
  ( Arguments (..)
  , opts
  ) where

import Options.Applicative
import Data.Maybe
import Data.Time

data Arguments = Arguments
  { runRate :: NominalDiffTime
  , configFile :: FilePath
  , logFile :: FilePath
  , debug :: Bool
  }

setDefault :: Alternative m => a -> m a -> m a
setDefault def
  = fmap (fromMaybe def) . optional

parseArgs :: Parser Arguments
parseArgs
  = Arguments
      <$> ( setDefault 300 . fmap fromIntegral . option auto $
              ( short 'r'
             <> long "runRate"
             <> metavar "INT"
             <> help "Rough interval (in seconds) between runs of log-monitor (default 300s)"
              )
          )
      <*> ( setDefault "log-monitor.yaml" . strOption $
              ( short 'c'
             <> long "config-file"
             <> metavar "FILE"
             <> help "Path to config file (default log-monitor.yaml)"
              )
          )
      <*> ( setDefault "log-monitor.log" . strOption $
              ( short 'l'
             <> long "log-file"
             <> metavar "FILE"
             <> help "File to store log-monitor logs (default log-monitor.log)"
              )
          )
      <*> ( switch $
              ( short 'd'
             <> long "debug"
             <> help "Log additional debugging information"
              )
          )

opts
  = info (helper <*> parseArgs)
      ( header "Monitor hslogger-logs and email in case of error."
     <> fullDesc
     <> footer "Log file formats require $msg, $loggername, $utcTime and $prio."
      )
    
