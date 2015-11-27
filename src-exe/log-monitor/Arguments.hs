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
             <> help "Rough interval (in seconds) between runs of log-monitor. Default 5m."
              )
          )
      <*> ( setDefault "log-monitor.yaml" . strOption $
              ( short 'c'
             <> long "config-file"
             <> metavar "FILE"
             <> help "Path to config file (default log-monitor.yaml)"
              )
          )

opts
  = info (helper <*> parseArgs)
      ( header "Monitor hslogger-logs and email in case of error."
     <> fullDesc
      )
    
