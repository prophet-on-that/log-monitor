{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Arguments

import System.Log.Reader
import System.Log.Logger (Priority (..))
import qualified Data.Text as T
import Data.Time
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.Attoparsec.Text.Lazy
import Control.Exception
import Data.Typeable
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Network.HostName
import Network.Mail.Mime
import Control.Monad
import Data.Monoid
import Data.Text.Lazy.Builder (toLazyText, fromText)
import Data.Yaml
import GHC.Generics
import System.Directory (doesFileExist)
import Options.Applicative (execParser)
import Control.Concurrent (threadDelay)

data LogMonitorException
  = LogParsingFailed SourceName String
  deriving (Show, Typeable)

instance Exception LogMonitorException           

type SourceName = T.Text

deriving instance Generic Priority
deriving instance FromJSON Priority
  
data Source = Source
  { sourceName :: SourceName
  , sourcePath :: FilePath
  , formatString :: FormatString
  , minPriority :: Priority
  } deriving (Generic, FromJSON)

data Config = Config
  { recipients :: [T.Text]
  , sources :: [Source]
  } deriving (Generic, FromJSON)
  
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a Nothing
  = Left a
maybeToEither _ (Just b)
  = Right b

readSource
  :: UTCTime -- ^ Lower bound on message time (exclusive)
  -> UTCTime -- ^ Upper bound on message time (inclusive)
  -> Source
  -> IO (Map Priority [LogMessage]) -- ^ Log message are stored in descending order of timestamp
readSource minTime maxTime source = do 
  lts <- L.lines <$> L.readFile (sourcePath source)
  let
    messageMap = do
      parser <- logMessageParser (formatString source) loggerNameParser
      let
        f messageMap lt = do
          lm <- eitherResult . parse parser $ lt
          timestamp' <- maybeToEither "No timestamp in format" . fmap zonedTimeToUTC . timestamp $ lm
          priority' <- maybeToEither "No priority in format" . priority $ lm
          if minTime < timestamp' && timestamp' <= maxTime && priority' >= (minPriority source)
            then
              return $ Map.adjust (lm :) priority' messageMap
            else
              return messageMap
      foldlM f Map.empty lts
  case messageMap of
    Left err ->
      throw $ LogParsingFailed (sourceName source) err
    Right messageMap' ->
      return messageMap'
  where
    loggerNameParser
      = takeTill isHorizontalSpace

toMail
  :: HostName
  -> [T.Text] -- ^ Recipient email addresses
  -> [(Source, Map Priority [LogMessage])]
  -> Mail
toMail (T.pack -> hostName) addrs messageMaps 
  = Mail hostAddress recipients [] [] [] [[plainPart body]]
  where
    hostAddress
      = Address (Just $ "Log Monitor " <> hostName) ("noReply@" <> hostName)
    recipients
      = map (Address Nothing) addrs
    body
      = L.unlines . map toMessage $ messageMaps
      where
        toMessage :: (Source, Map Priority [LogMessage]) -> L.Text
        toMessage (toLazyText . fromText . sourceName -> sourceName', messageMap) 
          = sourceName' <> ": " <> priorityPhrases
          where
            priorityPhrases
              = L.intercalate ", " $ Map.foldMapWithKey ((return .) . buildPhrase) messageMap
              where
                buildPhrase prio (length -> msgCount)
                  = L.pack $ show prio <> " - " <> show msgCount 
  
main = do
  arguments <- execParser Arguments.opts
  config <- decodeFileEither (Arguments.configFile arguments)
  case config of
    Left err ->
      putStrLn . prettyPrintParseException $ err
    Right (Config recipients' sources') -> do
      let
        runRate
          = Arguments.runRate arguments
            
        monitor :: UTCTime -> IO ()
        monitor previousTime = do
          sources'' <- filterM (doesFileExist . sourcePath) sources'
          now <- getCurrentTime
          messageMaps <- zip sources'' <$> mapM (readSource previousTime now) sources''
          let
            messageMaps'
              = filter (not . Map.null . snd) messageMaps
          when (not . null $ messageMaps') $ do
            hostName <- getHostName
            renderSendMail $ toMail hostName recipients' messageMaps'
          threadDelay (truncate $ 1000000 * runRate)
          monitor now

      now <- addUTCTime (-runRate) <$> getCurrentTime
      monitor now
        
      
