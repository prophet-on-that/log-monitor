{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Arguments

import System.Log.Reader hiding (LogMessage (..))
import qualified System.Log.Reader as R
import qualified Data.Text as T
import Data.Time
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.Attoparsec.Text.Lazy (eitherResult, takeTill, parse, isHorizontalSpace)
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
import System.Log.Logger
import System.Log.Handler.Simple (fileHandler)
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import Data.List (sortBy)
import Data.Ord (comparing, Down (..))
import Control.Concurrent.Async (mapConcurrently)

data LogMonitorException
  = LogParsingFailed SourceName String
  deriving (Show, Typeable)

instance Exception LogMonitorException

data LogMessage = LogMessage
  { message :: T.Text
  , loggerName :: T.Text
  , priority :: Priority
  , timestamp :: UTCTime
  } deriving (Show)

type SourceName = T.Text

deriving instance Generic Priority
deriving instance FromJSON Priority
  
data Source = Source
  { sourceName :: SourceName
  , sourcePath :: FilePath
  , formatString :: FormatString
  , minPriority :: Priority
  } deriving (Show, Generic, FromJSON)

data Config = Config
  { recipients :: [T.Text]
  , sources :: [Source]
  } deriving (Show, Generic, FromJSON)
  
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a Nothing
  = Left a
maybeToEither _ (Just b)
  = Right b

-- | May throw 'LogMonitorException'
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
          message' <- maybeToEither "No message in format" . R.message $ lm
          loggerName' <- maybeToEither "No loggerName in format" . R.loggerName $ lm
          timestamp' <- maybeToEither "No timestamp in format" . fmap zonedTimeToUTC . R.timestamp $ lm
          priority' <- maybeToEither "No priority in format" . R.priority $ lm
          if minTime < timestamp' && timestamp' <= maxTime && priority' >= (minPriority source)
            then do
              let
                newMessage
                  = LogMessage message' loggerName' priority' timestamp'
                alter Nothing
                  = Just [newMessage]
                alter (Just lms)
                  = Just $ newMessage : lms
              return $ Map.alter alter priority' messageMap
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
          = L.unlines $ header : L.empty : samples
          where
            header :: L.Text
            header
              = "Exceptions in " <> sourceName' <> ": " <> priorityPhrases
              where
                priorityPhrases
                  = L.intercalate ", " $ Map.foldMapWithKey (((return . L.pack) .) . buildPhrase) messageMap
                  where
                    buildPhrase prio (length -> 1)
                      = "1 " <> show prio
                    buildPhrase prio (length -> msgCount)
                      = show msgCount <> " " <> show prio <> "s"

            samples :: [L.Text]
            samples
              = map (toLazyText . fromText . buildSample) . take sampleCount . concatMap snd . sortBy (comparing $ Down . fst) . Map.toList $ messageMap
              where
                sampleCount
                  = 5
                    
                buildSample :: LogMessage -> T.Text
                buildSample lm 
                  = T.take maxChars $ (T.pack . show . priority) lm <> " " <> loggerName lm <> ": " <> message lm
                  where
                    maxChars
                      = 120
  
main = do
  arguments <- execParser Arguments.opts
  config <- decodeFileEither (Arguments.configFile arguments)
  case config of
    Left err ->
      putStrLn . prettyPrintParseException $ err
    Right (Config recipients' sources') -> do
      initLogging (Arguments.debug arguments) (Arguments.logFile arguments)
      let
        runRate
          = Arguments.runRate arguments
            
        monitor :: UTCTime -> IO ()
        monitor previousTime = do
          debugM logHandler "Monitoring.."
          sources'' <- filterM (exists . sourcePath) sources'
          now <- getCurrentTime
          let
            handler (LogParsingFailed (T.unpack -> sourceName') e) = do
              warningM (logHandler <> "." <> sourceName') $ "Log parsing failed: " <> e
              return Map.empty

          messageMaps <- fmap (zip sources'') $ mapConcurrently (handle handler . readSource previousTime now) sources''
          let
            messageMaps'
              = filter (not . Map.null . snd) messageMaps
          when (not . null $ messageMaps') $ do
            hostName <- getHostName
            infoM logHandler "Mailing.."
            let
              mail
                = toMail hostName recipients' messageMaps'
              mailWithSubject
                = mail
                    { mailHeaders = subject : mailHeaders mail }
                where
                  subject
                    = ("Subject", subjectLine)
                    where
                      subjectLine
                        = "Exceptions in " <> T.intercalate ", " (map (sourceName . fst) . toList $ messageMaps)
                      
            renderSendMail mailWithSubject
          threadDelay (truncate $ 1000000 * runRate)
          monitor now

      minTime <- addUTCTime (-runRate) <$> getCurrentTime
      monitor minTime 
  where
    initLogging :: Bool -> FilePath -> IO ()
    initLogging debug logFile = do
      -- Do not log to the standard error
      updateGlobalLogger rootLoggerName removeHandler

      -- Log to file instead
      h <- do
        lh <- fileHandler logFile DEBUG
        return $ setFormatter lh (simpleLogFormatter "[$utcTime $loggername $prio] $msg")
      updateGlobalLogger rootLoggerName (addHandler h)

      -- Listen to all messages on 'LogMonitor' loggers
      updateGlobalLogger "LogMonitor" (setLevel $ if debug then DEBUG else INFO)

    exists :: FilePath -> IO Bool
    exists path = do
      b <- doesFileExist path
      when (not b) $
        infoM logHandler $ "File " <> path <> " does not exist"
      return b

    logHandler
      = "LogMonitor"
