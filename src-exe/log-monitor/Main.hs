{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Arguments

import System.Log.Reader hiding (LogMessage (..))
import qualified System.Log.Reader as R
import qualified Data.Text as T
import Data.Time
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.Attoparsec.Text.Lazy (maybeResult, takeTill, parse, isHorizontalSpace)
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
import Data.Maybe

data LogMonitorException
  = InvalidParser SourceName String -- ^ Failure constructing log parser from format string.
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
  , admins :: [T.Text]
  , sources :: [Source]
  } deriving (Show, Generic, FromJSON)
  
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a Nothing
  = Left a
maybeToEither _ (Just b)
  = Right b

type ParseErrorCount = Integer

-- | May throw 'LogMonitorException'
readSource
  :: UTCTime -- ^ Lower bound on message time (exclusive)
  -> UTCTime -- ^ Upper bound on message time (inclusive)
  -> Source
  -> IO (ParseErrorCount, Map Priority [LogMessage]) -- ^ Log message are stored in descending order of timestamp
readSource minTime maxTime Source {..} = do 
  lts <- L.lines <$> L.readFile sourcePath
  case logMessageParser formatString loggerNameParser of
    Left err ->
      throw $ InvalidParser sourceName err
    Right parser -> do
      let
        helper (errorCount, messageMap) lt
          = fromMaybe (errorCount + 1, messageMap) $ do
              lm <- maybeResult . parse parser $ lt
              message' <- R.message lm
              loggerName' <- R.loggerName lm
              timestamp' <- fmap zonedTimeToUTC . R.timestamp $ lm
              priority' <- R.priority lm
              if minTime < timestamp' && timestamp' <= maxTime && priority' >= minPriority
                then do
                  let
                    newMessage
                      = LogMessage message' loggerName' priority' timestamp'
                    alter Nothing
                      = Just [newMessage]
                    alter (Just lms)
                      = Just $ newMessage : lms
                  return (errorCount, Map.alter alter priority' messageMap)
                else
                  return (errorCount, messageMap)
      return $ foldl' helper (0, Map.empty) lts
  where
    loggerNameParser
      = takeTill isHorizontalSpace

toMail
  :: Address -- ^ Host address.
  -> [T.Text] -- ^ Recipient email addresses.
  -> [(Source, Map Priority [LogMessage])]
  -> Mail
toMail hostAddress addrs messageMaps 
  = Mail hostAddress recipients [] [] [] [[plainPart body]]
  where
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
                  | T.length msg > maxChars
                      = T.take maxChars msg <> "..."
                  | otherwise
                      = msg
                  where
                    maxChars
                      = 300
                    msg
                      = (T.pack . show . priority) lm <> " " <> loggerName lm <> ": " <> message lm

main = do
  Arguments {..} <- execParser opts
  config <- decodeFileEither configFile
  case config of
    Left err ->
      putStrLn . prettyPrintParseException $ err
    Right Config {..}  -> do
      initLogging debug logFile
      
      hostAddress <- do
        hostName <- T.pack <$> getHostName
        return $ Address (Just $ "Log Monitor " <> hostName) ("noReply@" <> hostName)
        
      let
        monitor :: UTCTime -> IO ()
        monitor previousTime = do
          debugM logHandler "Monitoring.."
          sources'' <- filterM (exists . sourcePath) sources
          now <- getCurrentTime
          let
            handler (InvalidParser (T.unpack -> sourceName) err) = do
              warningM (logHandler <> "." <> sourceName) $ "Invalid parser spec: " <> err
              return (0, Map.empty)

          parsed <- mapConcurrently (handle handler . readSource previousTime now) sources''

          -- Email exceptions, where present.
          let
            messageMaps
              = filter (not . Map.null . snd) . zip sources'' . map snd $ parsed
          when (not . null $ messageMaps) $ do
            infoM logHandler "Mailing log exceptions.."
            let
              mail
                = toMail hostAddress recipients messageMaps
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

          -- Email log parsing errors, where present.
          let
            withErrors
              = filter ((> 0) . snd) . zip sources'' . map fst $ parsed
          when (not . null $ withErrors) $ do
            infoM logHandler "Mailing parsing errors.."
            let
              mail
                = Mail hostAddress recipients [] [] [] [[plainPart body]]
                where
                  recipients
                    = map (Address Nothing) admins
                  body
                    = "Log parsing errors in " <> L.intercalate ", " errorSources <> "."
                    where
                      errorSources :: [L.Text]
                      errorSources
                        = map helper withErrors
                        where
                          helper (toLazyText . fromText . sourceName -> sourceName, n)
                            = sourceName <> " (" <> (L.pack . show) n <> " unparsed lines)"
              mailWithSubject
                = mail { mailHeaders = subject : mailHeaders mail }
                where
                  subject
                    = ("Subject", "Log parsing errors in " <> T.intercalate ", " (map (sourceName . fst) . toList $ withErrors))
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
