{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Main where

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
  :: UTCTime -- ^ Lower bound on message time
  -> Source
  -> IO (Map Priority [LogMessage]) -- ^ Log message are stored in descending order of timestamp
readSource minTime source = do
  lts <- L.lines <$> L.readFile (sourcePath source)
  let
    messageMap = do
      parser <- logMessageParser (formatString source) loggerNameParser
      let
        f messageMap lt = do
          lm <- eitherResult . parse parser $ lt
          timestamp' <- maybeToEither "No timestamp in format" . timestamp $ lm
          priority' <- maybeToEither "No priority in format" . priority $ lm 
          if minTime >= zonedTimeToUTC timestamp' && priority' >= (minPriority source)
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
  let
    filePath
      = undefined
  config <- decodeFileEither filePath
  case config of
    Left err ->
      putStrLn . prettyPrintParseException $ err
    Right (Config recipients' sources') -> do
      sources'' <- filterM (doesFileExist . sourcePath) sources'
      now <- getCurrentTime
      let
        -- In minutes
        refreshTime
          = 5 
        minTime
          = addUTCTime (-60 * refreshTime) now
      messageMaps <- zip sources'' <$> mapM (readSource minTime) sources''
      let
        messageMaps'
          = filter (not . Map.null . snd) messageMaps
      when (not . null $ messageMaps') $ do
        hostName <- getHostName
        renderSendMail $ toMail hostName recipients' messageMaps'
        
      
