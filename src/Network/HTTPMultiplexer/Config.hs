{-# LANGUAGE OverloadedStrings #-}

module Network.HTTPMultiplexer.Config where

import Control.Applicative
import Data.Aeson
import Data.Yaml
import System.IO
import System.Exit


withConfigFile :: FilePath -> (Config -> IO a) -> IO a
withConfigFile configFile action = do
  c <- decodeFileEither configFile
  case c of
    Left err -> do
        hPutStrLn stderr ("error parsing configuration file " ++ configFile ++ ": " ++ show err)
        exitFailure
    Right config ->  action config


data Config = Config {
  cfgBackendAddress :: [String]
, cfgLogFilePath :: String
} deriving (Eq, Show)


instance FromJSON Config where
  parseJSON (Object m) = Config
    <$> m .: "backends"
    <*> m .: "log_filepath"
  parseJSON _ = empty
