{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Default

import qualified Network.HTTPMultiplexer.Config as MITM
import qualified Network.HTTPMultiplexer.Tee as MITM

import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Options.Applicative

import qualified System.Log.FastLogger as LOG


main :: IO ()
main = do 
  configFilePath <- execParser options
  MITM.withConfigFile configFilePath $ \config -> do
    let backends = MITM.cfgBackendAddress config
        log_filepath = MITM.cfgLogFilePath config
    fileLoggerSet <- LOG.newFileLoggerSet LOG.defaultBufSize log_filepath
    requestLogger <- Wai.mkRequestLogger def
    Warp.run 8000 (requestLogger (MITM.teeProxy backends fileLoggerSet))
  where 
    parser = strOption (
         long "config"
      <> noArgError ShowHelpText
      <> metavar "CONFIG"
      <> value "config/mitmproxy.yaml"
      <> help "configuration filepath"
      )
    options = info parser (fullDesc <> progDesc "http-multiplexer: http request mutiplexer")
