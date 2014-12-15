{-# LANGUAGE OverloadedStrings #-}

module Network.HTTPMultiplexer.Tee where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Control.Concurrent.Async as Async

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

import qualified Network.Wai as Wai

import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.ByteString as P
import qualified Pipes.HTTP as P

import qualified System.Log.FastLogger as LOG


logRequest :: LOG.LoggerSet -> Wai.Request -> IO ()
logRequest logger_set wai_req = do
  lbs_req_body <- Wai.lazyRequestBody wai_req 
  LOG.pushLogStr logger_set $ LOG.toLogStr $ B.concat [(Wai.requestMethod wai_req), " ", (Wai.rawPathInfo wai_req), (Wai.rawQueryString wai_req), "\r\n"]
  LOG.pushLogStr logger_set $ LOG.toLogStr $ lbs_req_body
  LOG.pushLogStr logger_set $ LOG.toLogStr $ ("\r\n\r\n" :: B.ByteString)


replayHttp :: HTTP.Request -> IO LB.ByteString
replayHttp http_req =
  if HTTP.secure http_req
    then
      HTTP.withManager HTTP.tlsManagerSettings $ \m ->
        P.withHTTP http_req m $ \resp -> P.toLazyM $ HTTP.responseBody resp
    else
      HTTP.withManager HTTP.defaultManagerSettings $ \m ->
        P.withHTTP http_req m $ \resp -> P.toLazyM $ HTTP.responseBody resp


mkBackendRequest :: Wai.Request -> String -> IO HTTP.Request
mkBackendRequest wai_req backend_host = do
  lbs_req_body <- Wai.lazyRequestBody wai_req 
  init_req <- HTTP.parseUrl backend_host
  let http_req = init_req { HTTP.path = (Wai.rawPathInfo wai_req)
                          , HTTP.queryString = (Wai.rawQueryString wai_req)
                          , HTTP.requestBody = HTTP.RequestBodyLBS (lbs_req_body)
                          } :: HTTP.Request
  return http_req


teeProxy :: [String] -> LOG.LoggerSet -> Wai.Application
teeProxy backend_hosts logger_set wai_req respond = do 
  logRequest logger_set wai_req
  http_reqs <- mapM (mkBackendRequest wai_req) backend_hosts
  responses <- Async.mapConcurrently replayHttp http_reqs
  let lbs_resp = head responses
  respond $ Wai.responseLBS HTTP.status200 [] lbs_resp

