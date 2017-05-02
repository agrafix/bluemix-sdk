{-# LANGUAGE OverloadedStrings #-}
module Network.Bluemix.Http
    ( runReq
    , Result(..)
    , Manager
    )
where

import Network.Bluemix.Auth

import Data.Aeson (eitherDecode, encode, ToJSON, FromJSON)
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Result r
    = ROkay r
    | RBadResponse !T.Text
    | RBadStatus !Status
    deriving (Show, Eq)

runReq :: (ToJSON req, FromJSON r) => Method -> Auth a -> T.Text -> req -> IO (Result r)
runReq method auth url requestObject =
    do initialRequest <-
           applyBasicAuth
           (T.encodeUtf8 (a_username auth)) (T.encodeUtf8 (a_password auth))
           <$> parseRequest (T.unpack url)
       let request =
               initialRequest
               { method = method
               , requestBody = RequestBodyLBS $ encode requestObject
               , requestHeaders =
                       (requestHeaders initialRequest)
                       ++ [("Content-Type", "application/json")]
               }
       response <- httpLbs request $ a_manager auth
       let rs = responseStatus response
       case statusCode rs of
         200 ->
             case eitherDecode (responseBody response) of
               Left errMsg -> pure (RBadResponse $ T.pack errMsg)
               Right ok -> pure (ROkay ok)
         s -> pure $ RBadStatus rs
