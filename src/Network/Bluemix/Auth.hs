module Network.Bluemix.Auth where

import Network.HTTP.Client
import qualified Data.Text as T

data Auth service
    = Auth
    { a_username :: !T.Text
    , a_password :: !T.Text
    , a_url :: !T.Text
    , a_manager :: !Manager
    }
