{-# LANGUAGE OverloadedStrings #-}
module Network.Watson.NaturalLanguage
    ( -- * Request
      QueryBody(..)
    , KeywordOptions(..)
    , Query(..)
      -- * Response
    , Emotion(..)
    , Sentiment(..)
    , Keyword(..)
    , Response(..)
    , Language(..)
      -- * API Call
    , NaturalLanguage, makeAuth, naturalLanguage, Result(..)
    )
where

import Network.Bluemix.Auth
import Network.Bluemix.Http

import Data.Aeson hiding (Result(..))
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V

data QueryBody
    = QBText !T.Text
    | QBHtml !T.Text
    | QBUrl !T.Text
    deriving (Show, Eq)

data KeywordOptions
    = KeywordOptions
    { ko_emotion :: !Bool
    , ko_sentiment :: !Bool
    , ko_limit :: !Int
    } deriving (Show, Eq)

data Query
    = Query
    { q_body :: !QueryBody
    , q_keywords :: !(Maybe KeywordOptions)
    } deriving (Show, Eq)

instance ToJSON Query where
    toJSON q =
        let body =
                case q_body q of
                  QBText t -> "text" .= t
                  QBHtml t -> "html" .= t
                  QBUrl t -> "url" .= t
            feats =
                object $
                catMaybes
                [ flip fmap (q_keywords q) $ \kwds ->
                  "keywords" .=
                    object
                    [ "sentiment" .= ko_sentiment kwds
                    , "emotion" .= ko_emotion kwds
                    , "limit" .= ko_limit kwds
                    ]
                ]
        in object ( body : "features" .= feats : [] )

data Emotion
    = Emotion
    { e_sadness :: !Double
    , e_joy :: !Double
    , e_disgust :: !Double
    , e_anger :: !Double
    } deriving (Show, Eq)

instance FromJSON Emotion where
    parseJSON =
        withObject "Emotion" $ \o ->
        Emotion
        <$> o .: "sadness"
        <*> o .: "joy"
        <*> o .: "disgust"
        <*> o .: "anger"

newtype Sentiment
    = Sentiment { unSentiment :: Double }
    deriving (Show, Eq)

instance FromJSON Sentiment where
    parseJSON =
        withObject "Sentiment" $ \o ->
        Sentiment <$> o .: "score"

data Keyword
    = Keyword
    { k_keyword :: !T.Text
    , k_relevance :: !Double
    , k_sentiment :: !(Maybe Sentiment)
    , k_emotion :: !(Maybe Emotion)
    } deriving (Show, Eq)

instance FromJSON Keyword where
    parseJSON =
        withObject "Keyword" $ \o ->
        Keyword
        <$> o .: "text"
        <*> o .: "relevance"
        <*> o .:? "sentiment"
        <*> o .:? "emotion"

data Language
    = LArabic
    | LEnglish
    | LFrench
    | LGerman
    | LItalian
    | LPortuguese
    | LRussian
    | LSpanish
    | LSwedish
    deriving (Show, Eq)

instance FromJSON Language where
    parseJSON =
        withText "Language" $ \t ->
        case t of
          "ar" -> pure LArabic
          "en" -> pure LEnglish
          "fr" -> pure LFrench
          "de" -> pure LGerman
          "it" -> pure LItalian
          "pt" -> pure LPortuguese
          "ru" -> pure LRussian
          "es" -> pure LSpanish
          "sv" -> pure LSwedish

data Response
    = Response
    { r_language :: !Language
    , r_keywords :: !(V.Vector Keyword)
    } deriving (Show, Eq)

instance FromJSON Response where
    parseJSON =
        withObject "Response" $ \o  ->
        Response
        <$> o .: "language"
        <*> o .:? "keywords" .!= V.empty

data NaturalLanguage
    = NaturalLanguage

makeAuth :: Manager -> T.Text -> T.Text -> Auth NaturalLanguage
makeAuth mgr user pass =
    Auth
    { a_username = user
    , a_password = pass
    , a_url = apiEndpointUrl
    , a_manager = mgr
    }

apiEndpointUrl :: T.Text
apiEndpointUrl = "https://gateway.watsonplatform.net/natural-language-understanding/api/v1/analyze?version=2017-02-27"

naturalLanguage :: Auth NaturalLanguage -> Query -> IO (Result Response)
naturalLanguage auth = runReq "POST" auth (a_url auth)
