
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module SOP.API (SOPAPI, sopServer) where

import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Network.HTTP.Types.Status (badRequest400)
import           Network.Wai (Response)
import           Servant

import           SOP.Validation
import           SOP.GitHubDispatch

-- Types
data EvidencePayload = EvidencePayload
  { req_key :: Text
  , value   :: Maybe Text
  , url     :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON EvidencePayload

data StagePayload = StagePayload
  { toStage    :: Text
  , payload    :: Value  -- YT fields when toStage=Schedule
  } deriving (Show, Generic)
instance FromJSON StagePayload

data Compliance = Compliance
  { missing :: [Text] } deriving (Show, Generic)
instance ToJSON Compliance

data TransitionResult = TransitionResult
  { status  :: Text
  , message :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON TransitionResult

type SOPAPI =
       "projects" :> Capture "id" Int :> "sop" :> "compliance" :> QueryParam' '[Required] "stage" Text :> Get '[JSON] Compliance
  :<|> "projects" :> Capture "id" Int :> "evidence" :> ReqBody '[JSON] EvidencePayload :> PostNoContent
  :<|> "projects" :> Capture "id" Int :> "stage"    :> ReqBody '[JSON] StagePayload    :> Post '[JSON] TransitionResult

sopServer :: Server SOPAPI
sopServer =
       getCompliance
  :<|> postEvidence
  :<|> postStage

getCompliance :: Int -> Text -> Handler Compliance
getCompliance pid stTxt = do
  let stage = case T.toLower stTxt of
                "ready"      -> Ready
                "inprogress" -> InProgress
                "qc"         -> QC
                "schedule"   -> Schedule
                "published"  -> Published
                _            -> Ready
  Missing reqs <- liftIO $ listMissing pid "liveSession" stage
  pure $ Compliance { missing = fmap reqKey reqs }

postEvidence :: Int -> EvidencePayload -> Handler NoContent
postEvidence _pid _body = do
  -- TODO: upsert evidence in DB
  pure NoContent

postStage :: Int -> StagePayload -> Handler TransitionResult
postStage pid body = do
  -- Validate missing
  Missing reqs <- liftIO $ listMissing pid "liveSession" (parseStage (toStage body))
  if not (null reqs)
    then throwError err400 { errBody = encode $ object
          [ "error"   .= ("MISSING_REQUIREMENTS" :: Text)
          , "missing" .= fmap reqKey reqs ] }
    else do
      -- If Schedule, dispatch GH workflow
      case T.toLower (toStage body) of
        "schedule" -> do
          res <- liftIO $ dispatchUpload (payload body)
          case res of
            Left e  -> pure $ TransitionResult "ERROR" (Just (T.pack e))
            Right _ -> pure $ TransitionResult "OK" (Just "upload_youtube dispatched")
        _ -> pure $ TransitionResult "OK" Nothing

parseStage :: Text -> Stage
parseStage s = case T.toLower s of
  "ready"      -> Ready
  "inprogress" -> InProgress
  "qc"         -> QC
  "schedule"   -> Schedule
  "published"  -> Published
  _            -> Ready
