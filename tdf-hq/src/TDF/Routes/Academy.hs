{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.Routes.Academy where

import           Prelude
import           GHC.Generics (Generic)
import           Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
import           Servant
import           Database.Persist
import           Database.Persist.Sql (fromSqlKey, toSqlKey, SqlPersistT)
import           Data.Text (Text)

-- DTOs
data EnrollReq = EnrollReq 
  { email :: Text
  , role :: Text
  , platform :: Maybe Text
  , referralCode :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON EnrollReq
instance ToJSON EnrollReq

type AppM = SqlPersistT IO

type API =
       "academy" :> "enroll" :> ReqBody '[JSON] EnrollReq :> Post '[JSON] NoContent
  :<|> "academy" :> "microcourse" :> Capture "slug" Text :> Get '[JSON] Value
  :<|> "academy" :> "progress" :> ReqBody '[JSON] Value :> Post '[JSON] NoContent
  :<|> "referrals" :> "claim" :> ReqBody '[JSON] Value :> Post '[JSON] NoContent
  :<|> "cohorts" :> "next" :> Get '[JSON] Value

-- Implementaciones: (pseudocódigo con runDB …) — focos mínimos para MVP
server :: ServerT API AppM
server =
  enrollH :<|> microH :<|> progressH :<|> claimH :<|> nextCohortH
  where
    enrollH (EnrollReq e r p mref) = do
      -- upsert de usuario y (si hay) registro de referral
      pure NoContent
    microH slug = do
      -- select de microcurso y lecciones por día
      pure (object ["slug" .= slug, "title" .= ("Release Readiness"::Text), "lessons" .= ([]::[Value])])
    progressH v = pure NoContent
    claimH v    = pure NoContent
    nextCohortH = pure (object ["slug" .= ("rr-sprint"::Text), "seatsLeft" .= (22::Int)])
