{-# LANGUAGE OverloadedStrings #-}
module TDF.Email.Service
  ( EmailService(..)
  , mkEmailService
  , sendWelcome
  , sendPasswordReset
  ) where

import Data.Text (Text)
import TDF.Config (AppConfig(..))
import qualified TDF.Email as Email

data EmailService = EmailService
  { esConfig   :: Maybe Email.EmailConfig
  , esAppBase  :: Maybe Text
  }

mkEmailService :: AppConfig -> EmailService
mkEmailService cfg = EmailService
  { esConfig  = emailConfig cfg
  , esAppBase = appBaseUrl cfg
  }

sendWelcome :: EmailService -> Text -> Text -> Text -> Text -> IO ()
sendWelcome svc name email username password =
  Email.sendWelcomeEmail
    (esConfig svc)
    name
    email
    username
    password
    (esAppBase svc)

sendPasswordReset :: EmailService -> Text -> Text -> Text -> IO ()
sendPasswordReset svc name email token =
  Email.sendPasswordResetEmail
    (esConfig svc)
    name
    email
    token
    (esAppBase svc)
