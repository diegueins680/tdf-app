{-# LANGUAGE OverloadedStrings #-}
module TDF.Email.Service
  ( EmailService(..)
  , mkEmailService
  , sendWelcome
  , sendPasswordReset
  , sendCourseRegistration
  , sendCoursePaymentReminder
  , sendTestEmail
  , sendMarketplaceOrder
  ) where

import Data.Text (Text)
import TDF.Config (AppConfig(..), EmailConfig)
import qualified TDF.Email as Email

data EmailService = EmailService
  { esConfig   :: Maybe EmailConfig
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

sendCourseRegistration :: EmailService -> Text -> Text -> Text -> Text -> Text -> IO ()
sendCourseRegistration svc name email courseTitle landingUrl datesSummary =
  Email.sendCourseRegistrationEmail
    (esConfig svc)
    name
    email
    courseTitle
    landingUrl
    datesSummary

sendCoursePaymentReminder :: EmailService -> Text -> Text -> Text -> Double -> Int -> Text -> IO ()
sendCoursePaymentReminder svc name email courseTitle price seatsLeft landingUrl =
  Email.sendCoursePaymentReminderEmail
    (esConfig svc)
    name
    email
    courseTitle
    price
    seatsLeft
    landingUrl

sendTestEmail :: EmailService -> Text -> Text -> Text -> [Text] -> Maybe Text -> IO ()
sendTestEmail svc name email subject bodyLines mCtaUrl =
  Email.sendTestEmail (esConfig svc) name email subject bodyLines mCtaUrl

sendMarketplaceOrder :: EmailService -> Text -> Text -> Text -> Text -> [Text] -> IO ()
sendMarketplaceOrder svc name email orderId totalDisplay items =
  Email.sendMarketplaceOrderEmail (esConfig svc) name email orderId totalDisplay items
