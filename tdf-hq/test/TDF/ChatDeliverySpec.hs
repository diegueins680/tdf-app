{-# LANGUAGE OverloadedStrings #-}
module TDF.ChatDeliverySpec (spec) where

import Data.Aeson (Value(..), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Foldable (toList)
import Test.Hspec

import TDF.Chat.Delivery

spec :: Spec
spec = describe "MSG-DELIVERY-001 chat email and push reminders" $ do
  describe "environment defaults" $ do
    it "keeps every channel off when nothing is configured" $ do
      let settings = deliverySettingsFromEnv (const Nothing)
      enabledChannels settings `shouldBe` []
      dsPushTokenKey settings `shouldBe` Nothing
      dsExpoPushUrl settings `shouldBe` "https://exp.host/--/api/v2/push/send"

    it "enables push only with an encryption key of at least 32 characters" $ do
      let env short name = lookup name
            [ ("CHAT_PUSH_DELIVERY_ENABLED", "true")
            , ("NOTIFICATION_PUSH_TOKEN_KEY", if short then "too-short" else replicate 32 'k') ]
      enabledChannels (deliverySettingsFromEnv (env True)) `shouldBe` []
      enabledChannels (deliverySettingsFromEnv (env False)) `shouldBe` ["push"]

    it "accepts the usual truthy spellings for email" $
      enabledChannels (deliverySettingsFromEnv (\name ->
        if name == "CHAT_EMAIL_DELIVERY_ENABLED" then Just "TRUE" else Nothing)) `shouldBe` ["email"]

  describe "retry backoff" $
    it "doubles from one minute and is capped at one hour" $ do
      map retryDelaySeconds [1, 2, 3, 4, 5] `shouldBe` [60, 120, 240, 480, 960]
      retryDelaySeconds 50 `shouldBe` 3600
      retryDelaySeconds 0 `shouldBe` 60

  describe "Expo push tokens" $ do
    it "accepts Expo token shapes" $ do
      validExpoPushToken "ExponentPushToken[xxxxxxxxxxxxxxxxxxxxxx]" `shouldBe` True
      validExpoPushToken "ExpoPushToken[abc-DEF_123]" `shouldBe` True
    it "rejects other strings" $ do
      validExpoPushToken "" `shouldBe` False
      validExpoPushToken "ExponentPushToken[]" `shouldBe` False
      validExpoPushToken "ExponentPushToken[abc] " `shouldBe` False
      validExpoPushToken "ExponentPushToken[a b]" `shouldBe` False
      validExpoPushToken "fcm:abcdef" `shouldBe` False
      validExpoPushToken (T.pack ("ExponentPushToken[" <> replicate 300 'a' <> "]")) `shouldBe` False

  describe "push payload" $
    it "carries only the notice id as metadata, one message per device" $ do
      let payload = expoPushMessages 122 ["ExponentPushToken[a]", "ExponentPushToken[b]"]
      case payload of
        Array messages -> case toList messages of
          [Object first, Object _] -> do
            KeyMap.lookup "data" first `shouldBe` Just (object ["notificationId" .= (122 :: Int)])
            KeyMap.lookup "body" first `shouldBe` Just (String "Tienes un mensaje nuevo en TDF.")
          _ -> expectationFailure "expected two Expo messages"
        _ -> expectationFailure "expected an array of Expo messages"

  describe "email content" $ do
    it "does not include message text or sender identity placeholders" $ do
      let (subject, preheader, bodyLines) = chatEmailContent
          everything = T.toLower (T.unwords (subject : preheader : bodyLines))
      subject `shouldBe` "Tienes un mensaje nuevo en TDF"
      everything `shouldNotSatisfy` T.isInfixOf "@"
      everything `shouldSatisfy` T.isInfixOf "desactivar"
    it "links to the exact conversation" $ do
      chatThreadUrl (Just "https://tdfrecords.net/") 6 `shouldBe` Just "https://tdfrecords.net/chat?threadId=6"
      chatThreadUrl Nothing 6 `shouldBe` Nothing
      chatThreadUrl (Just "  ") 6 `shouldBe` Nothing

  describe "Expo tickets" $ do
    let body = BL.pack
          "{\"data\":[{\"status\":\"ok\",\"id\":\"x\"},\
          \{\"status\":\"error\",\"message\":\"gone\",\"details\":{\"error\":\"DeviceNotRegistered\"}},\
          \{\"status\":\"error\",\"message\":\"slow\",\"details\":{\"error\":\"MessageRateExceeded\"}}]}"
    it "parses ok, unregistered and other errors in order" $
      parseExpoTickets body `shouldBe`
        Just [TicketOk, TicketDeviceNotRegistered, TicketError "MessageRateExceeded"]
    it "rejects responses without a ticket array" $ do
      parseExpoTickets "{\"errors\":[{\"code\":\"PUSH_TOO_MANY_EXPERIENCE_IDS\"}]}" `shouldBe` Nothing
      parseExpoTickets "not json" `shouldBe` Nothing
    it "is accepted when any device accepted" $
      classifyExpoTickets [TicketDeviceNotRegistered, TicketOk] `shouldBe` ExpoAccepted
    it "skips when every device is unregistered" $
      classifyExpoTickets [TicketDeviceNotRegistered, TicketDeviceNotRegistered] `shouldBe` ExpoNoDevice
    it "retries transient provider errors and empty answers" $ do
      classifyExpoTickets [TicketError "MessageRateExceeded"] `shouldBe` ExpoRetry "provider_error"
      classifyExpoTickets [] `shouldBe` ExpoRetry "provider_error"
    it "round-trips through JSON encoding of the payload" $
      (decode (encode (expoPushMessages 1 ["ExponentPushToken[a]"])) :: Maybe Value) `shouldSatisfy` (/= Nothing)
