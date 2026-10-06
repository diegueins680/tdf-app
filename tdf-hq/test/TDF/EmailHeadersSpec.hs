{-# LANGUAGE OverloadedStrings #-}
module TDF.EmailHeadersSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (isInfixOf, nub)
import qualified Data.Text as T
import Data.Time (UTCTime(..), fromGregorian)
import Network.Mail.Mime (Address(..), Mail(..), emptyMail, plainPart, renderMail')
import Test.Hspec
import TDF.Email.Headers (prepareOutgoingMail, stampOutgoingMail)
import TDF.Config (parseUndeliverableRecipients, isWebadorSmtpHost)
import qualified TDF.Config as Config
import qualified TDF.Email as Email
import System.IO.Error (ioeGetErrorString)
import qualified Test.QuickCheck as QC

spec :: Spec
spec = describe "outgoing SMTP message identity" $ do
  it "normalizes suppression sets idempotently for generated valid recipients" $
    QC.property $ \(QC.NonNegative number) ->
      let address = "user" <> show (number :: Int) <> "@example.test"
      in parseUndeliverableRecipients (Just (address <> ", " <> address <> "\n"))
          == Right [T.pack address]
  it "rejects a reviewed failed address before attempting the unreachable SMTP host" $ do
    let cfg = Config.EmailConfig
          { Config.emailFromName = "TDF Records"
          , Config.emailFromAddress = "info@tdfrecords.net"
          , Config.smtpHost = "127.0.0.1"
          , Config.smtpPort = 1
          , Config.smtpUsername = "test"
          , Config.smtpPassword = "test"
          , Config.smtpUseTLS = False
          , Config.smtpUndeliverableRecipients = ["broken@example.test"]
          }
    Email.sendTestEmail (Just cfg) "" " Broken@Example.test " "Test" ["Do not send"] Nothing
      `shouldThrow` (\err -> ioeGetErrorString err ==
        "Email recipient has a confirmed delivery failure; address review required")
  it "normalizes reviewed hard failures and rejects malformed suppression configuration" $ do
    parseUndeliverableRecipients Nothing `shouldBe` Right []
    parseUndeliverableRecipients (Just " Broken@Example.test,broken@example.test\nother@example.test ")
      `shouldBe` Right ["broken@example.test", "other@example.test"]
    parseUndeliverableRecipients (Just "not-an-address") `shouldBe`
      Left "SMTP_UNDELIVERABLE_RECIPIENTS must contain valid email addresses"
  it "recognizes the documented mailbox provider without matching unrelated hostnames" $ do
    map isWebadorSmtpHost ["mail.webador.com", "MAIL.WEBADOR.COM.", "mail.jouwweb.nl"]
      `shouldBe` [True, True, True]
    map isWebadorSmtpHost ["notwebador.com", "webador.com.example.test", "smtp.example.test"]
      `shouldBe` [False, False, False]
  it "puts one RFC date and traceable ID on the serialized message before the relay" $ do
    let now = UTCTime (fromGregorian 2026 9 18) 58071
        (_, stamped) = stampOutgoingMail now "11111111-2222-4333-8444-555555555555"
          fixture { mailHeaders = [("Subject", "TDF test"), ("dAtE", "old"), ("message-ID", "old")] }
    wire <- LBS.unpack <$> renderMail' stamped
    let headers = takeWhile (not . null) (lines wire)
    length (filter ("Date:" `isInfixOf`) headers) `shouldBe` 1
    length (filter ("Message-ID:" `isInfixOf`) headers) `shouldBe` 1
    wire `shouldSatisfy` isInfixOf "Date: Fri, 18 Sep 2026 16:07:51 +0000\n"
    wire `shouldSatisfy` isInfixOf "Message-ID: <11111111-2222-4333-8444-555555555555@tdfrecords.net>"
    wire `shouldSatisfy` isInfixOf "Subject: TDF test"
    wire `shouldSatisfy` isInfixOf "Operational message"
    mailFrom stamped `shouldBe` mailFrom fixture
    mailTo stamped `shouldBe` mailTo fixture
  it "generates distinct attempt IDs without encoding recipients" $ do
    attempts <- sequence (replicate 32 (prepareOutgoingMail fixture))
    let ids = map fst attempts
    length (nub ids) `shouldBe` 32
    map (lookup "Message-ID" . mailHeaders . snd) attempts `shouldBe` map Just ids
    ids `shouldSatisfy` all (T.isSuffixOf "@tdfrecords.net>")
    ids `shouldSatisfy` all (not . T.isInfixOf "private-recipient")
  where
    fixture = (emptyMail (Address (Just "TDF Records") "info@tdfrecords.net"))
      { mailTo = [Address Nothing "private-recipient@example.test"]
      , mailParts = [[plainPart "Operational message"]]
      }
