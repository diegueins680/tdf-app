{-# LANGUAGE OverloadedStrings #-}
module TDF.TicketConfirmationSpec (spec) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (bracket,finally)
import GHC.IO.Handle (hDuplicate,hDuplicateTo)
import System.IO (stdout,hFlush,hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import qualified TDF.Email as Email
import TDF.Ticketing.Confirmation

spec :: Spec
spec = describe "web ticket confirmation" $ do
  it "does not log recipients, financial details or bearer links when SMTP is absent" $ do
    let private = "CANARY-PRIVATE"
    output <- withSystemTempFile "ticket-log-privacy" $ \path handle -> do
      bracket (hDuplicate stdout) hClose $ \original -> do
        hDuplicateTo handle stdout
        (do
          Email.sendTicketConfirmationEmail Nothing private private private private 1 private private [private] (Just private)
          Email.sendTicketTransferNotificationEmail Nothing private private private private private private private (Just private)
          Email.sendWaitlistNotificationEmail Nothing private private private private private 1 private private (Just private)
          Email.sendRefundConfirmationEmail Nothing private private private private private (Just private) (Just private)
          ) `finally` (hFlush stdout >> hDuplicateTo original stdout)
        TIO.readFile path
    output `shouldSatisfy` (not . T.isInfixOf private)
    T.count "not sent" output `shouldBe` 4
  it "includes usable codes and the canonical public event without requiring an account or app" $ do
    let body = T.unlines (confirmationBody receipt)
    body `shouldSatisfy` T.isInfixOf "TDF-ABCDEF012345"
    body `shouldSatisfy` T.isInfixOf "https://www.tdfrecords.net/eventos/141"
    body `shouldSatisfy` T.isInfixOf "No necesitas instalar una app"
    body `shouldSatisfy` (not . T.isInfixOf "tdf://")
    body `shouldSatisfy` (not . T.isInfixOf "desde tu cuenta")
  it "keeps the optional app link and warns against sharing bearer credentials" $ do
    let body = T.unlines (confirmationBody receipt)
    body `shouldSatisfy` T.isInfixOf "https://www.tdfrecords.net/app"
    body `shouldSatisfy` T.isInfixOf "No compartas este correo ni los códigos"
  it "does not insert recipient email or a private order token into the shareable event link" $ do
    let body = T.unlines (confirmationBody receipt)
    body `shouldSatisfy` (not . T.isInfixOf "buyer@example.invalid")
    confirmationEventUrl receipt `shouldBe` "https://www.tdfrecords.net/eventos/141"
  it "distinguishes bought quantity from codes still held by the buyer" $ do
    let body = T.unlines (confirmationBody receipt{confirmationQuantity=4})
    body `shouldSatisfy` T.isInfixOf "Entradas compradas: 4"
    body `shouldSatisfy` T.isInfixOf "Las entradas transferidas, canceladas o ya utilizadas no se incluyen"

receipt :: Confirmation
receipt = Confirmation
  { confirmationName="Test Buyer",confirmationEmail="buyer@example.invalid"
  , confirmationEvent="Synthetic workshop",confirmationDate="2026-10-24 14:00 (America/Guayaquil)"
  , confirmationTier="General",confirmationQuantity=1,confirmationTotal="USD 20.00"
  , confirmationCodes=["TDF-ABCDEF012345"]
  , confirmationEventUrl="https://www.tdfrecords.net/eventos/141"
  , confirmationAppUrl="https://www.tdfrecords.net/app"
  }
