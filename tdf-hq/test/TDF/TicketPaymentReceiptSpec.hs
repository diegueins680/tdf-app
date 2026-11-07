{-# LANGUAGE OverloadedStrings #-}
module TDF.TicketPaymentReceiptSpec (spec) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Servant (errHTTPCode, runHandler)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import TDF.Config (AppConfig(..), loadConfig)
import TDF.DB (Env(..))
import TDF.Server.TicketPaymentReceipt

spec :: Spec
spec = describe "ticket-payment-receipts" $ do
  it "accepts PDF, PNG and JPEG signatures with matching MIME types" $ do
    validateReceipt pdf `shouldSatisfy` isRight
    validateReceipt (encoded "image/png" (BS.pack [137,80,78,71,13,10,26,10])) `shouldSatisfy` isRight
    validateReceipt (encoded "image/jpeg" (BS.pack [255,216,255,217])) `shouldSatisfy` isRight
  it "rejects malformed base64, mismatched MIME, active content and empty files" $ do
    validateReceipt (TicketPaymentReceipt "application/pdf" "not base64") `shouldSatisfy` isLeft
    validateReceipt (encoded "image/png" "%PDF-1.7") `shouldSatisfy` isLeft
    validateReceipt (encoded "image/svg+xml" "<svg/>") `shouldSatisfy` isLeft
    validateReceipt (encoded "application/pdf" BS.empty) `shouldSatisfy` isLeft
  it "rejects decoded files larger than 5 MiB" $
    validateReceipt (encoded "application/pdf" ("%PDF-" <> BS.replicate (5 * 1024 * 1024) 0)) `shouldSatisfy` isLeft
  it "binds receipt keys to both order and file contents" $ do
    let first = ("pdf", "%PDF-first")
    receiptObjectKey 3 first `shouldBe` receiptObjectKey 3 first
    receiptObjectKey 3 first `shouldNotBe` receiptObjectKey 4 first
    receiptObjectKey 3 first `shouldNotBe` receiptObjectKey 3 ("pdf", "%PDF-second")
  it "stores and reads a private receipt and refuses keys from another order" $
    withSystemTempDirectory "tdf-receipt-test" $ \root ->
      withSetting "TICKET_PAYMENT_EVIDENCE_DIR" (Just (root </> "private")) $ do
        cfg <- loadConfig
        let env = Env (error "receipt storage does not access a database") cfg{assetsRootDir = root </> "public"}
            action = do
              validated <- either (error . T.unpack) pure (validateReceipt pdf)
              (key, write) <- prepareReceipt 3 validated
              liftIO write
              stored <- loadReceipt 3 key
              pure (key, stored)
        outcome <- runHandler (runReaderT action env)
        fmap snd outcome `shouldBe` Right pdf
        case outcome of
          Right (key, _) -> do
            wrong <- runHandler (runReaderT (loadReceipt 4 key) env)
            fmap errHTTPCode (either Just (const Nothing) wrong) `shouldBe` Just 404
          Left failure -> expectationFailure (show failure)
  it "refuses traversal before consulting storage" $ do
    outcome <- runHandler (runReaderT (loadReceipt 3 "../private.pdf") (error "must not read environment"))
    fmap errHTTPCode (either Just (const Nothing) outcome) `shouldBe` Just 404
  it "refuses storage within public assets" $
    withSystemTempDirectory "tdf-receipt-public-test" $ \root ->
      withSetting "TICKET_PAYMENT_EVIDENCE_DIR" (Just (root </> "public" </> "receipts")) $ do
        cfg <- loadConfig
        let env = Env (error "unused database") cfg{assetsRootDir = root </> "public"}
        outcome <- runHandler (runReaderT (prepareReceipt 3 ("pdf", "%PDF-1.7")) env)
        fmap errHTTPCode (either Just (const Nothing) outcome) `shouldBe` Just 503
  it "requires explicitly configured private storage in production" $
    withSetting "APP_ENV" (Just "production") $ withSetting "TICKET_PAYMENT_EVIDENCE_DIR" Nothing $ do
      outcome <- runHandler (runReaderT (prepareReceipt 3 ("pdf", "%PDF-1.7")) (error "must fail before config"))
      fmap errHTTPCode (either Just (const Nothing) outcome) `shouldBe` Just 503

pdf :: TicketPaymentReceipt
pdf = encoded "application/pdf" "%PDF-1.7"

encoded :: T.Text -> BS.ByteString -> TicketPaymentReceipt
encoded mime bytes = TicketPaymentReceipt mime (TE.decodeUtf8 (BAE.convertToBase BAE.Base64 bytes))

withSetting :: String -> Maybe String -> IO a -> IO a
withSetting key value action = bracket (lookupEnv key) restore (const (restore value >> action))
  where restore = maybe (unsetEnv key) (setEnv key)
