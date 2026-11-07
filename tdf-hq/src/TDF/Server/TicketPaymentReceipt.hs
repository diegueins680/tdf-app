{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.TicketPaymentReceipt
  ( TicketPaymentReceipt(..), validateReceipt, receiptObjectKey, prepareReceipt, loadReceipt
  ) where

import Control.Exception (IOException, bracketOnError, finally, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON(..), ToJSON, defaultOptions, genericParseJSON, rejectUnknownFields)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Servant (Handler, err404, err503, ServerError(..), throwError)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import System.IO (hClose, openBinaryTempFile)

import TDF.Config (assetsRootDir, isProductionRuntime, productionRuntimeKeys)
import TDF.DB (Env(..))

-- Returned only by the authorized event handler, never by public assets.
data TicketPaymentReceipt = TicketPaymentReceipt
  { contentType :: Text
  , base64 :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON TicketPaymentReceipt
instance FromJSON TicketPaymentReceipt where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }

validateReceipt :: TicketPaymentReceipt -> Either Text (Text, BS.ByteString)
validateReceipt TicketPaymentReceipt{..}
  | T.length base64 > 6990508 = Left "El comprobante no debe superar 5 MB."
  | otherwise = do
      bytes <- either (const (Left "El archivo del comprobante no es válido.")) Right
        (BAE.convertFromBase BAE.Base64 (TE.encodeUtf8 base64))
      unless (BS.length bytes > 0 && BS.length bytes <= 5 * 1024 * 1024) $
        Left "El comprobante debe contener entre 1 byte y 5 MB."
      extension <- case contentType of
        "image/jpeg" | BS.pack [255,216,255] `BS.isPrefixOf` bytes -> Right "jpg"
        "image/png" | BS.pack [137,80,78,71,13,10,26,10] `BS.isPrefixOf` bytes -> Right "png"
        "application/pdf" | "%PDF-" `BS.isPrefixOf` bytes -> Right "pdf"
        _ -> Left "Adjunta un comprobante en JPG, PNG o PDF válido."
      pure (extension, bytes)

receiptObjectKey :: Int64 -> (Text, BS.ByteString) -> Text
receiptObjectKey orderId (extension, bytes) =
  T.pack (show orderId) <> "/" <> T.pack (show (hash bytes :: Digest SHA256)) <> "." <> extension

type AppM = ReaderT Env Handler

privateRoot :: AppM FilePath
privateRoot = do
  configured <- liftIO (lookupEnv "TICKET_PAYMENT_EVIDENCE_DIR")
  environment <- liftIO $ traverse (\key -> do value <- lookupEnv key; pure (key, value)) productionRuntimeKeys
  let production = isProductionRuntime environment
  when (production && maybe True null configured) $
    throwError err503 { errBody = "TICKET_PAYMENT_EVIDENCE_DIR must point to durable private storage" }
  let root = fromMaybe "uploads/ticket-payment-evidence" configured
  when (null root) $ throwError err503 { errBody = "Private receipt storage is not configured" }
  publicRoot <- asks (assetsRootDir . envConfig)
  result <- liftIO $ tryIO $ do
    createDirectoryIfMissing True root
    private <- canonicalizePath root
    public <- canonicalizePath publicRoot
    pure (private, public)
  (private, public) <- either (const storageUnavailable) pure result
  when (private == public || addTrailingPathSeparator public `isPrefixOf` addTrailingPathSeparator private) $
    throwError err503 { errBody = "Receipt storage must be outside public assets" }
  pure private

storageUnavailable :: AppM a
storageUnavailable = throwError err503 { errBody = "Private receipt storage is unavailable" }

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- Prepare storage only after capability authentication. Execute the write inside
-- the evidence transaction after locking/checking its state, so a failed write
-- cannot submit evidence and a conflicting replay cannot replace its receipt.
prepareReceipt :: Int64 -> (Text, BS.ByteString) -> AppM (Text, IO ())
prepareReceipt orderId validated@(_, bytes) = do
  root <- privateRoot
  let key = receiptObjectKey orderId validated
      directory = root </> show orderId
      path = root </> T.unpack key
      directoryPermissions = setOwnerSearchable True . setOwnerWritable True . setOwnerReadable True $ emptyPermissions
      filePermissions = setOwnerWritable True . setOwnerReadable True $ emptyPermissions
      write = do
        setPermissions root directoryPermissions
        createDirectoryIfMissing True directory
        setPermissions directory directoryPermissions
        bracketOnError (openBinaryTempFile directory ".receipt")
          (\(temporary, handle) -> hClose handle `finally` removeFile temporary) $
          \(temporary, handle) -> do
            BS.hPut handle bytes
            hClose handle
            setPermissions temporary filePermissions
            renameFile temporary path
  pure (key, write)

loadReceipt :: Int64 -> Text -> AppM TicketPaymentReceipt
loadReceipt orderId key = do
  let parts = T.splitOn "/" key
      validName name = case T.splitOn "." name of
        [digest, extension] -> T.length digest == 64
          && T.all (`elem` ("0123456789abcdef" :: String)) digest
          && extension `elem` ["jpg", "png", "pdf"]
        _ -> False
  unless (case parts of [order, name] -> order == T.pack (show orderId) && validName name; _ -> False) $
    throwError err404 { errBody = "Receipt not found" }
  root <- privateRoot
  outcome <- liftIO $ tryIO $ do
    let path = root </> T.unpack key
    size <- getFileSize path
    if size > 5 * 1024 * 1024 then pure Nothing else Just <$> BS.readFile path
  bytes <- case outcome of
    Right (Just found) -> pure found
    _ -> throwError err404 { errBody = "Receipt file not found" }
  let mime = case takeExtension (T.unpack key) of
        ".pdf" -> "application/pdf"
        ".png" -> "image/png"
        _ -> "image/jpeg"
      receipt = TicketPaymentReceipt mime (TE.decodeUtf8 (BAE.convertToBase BAE.Base64 bytes))
  validated <- either (const (throwError err404)) pure (validateReceipt receipt)
  unless (receiptObjectKey orderId validated == key) $ throwError err404
  pure receipt
