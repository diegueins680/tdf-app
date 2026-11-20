{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module TDF.Server.DatafastCheckoutContext
  ( loadDatafastCartParameters, recheckDatafastCheckout, loadDatafastClientIP
  , loadServiceDatafastCheckoutReuse ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist (PersistValue(..), get)
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql, runSqlPool)
import Servant (Handler, err400, err404, err409, err503, errBody, throwError)
import System.Environment (lookupEnv)
import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.DatafastClientIP as ClientIP
import qualified TDF.Commerce.DatafastConfig as DatafastConfig
import qualified TDF.ModelsExtra as ME
import TDF.DB (Env(..))

type AppM = ReaderT Env Handler

-- Immutable cart projection and request transport context, shared by checkout handlers.
loadDatafastCartParameters
  :: Checkout.CheckoutReference -> Int -> Text -> AppM [(ByteString, ByteString)]
loadDatafastCartParameters checkout expectedCents expectedCurrency = do
  Env{ envPool } <- ask
  (headers, rows) <- liftIO $ flip runSqlPool envPool $ do
    headers <- rawSql
      "SELECT total_minor, currency,\
      \ (subtotal_minor = total_minor AND discount_minor = 0 AND tax_minor = 0 AND fee_minor = 0)\
      \ FROM commerce_checkout_session WHERE id=?::uuid"
      [PersistText (Checkout.checkoutReferenceId checkout)]
      :: SqlPersistT IO [(Single Int64, Single Text, Single Bool)]
    rows <- rawSql
      "SELECT description, quantity, unit_amount_minor,\
      \ (subtotal_minor::numeric = quantity::numeric * unit_amount_minor::numeric\
      \ AND total_minor = subtotal_minor AND discount_minor = 0\
      \ AND tax_minor = 0 AND fee_minor = 0)\
      \ FROM commerce_checkout_line_item WHERE checkout_id=?::uuid ORDER BY line_number"
      [PersistText (Checkout.checkoutReferenceId checkout)]
      :: SqlPersistT IO [(Single Text, Single Int, Single Int64, Single Bool)]
    pure (headers, rows)
  let reject :: Text -> AppM a
      reject message = throwError err409 { errBody = BL.fromStrict (TE.encodeUtf8 message) }
  case headers of
    [(Single amount, Single currency, Single parentValid)] -> do
      unless (amount == fromIntegral expectedCents
          && currency == T.toUpper (T.strip expectedCurrency)) $
        reject "Dataweb cart does not match the immutable checkout amount and currency"
      unless (parentValid && all (\(_, _, _, Single valid) -> valid) rows) $
        reject "Dataweb cart adjustments require a qualified line and tax projection"
      let linesToSend =
            [ DatafastConfig.DatafastCartLine description quantity unit
            | (Single description, Single quantity, Single unit, _) <- rows
            ]
      either reject pure (DatafastConfig.datafastCartParameters amount currency linesToSend)
    _ -> reject "Dataweb requires one canonical checkout snapshot"


recheckDatafastCheckout
  :: AppM (Maybe saved)
  -> Either saved ClientIP.DatafastClientIP
  -> AppM (Either saved ClientIP.DatafastClientIP)
recheckDatafastCheckout reload preflight = do
  latest <- reload
  case (latest, preflight) of
    (Just saved, _) -> pure (Left saved)
    (Nothing, Right clientIP) -> pure (Right clientIP)
    (Nothing, Left _) -> throwError err409
      { errBody = "Saved Datafast checkout is no longer available" }

loadServiceDatafastCheckoutReuse :: ME.ServiceStorefrontOrderId -> AppM (Maybe Text)
loadServiceDatafastCheckoutReuse orderId = do
  Env{..} <- ask
  current <- liftIO (runSqlPool (get orderId) envPool)
    >>= maybe (throwError err404 { errBody = "Order not found" }) pure
  let status = ME.serviceStorefrontOrderStatus current
  when (status `notElem` ["awaiting_payment", "pending_payment", "payment_failed", "datafast_pending"]) $
    throwError err400 { errBody = "Order is not in a payable state" }
  pure $ if status == "datafast_pending"
    then ME.serviceStorefrontOrderDatafastCheckoutId current else Nothing


loadDatafastClientIP :: ClientIP.DatafastClientRequest -> AppM ClientIP.DatafastClientIP
loadDatafastClientIP request = do
  configured <- liftIO (lookupEnv "DATAFAST_TRUSTED_PROXY_IPS")
  policy <- either reject pure (ClientIP.parseTrustedDatafastProxies configured)
  either reject pure (ClientIP.resolveDatafastClientIP policy request)
  where
    reject message = throwError err503
      { errBody = BL.fromStrict (TE.encodeUtf8 message) }

