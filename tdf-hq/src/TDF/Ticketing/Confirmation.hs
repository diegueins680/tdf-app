{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Ticketing.Confirmation
  ( Confirmation(..)
  , confirmationBody
  , processConfirmationWith
  , startTicketConfirmationWorker
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception.Safe (tryAny)
import Control.Monad (forever, void, when)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.Persist
import Database.Persist.Sql
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import System.IO (hPutStrLn, stderr)

import TDF.Config (AppConfig(..), EmailConfig)
import TDF.DB (Env(..))
import qualified TDF.Email as Email
import TDF.Internationalization (formatMoney)
import qualified TDF.Models.SocialEventsModels as M
import qualified TDF.Ticketing.Transfer as Transfer

data Confirmation = Confirmation
  { confirmationName :: Text
  , confirmationEmail :: Text
  , confirmationEvent :: Text
  , confirmationDate :: Text
  , confirmationTier :: Text
  , confirmationQuantity :: Int
  , confirmationTotal :: Text
  , confirmationCodes :: [Text]
  , confirmationEventUrl :: Text
  , confirmationAppUrl :: Text
  } deriving (Eq, Show)

confirmationBody :: Confirmation -> [Text]
confirmationBody receipt =
  [ "Tu compra está confirmada."
  , "Evento: " <> confirmationEvent receipt
  , "Fecha: " <> confirmationDate receipt
  , "Entradas compradas: " <> T.pack (show (confirmationQuantity receipt))
      <> " x " <> confirmationTier receipt
  , "Total pagado: " <> confirmationTotal receipt
  , ""
  , "Códigos de tus entradas disponibles:"
  ] <> confirmationCodes receipt <>
  [ ""
  , "Puedes presentar estos códigos en el acceso. Cada entrada admite un solo ingreso."
  , "No compartas este correo ni los códigos: permiten utilizar tus entradas."
  , "Las entradas transferidas, canceladas o ya utilizadas no se incluyen en esta lista."
  , "Consulta horarios, ubicación y condiciones en la página del evento:"
  , confirmationEventUrl receipt
  , ""
  , "No necesitas instalar una app para usar tu entrada."
  , "Si quieres conocer TDF para Android e iOS: " <> confirmationAppUrl receipt
  ]

-- The queue contains only a canonical order reference. Resolve recipient and
-- credentials at delivery time; never retain a second copy of buyer PII or QR.
loadConfirmation :: Env -> Int64 -> IO (Maybe Confirmation)
loadConfirmation Env{envPool,envConfig} orderId = runSqlPool (do
  let key = toSqlKey orderId
  paid <- rawSql
    "SELECT r.order_id FROM event_ticket_checkout_runtime r JOIN event_ticket_order o ON o.id=r.order_id WHERE r.order_id=? AND r.payment_status='paid' AND r.fulfillment_status='issued' AND o.status='paid'"
    [PersistInt64 orderId] :: SqlPersistT IO [Single Int64]
  order <- get key
  case (paid,order) of
    ([_],Just row) -> do
      event <- get (M.eventTicketOrderEventId row)
      tier <- get (M.eventTicketOrderTierId row)
      tickets <- selectList [M.EventTicketOrderRefId ==. key] [Asc M.EventTicketId]
      dates <- rawSql
        ("SELECT to_char(e.start_time AT TIME ZONE COALESCE(tz.name,'UTC'),'YYYY-MM-DD HH24:MI')"
          <> " || ' (' || COALESCE(tz.name,'UTC') || ')' FROM social_event e"
          <> " LEFT JOIN pg_timezone_names tz ON tz.name=e.timezone WHERE e.id=?")
        [toPersistValue (M.eventTicketOrderEventId row)] :: SqlPersistT IO [Single Text]
      let codes = [M.eventTicketCode ticket | Entity _ ticket <- tickets,
            M.eventTicketStatus ticket == "issued", M.eventTicketCheckedInAt ticket == Nothing,
            Transfer.retainedByBuyer ticket]
          base = T.dropWhileEnd (== '/') (fromMaybe "https://www.tdfrecords.net" (appBaseUrl envConfig))
      pure $ case (event,tier,dates,M.eventTicketOrderBuyerEmail row,codes) of
        (Just e,Just t,[Single date],Just email,_:_) | not (T.null (T.strip email)) -> Just Confirmation
          { confirmationName=fromMaybe "" (M.eventTicketOrderBuyerName row)
          , confirmationEmail=email,confirmationEvent=M.socialEventTitle e
          , confirmationDate=date,confirmationTier=M.eventTicketTierName t
          , confirmationQuantity=M.eventTicketOrderQuantity row
          , confirmationTotal=formatMoney "es-EC" (M.eventTicketOrderCurrency row) (fromIntegral (M.eventTicketOrderAmountCents row))
          , confirmationCodes=codes
          , confirmationEventUrl=base <> "/eventos/" <> T.pack (show (fromSqlKey (M.eventTicketOrderEventId row)))
          , confirmationAppUrl=base <> "/app"
          }
        _ -> Nothing
    _ -> pure Nothing) envPool

-- One short database claim, then bounded SMTP outside the transaction. Completion
-- is fenced by the lease. A crash after SMTP acceptance can cause a duplicate
-- email on retry; it cannot issue another ticket or charge the buyer again.
processConfirmationWith :: Env -> (Confirmation -> IO ()) -> IO Bool
processConfirmationWith env@Env{envPool} send = do
  lease <- UUID.toText <$> UUID.nextRandom
  claimed <- runSqlPool (rawSql "SELECT event_ticket_claim_confirmation(?::uuid)"
    [PersistText lease] :: SqlPersistT IO [Single (Maybe Int64)]) envPool
  case claimed of
    [Single (Just orderId)] -> do
      result <- tryAny $ do
        receipt <- loadConfirmation env orderId
        case receipt of
          Nothing -> pure "no_eligible_tickets"
          Just value -> do
            delivered <- timeout (30*1000000) (send value)
            pure $ case delivered of Just () -> "accepted"; Nothing -> "delivery_failed"
      let outcome = either (const "delivery_failed") id result
      _ <- runSqlPool (rawSql "SELECT event_ticket_finish_confirmation(?,?::uuid,?)"
        [PersistInt64 orderId,PersistText lease,PersistText outcome] :: SqlPersistT IO [Single Bool]) envPool
      pure True
    _ -> pure False

sendConfirmation :: EmailConfig -> Confirmation -> IO ()
sendConfirmation cfg receipt = Email.sendTransactionalEmail cfg
  (confirmationName receipt) (confirmationEmail receipt)
  ("Tus entradas para " <> confirmationEvent receipt)
  "Confirmación de compra de entradas"
  (confirmationBody receipt) (Just (confirmationEventUrl receipt))

startTicketConfirmationWorker :: Env -> IO ()
startTicketConfirmationWorker env@Env{envConfig} = do
  enabled <- (== Just "true") <$> lookupEnv "TICKET_CONFIRMATION_EMAIL_ENABLED"
  when enabled $ case emailConfig envConfig of
    Nothing -> hPutStrLn stderr "[TicketConfirmation] Worker disabled: SMTP configuration missing; queued receipts retained"
    Just cfg -> void . forkIO . forever $ do
      result <- tryAny (processConfirmationWith env (sendConfirmation cfg))
      case result of
        Left _ -> void $ tryAny $ hPutStrLn stderr "[TicketConfirmation] Worker iteration failed; queued receipts retained"
        Right _ -> pure ()
      threadDelay (5*1000000)
