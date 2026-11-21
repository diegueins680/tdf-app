{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | Notifications a newly created public booking request sends
-- (BOOK-PUBLIC-INTAKE-001).
module TDF.Server.PublicBookingNotifications
  ( notifyPublicBookingRequest
  ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent (forkIO)
import           Control.Exception (SomeException, displayException, try)
import           Control.Monad (forM_, mfilter, unless, void)
import           Data.Char (isDigit)
import           Data.List (nub)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (getCurrentTime, hoursToTimeZone, utcToLocalTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Database.Persist (get, insert_)
import           Database.Persist.Sql (runSqlPool)
import           System.Environment (lookupEnv)
import           System.IO (hPutStrLn, stderr)

import           TDF.Catalog.Security (selectCanonicalPartyIdsByRole)
import           TDF.DB (Env(..))
import qualified TDF.DTO as DTO
import qualified TDF.Email.Service as EmailSvc
import qualified TDF.LogBuffer as LogBuf
import           TDF.Models (Party, PartyId)
import qualified TDF.Models as M
import           TDF.WhatsApp.History (OutgoingWhatsAppRecord(..), recordOutgoingWhatsAppMessage)
import           TDF.WhatsApp.Transport (WhatsAppEnv, loadWhatsAppEnv, sendWhatsAppTemplateIO, sendWhatsAppTextIO)

-- | A public booking request must reach both sides: the customer gets a
-- receipt and studio staff (admin/manager/studio manager) get an in-app
-- notification, an email and a WhatsApp alert. Each delivery is best-effort,
-- and the external ones are sent off the request thread, so a mail or WhatsApp
-- outage or delay never turns an accepted request into an error for the
-- customer.
--
-- WhatsApp only delivers business-initiated free-form text inside an open
-- 24-hour window, so staff alerts use the approved template named by
-- WHATSAPP_STAFF_ALERT_TEMPLATE (body variables: 1 service, 2 customer,
-- 3 start, 4 contact; language WHATSAPP_STAFF_ALERT_TEMPLATE_LANGUAGE,
-- default es) when it is configured, and free-form text otherwise.
notifyPublicBookingRequest :: Env -> DTO.BookingDTO -> Text -> Text -> Maybe Text -> IO ()
notifyPublicBookingRequest Env{envPool = pool, envConfig = cfg} booking customerName customerEmail mPhone = do
  now <- getCurrentTime
  let DTO.BookingDTO
        { DTO.bookingId = bookingKey
        , DTO.title = bookingTitle
        , DTO.serviceType = mServiceType
        , DTO.startsAt = bookingStart
        , DTO.notes = details
        } = booking
      emailSvc = EmailSvc.mkEmailService cfg
      serviceLabel = fromMaybe bookingTitle mServiceType
      ecuadorTime = utcToLocalTime (hoursToTimeZone (-5)) bookingStart
      startLabel = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" ecuadorTime) <> " (hora Ecuador)"
      contact = customerEmail <> maybe "" (" · " <>) mPhone
      logFailure :: Text -> Either SomeException () -> IO ()
      logFailure label result = case result of
        Left err -> logWarning (label <> ": " <> T.pack (displayException err))
        Right () -> pure ()
      logWarning :: Text -> IO ()
      logWarning detail = do
        let msg = "[Bookings] Failed to " <> detail <> " (booking " <> T.pack (show bookingKey) <> ")"
        hPutStrLn stderr (T.unpack msg)
        LogBuf.addLog LogBuf.LogWarning msg
  staffResult <- (try $ flip runSqlPool pool $ do
    groups <- mapM selectCanonicalPartyIdsByRole [M.Admin, M.Manager, M.StudioManager]
    let recipients = nub (concat groups)
    forM_ recipients $ \recipient -> insert_ M.Notification
      { M.notificationRecipientPartyId = recipient
      , M.notificationNotifType = "public_booking_request"
      , M.notificationTitle = "Nueva solicitud: " <> serviceLabel
      , M.notificationBody = customerName <> " · " <> startLabel <> " · " <> contact
      , M.notificationTargetType = Just "booking"
      , M.notificationTargetId = Just (fromIntegral bookingKey)
      , M.notificationTargetKey = Nothing
      , M.notificationIsRead = False
      , M.notificationCreatedAt = now
      }
    parties <- mapM get recipients
    pure [ (recipient, party)
         | (recipient, Just party) <- zip recipients parties
         ]) :: IO (Either SomeException [(PartyId, Party)])
  staff <- case staffResult of
    Left err -> do
      logFailure "notify staff in-app" (Left err)
      pure []
    Right rows -> pure rows
  let staffEmails =
        [ (M.partyDisplayName party, addr)
        | (_, party) <- staff
        , Just addr <- [nonBlank (M.partyPrimaryEmail party)]
        ]
      staffPhones =
        [ (partyKey, M.partyDisplayName party, phone)
        | (partyKey, party) <- staff
        , Just phone <- [nonBlank (M.partyWhatsapp party) <|> nonBlank (M.partyPrimaryPhone party)]
        ]
  -- Email and WhatsApp providers can be slow or down. They run after the
  -- response on their own thread, so the customer never waits on them and the
  -- web client's request deadline cannot turn an accepted booking into an error.
  void $ forkIO $ (>>= logFailure "deliver external alerts") $ try $ do
    forM_ (nubOrdOn (T.toLower . snd) staffEmails) $ \(staffName, staffEmail) ->
      try (EmailSvc.sendPublicBookingRequestStaff emailSvc staffName staffEmail
             serviceLabel startLabel customerName contact details)
        >>= logFailure ("email staff " <> staffEmail)
    try (EmailSvc.sendPublicBookingRequestReceived emailSvc customerName customerEmail
           serviceLabel startLabel details)
      >>= logFailure "email customer"
    unless (null staffPhones) $ do
      envResult <- try loadWhatsAppEnv :: IO (Either SomeException WhatsAppEnv)
      case envResult of
        Left err -> logFailure "load WhatsApp config" (Left err)
        Right waEnv -> do
          mTemplate <- lookupEnv "WHATSAPP_STAFF_ALERT_TEMPLATE"
          mLanguage <- lookupEnv "WHATSAPP_STAFF_ALERT_TEMPLATE_LANGUAGE"
          let template = mfilter (not . T.null) (T.strip . T.pack <$> mTemplate)
              language = fromMaybe "es" (mfilter (not . T.null) (T.strip . T.pack <$> mLanguage))
              singleLine = T.unwords . T.words
              textBody = T.intercalate "\n" $
                [ "Nueva solicitud: " <> serviceLabel
                , "Cliente: " <> customerName
                , "Fecha: " <> startLabel
                , "Contacto: " <> contact
                ]
                <> maybe [] (\ns -> ["", ns]) details
                <> ["", "Revisa la agenda en TDF HQ (/estudio/calendario)."]
          forM_ (nubOrdOn (\(_, _, phone) -> T.filter isDigit phone) staffPhones) $ \(partyKey, staffName, phone) -> do
            (sentBody, sendResult) <- case template of
              Just name -> do
                res <- sendWhatsAppTemplateIO waEnv phone name language
                  (map singleLine [serviceLabel, customerName, startLabel, contact])
                pure ("[template " <> name <> "/" <> language <> "]", res)
              Nothing -> do
                res <- sendWhatsAppTextIO waEnv phone textBody
                pure (textBody, res)
            case sendResult of
              Left err -> logWarning ("WhatsApp staff " <> staffName <> ": " <> err)
              Right _ -> pure ()
            try (void (flip runSqlPool pool (recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord
                { owrRecipientPhone = phone
                , owrRecipientPartyId = Just partyKey
                , owrRecipientName = Just staffName
                , owrRecipientEmail = Nothing
                , owrActorPartyId = Nothing
                , owrBody = sentBody
                , owrSource = Just "public_booking_request_staff_alert"
                , owrReplyToMessageId = Nothing
                , owrReplyToExternalId = Nothing
                , owrResendOfMessageId = Nothing
                , owrMetadata = Nothing
                } sendResult)))
              >>= logFailure ("record WhatsApp staff alert " <> staffName)
  where
    nonBlank = mfilter (not . T.null) . fmap T.strip
    nubOrdOn :: Eq k => (a -> k) -> [a] -> [a]
    nubOrdOn key = go []
      where
        go _ [] = []
        go seen (x:xs)
          | key x `elem` seen = go seen xs
          | otherwise = x : go (key x : seen) xs
