{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module TDF.Cron
  ( startCoursePaymentReminderJob
  ) where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Exception       (SomeException, try)
import           Control.Monad           (forever, void, when)
import           Data.Foldable           (for_)
import           Data.List               (foldl')
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time               ( LocalTime(..)
                                         , TimeOfDay(..)
                                         , UTCTime
                                         , ZonedTime(..)
                                         , addDays
                                         , diffUTCTime
                                         , getCurrentTime
                                         , getZonedTime
                                         , zonedTimeToUTC
                                         )
import           Database.Persist        ( (!=.)
                                         , (==.)
                                         , Entity(..)
                                         , SelectOpt(..)
                                         , count
                                         , selectList
                                         )
import           Database.Persist.Sql    (runSqlPool)
import           System.IO               (hPutStrLn, stderr)

import           TDF.DB                  (Env(..))
import qualified TDF.Email.Service       as EmailSvc
import qualified TDF.LogBuffer           as LogBuf
import qualified TDF.ModelsExtra         as ME
import           TDF.Routes.Courses      (CourseMetadata(..))
import           TDF.Server              ( buildLandingUrl
                                         , courseMetadataFor
                                         , normalizeSlug
                                         , productionCourseCapacity
                                         , productionCoursePrice
                                         , productionCourseSlug
                                         )

-- | Launch a background thread that sends payment reminders every day at 09:00 (server local time).
startCoursePaymentReminderJob :: Env -> IO ()
startCoursePaymentReminderJob env = do
  void (forkIO (cronLoop env))
  LogBuf.addLog LogBuf.LogInfo "[Cron][CoursePayment] Scheduled daily reminder at 09:00 local time."

cronLoop :: Env -> IO ()
cronLoop env = forever $ do
  target <- nextNineAMUtc
  waitUntil target
  result <- try (sendCoursePaymentReminders env) :: IO (Either SomeException ())
  case result of
    Left err -> do
      let msg = "[Cron][CoursePayment] Job failed: " <> T.pack (show err)
      hPutStrLn stderr (T.unpack msg)
      LogBuf.addLog LogBuf.LogError msg
    Right () ->
      LogBuf.addLog LogBuf.LogInfo "[Cron][CoursePayment] Reminder job finished."

-- | Compute the next 09:00 in UTC, based on the server's local timezone.
nextNineAMUtc :: IO UTCTime
nextNineAMUtc = do
  now <- getZonedTime
  let ZonedTime (LocalTime day tod) tz = now
      targetDay = if tod < nineAM then day else addDays 1 day
      targetZoned = ZonedTime (LocalTime targetDay nineAM) tz
  pure (zonedTimeToUTC targetZoned)
  where
    nineAM = TimeOfDay 9 0 0

waitUntil :: UTCTime -> IO ()
waitUntil targetUtc = do
  now <- getCurrentTime
  let diffSeconds = diffUTCTime targetUtc now
      waitMicros = max 0 (ceiling (realToFrac diffSeconds * (1e6 :: Double)) :: Int)
  when (waitMicros > 0) (threadDelay waitMicros)

sendCoursePaymentReminders :: Env -> IO ()
sendCoursePaymentReminders Env{..} = do
  let slugVal = normalizeSlug productionCourseSlug
      emailSvc = EmailSvc.mkEmailService envConfig
      landingUrl = buildLandingUrl envConfig
      courseTitle = maybe "Curso de Producción Musical" title (courseMetadataFor envConfig Nothing slugVal)
  paidCount <- runSqlPool (count [ ME.CourseRegistrationCourseSlug ==. slugVal
                                 , ME.CourseRegistrationStatus ==. "paid"
                                 ]) envPool
  pendingRegs <- runSqlPool
    (selectList
      [ ME.CourseRegistrationCourseSlug ==. slugVal
      , ME.CourseRegistrationStatus ==. "pending_payment"
      , ME.CourseRegistrationEmail !=. Nothing
      ]
      [Desc ME.CourseRegistrationCreatedAt])
    envPool
  let recipients = dedupeByEmail pendingRegs
      remainingSeats = max 0 (productionCourseCapacity - paidCount)
      introMsg = T.concat
        [ "[Cron][CoursePayment] Pending with email: "
        , T.pack (show (length recipients))
        , "; seats left: "
        , T.pack (show remainingSeats)
        , "."
        ]
  LogBuf.addLog LogBuf.LogInfo introMsg
  case EmailSvc.esConfig emailSvc of
    Nothing -> do
      let msg = "[Cron][CoursePayment] SMTP not configured; skipped sending reminders."
      hPutStrLn stderr (T.unpack msg)
      LogBuf.addLog LogBuf.LogWarning msg
    Just _ ->
      for_ recipients $ \(nameTxt, emailTxt) -> do
        sendResult <- try
          (EmailSvc.sendCoursePaymentReminder
            emailSvc
            nameTxt
            emailTxt
            courseTitle
            productionCoursePrice
            remainingSeats
            landingUrl)
          :: IO (Either SomeException ())
        case sendResult of
          Left err -> do
            let msg = "[Cron][CoursePayment] Failed to email " <> emailTxt <> ": " <> T.pack (show err)
            hPutStrLn stderr (T.unpack msg)
            LogBuf.addLog LogBuf.LogError msg
          Right () ->
            LogBuf.addLog LogBuf.LogInfo ("[Cron][CoursePayment] Reminder sent to " <> emailTxt)

-- | Keep only the latest registration per email (case-insensitive).
dedupeByEmail :: [Entity ME.CourseRegistration] -> [(Text, Text)]
dedupeByEmail =
  Map.elems . foldl' go Map.empty
  where
    go acc (Entity _ reg) =
      case ME.courseRegistrationEmail reg >>= nonEmpty of
        Nothing       -> acc
        Just rawEmail ->
          let key = T.toLower rawEmail
              entry = (fromMaybe "" (ME.courseRegistrationFullName reg), rawEmail)
          in Map.insertWith (\_ old -> old) key entry acc
    nonEmpty txt =
      let trimmed = T.strip txt
      in if T.null trimmed then Nothing else Just trimmed
