{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module TDF.Cron
  ( startCoursePaymentReminderJob
  , startInstagramSyncJob
  ) where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Exception       (SomeException, catch, try)
import           Control.Applicative    ((<|>))
import           Control.Monad           (forever, void, when, foldM)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Foldable           (for_)
import           Data.List               (foldl')
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (catMaybes, fromMaybe)
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
                                         , getBy
                                         , insert
                                         , insert_
                                         , insertEntity
                                         , update
                                         , (=.)
                                          )
import           Database.Persist.Sql    (SqlPersistT, runSqlPool)
import           System.Random           (randomRIO)
import           System.IO               (hPutStrLn, stderr)

import           TDF.DB                  (Env(..))
import qualified TDF.Email.Service       as EmailSvc
import qualified TDF.LogBuffer           as LogBuf
import qualified TDF.ModelsExtra         as ME
import           TDF.Models
import           TDF.Services.InstagramSync (InstagramMedia(..), fetchUserMedia)
import           TDF.Routes.Courses      (CourseMetadata(..))
import           TDF.Server              ( buildLandingUrl
                                         , courseMetadataFor
                                         , loadWhatsAppEnv
                                         , toCourseMetadata
                                         , normalizeSlug
                                         , productionCourseSlug
                                         , productionCoursePrice
                                         , productionCourseCapacity
                                         )
import qualified TDF.Trials.Models       as Trials
import           TDF.Config              (instagramAppToken)

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
  let slugVal = normalizeSlug (productionCourseSlug envConfig)
      emailSvc = EmailSvc.mkEmailService envConfig
      landingUrl = buildLandingUrl envConfig
  waEnv <- loadWhatsAppEnv
  metaFromDb <- runSqlPool
    (do
      mCourse <- getBy (Trials.UniqueCourseSlug slugVal)
      case mCourse of
        Nothing -> pure Nothing
        Just (Entity cid course) -> do
          sessions <- selectList
            [Trials.CourseSessionModelCourseId ==. cid]
            [Asc Trials.CourseSessionModelOrder, Asc Trials.CourseSessionModelDate]
          syllabus <- selectList
            [Trials.CourseSyllabusItemCourseId ==. cid]
            [Asc Trials.CourseSyllabusItemOrder, Asc Trials.CourseSyllabusItemId]
          regsCount <- count
            [ ME.CourseRegistrationCourseSlug ==. slugVal
            , ME.CourseRegistrationStatus !=. "cancelled"
            ]
          let meta = toCourseMetadata envConfig waEnv course sessions syllabus
              remainingSeats = max 0 (Trials.courseCapacity course - regsCount)
          pure (Just meta{ remaining = remainingSeats })
    )
    envPool
  let meta = metaFromDb <|> courseMetadataFor envConfig Nothing slugVal
      courseTitle = maybe "Curso de Producción Musical" title meta
      priceUsd = maybe productionCoursePrice price meta
      capacityVal = maybe productionCourseCapacity capacity meta
  totalCount <- runSqlPool
    (count [ ME.CourseRegistrationCourseSlug ==. slugVal
           , ME.CourseRegistrationStatus !=. "cancelled"
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
      remainingSeatsRaw = capacityVal - totalCount
      remainingSeats = max 1 remainingSeatsRaw
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
            priceUsd
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

-- | Instagram sync: schedule one fetch per handle per day at a random time.
startInstagramSyncJob :: Env -> IO ()
startInstagramSyncJob env = do
  void (forkIO (instagramCronLoop env))
  LogBuf.addLog LogBuf.LogInfo "[Cron][IGSync] Scheduled daily Instagram sync per handle (randomized)."

instagramCronLoop :: Env -> IO ()
instagramCronLoop env = forever $ do
  accounts <- ensureInstagramAccounts env
  for_ accounts (scheduleSync env)
  threadDelay (24 * 60 * 60 * 1000000) -- wait a day before scheduling again

ensureInstagramAccounts :: Env -> IO [Entity SocialSyncAccount]
ensureInstagramAccounts Env{..} = runSqlPool action envPool
  where
    action = do
      now <- liftIO getCurrentTime
      parties <- selectList [PartyInstagram !=. Nothing] []
      catMaybes <$> mapM (ensureAccount now) parties

    ensureAccount now (Entity pid party) =
      case nonEmptyText (partyInstagram party) of
        Nothing -> pure Nothing
        Just rawHandle -> do
          let handleTxt = normalizeHandle rawHandle
          existing <- getBy (UniqueSocialSyncAccount "instagram" handleTxt)
          case existing of
            Just ent -> pure (Just ent)
            Nothing -> do
              ent <- insertEntity SocialSyncAccount
                { socialSyncAccountPartyId = Just pid
                , socialSyncAccountArtistProfileId = Nothing
                , socialSyncAccountPlatform = "instagram"
                , socialSyncAccountExternalUserId = handleTxt
                , socialSyncAccountHandle = Just handleTxt
                , socialSyncAccountAccessToken = instagramAppToken envConfig
                , socialSyncAccountTokenExpiresAt = Nothing
                , socialSyncAccountStatus = "pending"
                , socialSyncAccountLastSyncedAt = Nothing
                , socialSyncAccountCreatedAt = now
                , socialSyncAccountUpdatedAt = Nothing
                }
              pure (Just ent)

scheduleSync :: Env -> Entity SocialSyncAccount -> IO ()
scheduleSync env ent@(Entity _ acc) = do
  delaySeconds <- randomRIO (0, 24 * 60 * 60 - 1)
  let labelTxt = displayHandle acc
  LogBuf.addLog LogBuf.LogInfo ("[Cron][IGSync] " <> labelTxt <> " scheduled in " <> T.pack (show delaySeconds) <> "s")
  void $ forkIO $ do
    threadDelay (delaySeconds * 1000000)
    syncInstagramAccount env ent

syncInstagramAccount :: Env -> Entity SocialSyncAccount -> IO ()
syncInstagramAccount Env{..} (Entity accId acc) = do
  now <- getCurrentTime
  let labelTxt = displayHandle acc
      token = socialSyncAccountAccessToken acc <|> instagramAppToken envConfig
  case token of
    Nothing -> LogBuf.addLog LogBuf.LogWarning ("[Cron][IGSync] Skipping " <> labelTxt <> " (no access token configured).")
    Just tok -> do
      mediaResult <- fetchUserMedia envConfig tok (socialSyncAccountExternalUserId acc)
      case mediaResult of
        Left errTxt -> do
          LogBuf.addLog LogBuf.LogError ("[Cron][IGSync] Failed for " <> labelTxt <> ": " <> errTxt)
          void $ runSqlPool
            (insert SocialSyncRun
              { socialSyncRunPlatform = "instagram"
              , socialSyncRunIngestSource = "cron"
              , socialSyncRunStartedAt = now
              , socialSyncRunEndedAt = Just now
              , socialSyncRunStatus = "error"
              , socialSyncRunNewPosts = 0
              , socialSyncRunUpdatedPosts = 0
              , socialSyncRunErrorMessage = Just errTxt
              }) envPool
        Right mediaList -> do
          let recentMedia = filter (isNewSince acc) mediaList
          (inserted, updated) <- runSqlPool (upsertMedia now accId acc recentMedia) envPool
          void $ runSqlPool
            (update accId
              [ SocialSyncAccountLastSyncedAt =. Just now
              , SocialSyncAccountUpdatedAt =. Just now
              , SocialSyncAccountStatus =. "connected"
              ]) envPool
          void $ runSqlPool
            (insert SocialSyncRun
              { socialSyncRunPlatform = "instagram"
              , socialSyncRunIngestSource = "cron"
              , socialSyncRunStartedAt = now
              , socialSyncRunEndedAt = Just now
              , socialSyncRunStatus = "ok"
              , socialSyncRunNewPosts = inserted
              , socialSyncRunUpdatedPosts = updated
              , socialSyncRunErrorMessage = Nothing
              }) envPool
          LogBuf.addLog LogBuf.LogInfo ("[Cron][IGSync] Synced " <> labelTxt <> " (+" <> T.pack (show inserted) <> ", updated " <> T.pack (show updated) <> ").")

upsertMedia
  :: UTCTime
  -> SocialSyncAccountId
  -> SocialSyncAccount
  -> [InstagramMedia]
  -> SqlPersistT IO (Int, Int)
upsertMedia now accId acc medias = foldM go (0, 0) medias
  where
    go :: (Int, Int) -> InstagramMedia -> SqlPersistT IO (Int, Int)
    go (ins, upd) media = do
      mExisting <- getBy (UniqueSocialSyncPost "instagram" (imId media))
      let mediaTxt = nonEmptyText (imMediaUrl media)
          tagsTxt = nonEmptyText (Just (T.intercalate "," (classifyTags (imCaption media))))
          summaryTxt = buildSummary (imCaption media)
          baseFields = catMaybes
            [ (\v -> SocialSyncPostCaption =. Just v) <$> imCaption media
            , (\v -> SocialSyncPostPermalink =. Just v) <$> imPermalink media
            , (\v -> SocialSyncPostMediaUrls =. Just v) <$> mediaTxt
            , (\v -> SocialSyncPostPostedAt =. Just v) <$> imTimestamp media
            , Just (SocialSyncPostFetchedAt =. now)
            , Just (SocialSyncPostUpdatedAt =. now)
            , Just (SocialSyncPostIngestSource =. "cron")
            , (\v -> SocialSyncPostTags =. Just v) <$> tagsTxt
            , (\v -> SocialSyncPostSummary =. Just v) <$> summaryTxt
            ]
          artistFields = catMaybes
            [ (\v -> SocialSyncPostArtistPartyId =. Just v) <$> socialSyncAccountPartyId acc
            , (\v -> SocialSyncPostArtistProfileId =. Just v) <$> socialSyncAccountArtistProfileId acc
            ]
      case mExisting of
        Just (Entity key _) -> do
          update key (baseFields <> artistFields)
          pure (ins, upd + 1)
        Nothing -> do
          insert_ SocialSyncPost
            { socialSyncPostAccountId = Just accId
            , socialSyncPostPlatform = "instagram"
            , socialSyncPostExternalPostId = imId media
            , socialSyncPostArtistPartyId = socialSyncAccountPartyId acc
            , socialSyncPostArtistProfileId = socialSyncAccountArtistProfileId acc
            , socialSyncPostCaption = imCaption media
            , socialSyncPostPermalink = imPermalink media
            , socialSyncPostMediaUrls = mediaTxt
            , socialSyncPostPostedAt = imTimestamp media
            , socialSyncPostFetchedAt = now
            , socialSyncPostTags = tagsTxt
            , socialSyncPostSummary = summaryTxt
            , socialSyncPostIngestSource = "cron"
            , socialSyncPostLikeCount = Nothing
            , socialSyncPostCommentCount = Nothing
            , socialSyncPostShareCount = Nothing
            , socialSyncPostViewCount = Nothing
            , socialSyncPostCreatedAt = now
            , socialSyncPostUpdatedAt = now
            }
          pure (ins + 1, upd)

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText Nothing = Nothing
nonEmptyText (Just txt) =
  let trimmed = T.strip txt
  in if T.null trimmed then Nothing else Just trimmed

classifyTags :: Maybe Text -> [Text]
classifyTags mCaption =
  let base = T.toLower (fromMaybe "" mCaption)
      matches xs = any (`T.isInfixOf` base) xs
      tags = catMaybes
        [ if matches ["show", "gig", "live", "tour", "set time", "festival"] then Just "show" else Nothing
        , if matches ["release", "single", "album", "ep", "out now", "streaming"] then Just "release" else Nothing
        , if matches ["merch", "shirt", "hoodie", "drop", "store", "shop"] then Just "merch" else Nothing
        , if matches ["press", "interview", "review", "feature", "coverage"] then Just "press" else Nothing
        ]
  in if null tags then ["general"] else tags

buildSummary :: Maybe Text -> Maybe Text
buildSummary Nothing = Nothing
buildSummary (Just txt) =
  let trimmed = T.strip txt
  in if T.null trimmed then Nothing else Just (T.take 180 trimmed)

normalizeHandle :: Text -> Text
normalizeHandle = T.dropWhile (== '@') . T.strip

displayHandle :: SocialSyncAccount -> Text
displayHandle acc = "@" <> fromMaybe (socialSyncAccountExternalUserId acc) (socialSyncAccountHandle acc)

isNewSince :: SocialSyncAccount -> InstagramMedia -> Bool
isNewSince acc media =
  case socialSyncAccountLastSyncedAt acc of
    Nothing -> True
    Just lastTs ->
      case imTimestamp media of
        Nothing    -> True
        Just postedTs -> postedTs > lastTs
