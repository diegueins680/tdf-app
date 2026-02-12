{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module TDF.Cron
  ( startCoursePaymentReminderJob
  , startInstagramSyncJob
  , startSocialAutoReplyJob
  ) where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Exception       (SomeException, try)
import           Control.Applicative    ((<|>))
import           Control.Monad           (forever, void, when, foldM)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Foldable           (for_)
import           Data.List               (find, foldl')
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (catMaybes, fromMaybe, isJust)
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
                                         , selectFirst
                                         , getBy
                                         , get
                                         , insert
                                         , insert_
                                         , insertEntity
                                         , update
                                         , (=.)
                                          )
import           Database.Persist.Sql    (SqlPersistT, runSqlPool)
import           System.Random           (randomRIO)
import           System.IO               (hPutStrLn, stderr)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           TDF.DB                  (Env(..), ConnectionPool)
import qualified TDF.Email.Service       as EmailSvc
import qualified TDF.LogBuffer           as LogBuf
import qualified TDF.ModelsExtra         as ME
import           TDF.Models
import           TDF.Services.InstagramMessaging (sendInstagramText)
import           TDF.Services.FacebookMessaging (sendFacebookText)
import           TDF.Services.InstagramSync (InstagramMedia(..), fetchUserMedia)
import           TDF.Routes.Courses      (CourseMetadata(..))
import           TDF.Server              ( buildLandingUrl
                                         , loadAdExamples
                                         , runRagChatWithStatus
                                         , courseMetadataFor
                                         , loadWhatsAppEnv
                                         , WhatsAppEnv(..)
                                         , toCourseMetadata
                                         , normalizeSlug
                                         , productionCourseSlug
                                         , productionCoursePrice
                                         , productionCourseCapacity
                                         )
import           TDF.RagStore            (ensureRagIndex, retrieveRagContext)
import qualified TDF.Trials.Models       as Trials
import           TDF.Config              (AppConfig, instagramAppToken)
import           TDF.WhatsApp.Client     (sendText)

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
nextNineAMUtc = nextLocalTimeUtc (TimeOfDay 9 0 0)

nextTenThirtyAMUtc :: IO UTCTime
nextTenThirtyAMUtc = nextLocalTimeUtc (TimeOfDay 10 30 0)

nextLocalTimeUtc :: TimeOfDay -> IO UTCTime
nextLocalTimeUtc targetTime = do
  now <- getZonedTime
  let ZonedTime (LocalTime day tod) tz = now
      targetDay = if tod < targetTime then day else addDays 1 day
      targetZoned = ZonedTime (LocalTime targetDay targetTime) tz
  pure (zonedTimeToUTC targetZoned)

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

-- | Launch a background thread that sends daily auto replies for social inbox messages.
startSocialAutoReplyJob :: Env -> IO ()
startSocialAutoReplyJob env = do
  void (forkIO (socialReplyLoop env))
  LogBuf.addLog LogBuf.LogInfo "[Cron][SocialAutoReply] Scheduled daily replies at 10:30 local time."

socialReplyLoop :: Env -> IO ()
socialReplyLoop env = forever $ do
  target <- nextTenThirtyAMUtc
  waitUntil target
  result <- try (sendSocialAutoReplies env) :: IO (Either SomeException ())
  case result of
    Left err -> do
      let msg = "[Cron][SocialAutoReply] Job failed: " <> T.pack (show err)
      hPutStrLn stderr (T.unpack msg)
      LogBuf.addLog LogBuf.LogError msg
    Right () ->
      LogBuf.addLog LogBuf.LogInfo "[Cron][SocialAutoReply] Reply job finished."

sendSocialAutoReplies :: Env -> IO ()
sendSocialAutoReplies env@Env{envPool, envConfig} = do
  refreshAttempt <- try (ensureRagIndex envConfig envPool) :: IO (Either SomeException (Either Text Bool))
  case refreshAttempt of
    Left err ->
      LogBuf.addLog LogBuf.LogWarning ("[Cron][SocialAutoReply] RAG refresh crashed: " <> T.pack (show err))
    Right (Left err) ->
      LogBuf.addLog LogBuf.LogWarning ("[Cron][SocialAutoReply] RAG refresh failed: " <> err)
    Right (Right True) ->
      LogBuf.addLog LogBuf.LogInfo "[Cron][SocialAutoReply] RAG index refreshed."
    Right (Right False) ->
      pure ()
  igCount <- processInstagramReplies env
  fbCount <- processFacebookReplies env
  waCount <- processWhatsAppReplies env
  LogBuf.addLog LogBuf.LogInfo
    ("[Cron][SocialAutoReply] Instagram replies: " <> T.pack (show igCount)
      <> " | Facebook replies: " <> T.pack (show fbCount)
      <> " | WhatsApp replies: " <> T.pack (show waCount))

data AdContext = AdContext
  { acAdId :: Maybe ME.AdCreativeId
  , acAdName :: Maybe Text
  , acCampaignName :: Maybe Text
  } deriving (Show)

resolveAdContext
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> SqlPersistT IO AdContext
resolveAdContext mAdExternal mAdName mCampaignName = do
  mAd <- resolveAd mAdExternal mAdName
  mCampaign <- case mAd >>= (ME.adCreativeCampaignId . entityVal) of
    Nothing -> resolveCampaignByName mCampaignName
    Just cid -> fmap (Entity cid) <$> get cid
  let acAdId = fmap entityKey mAd
      acAdName = fmap (ME.adCreativeName . entityVal) mAd <|> mAdName
      acCampaignName = fmap (ME.campaignName . entityVal) mCampaign <|> mCampaignName
  pure AdContext{..}
  where
    resolveAd Nothing Nothing = pure Nothing
    resolveAd (Just extId) _ = selectFirst [ME.AdCreativeExternalId ==. Just extId] []
    resolveAd Nothing (Just nameTxt) = do
      rows <- selectList [] [Desc ME.AdCreativeUpdatedAt, LimitTo 200]
      pure $ find (\(Entity _ a) -> T.toCaseFold (ME.adCreativeName a) == T.toCaseFold nameTxt) rows

    resolveCampaignByName Nothing = pure Nothing
    resolveCampaignByName (Just nameTxt) = do
      rows <- selectList [] [Desc ME.CampaignUpdatedAt, LimitTo 200]
      pure $ find (\(Entity _ c) -> T.toCaseFold (ME.campaignName c) == T.toCaseFold nameTxt) rows

buildChannelNote :: Text -> Maybe Text -> Maybe Text -> Text
buildChannelNote channel mAd mCampaign =
  T.intercalate " · " (catMaybes
    [ Just channel
    , ("ad=" <>) <$> mAd
    , ("campaña=" <>) <$> mCampaign
    ])

processInstagramReplies :: Env -> IO Int
processInstagramReplies Env{envPool, envConfig} = do
  pending <- runSqlPool
    (selectList
      [ InstagramMessageDirection ==. "incoming"
      , InstagramMessageRepliedAt ==. Nothing
      ]
      [Asc InstagramMessageCreatedAt, LimitTo 200])
    envPool
  foldM (replyInstagramOne envConfig envPool) 0 pending

replyInstagramOne :: AppConfig -> ConnectionPool -> Int -> Entity InstagramMessage -> IO Int
replyInstagramOne cfg pool acc (Entity key msg) = do
  now <- getCurrentTime
  let body = fromMaybe "" (instagramMessageText msg)
  if T.null (T.strip body)
    then pure acc
    else do
      adContext <- runSqlPool
        (resolveAdContext (instagramMessageAdExternalId msg) (instagramMessageAdName msg) (instagramMessageCampaignName msg))
        pool
      let channelNote = buildChannelNote "instagram" (acAdName adContext) (acCampaignName adContext)
      examples <- runSqlPool
        (loadAdExamples (isJust (acAdId adContext)) (maybe [] pure (acAdId adContext)))
        pool
      kb <- retrieveRagContext cfg pool body
      replyRes <- runRagChatWithStatus cfg kb examples body (Just channelNote)
      case replyRes of
        Left err -> do
          runSqlPool
            (update key
              [ InstagramMessageReplyError =. Just err
              , InstagramMessageAdName =. acAdName adContext
              , InstagramMessageCampaignName =. acCampaignName adContext
              ])
            pool
          pure acc
        Right replyRaw -> do
          let reply = T.strip replyRaw
          if T.null reply
            then do
              runSqlPool
                (update key
                  [ InstagramMessageReplyError =. Just "OpenAI response empty"
                  , InstagramMessageAdName =. acAdName adContext
                  , InstagramMessageCampaignName =. acCampaignName adContext
                  ])
                pool
              pure acc
            else do
              sendResult <- sendInstagramText cfg (instagramMessageSenderId msg) reply
              case sendResult of
                Left err -> do
                  runSqlPool
                    (update key
                      [ InstagramMessageReplyError =. Just err
                      , InstagramMessageAdName =. acAdName adContext
                      , InstagramMessageCampaignName =. acCampaignName adContext
                      ])
                    pool
                  pure acc
                Right _ -> do
                  runSqlPool
                    (do
                      update key
                        [ InstagramMessageRepliedAt =. Just now
                        , InstagramMessageReplyText =. Just reply
                        , InstagramMessageReplyError =. Nothing
                        , InstagramMessageAdName =. acAdName adContext
                        , InstagramMessageCampaignName =. acCampaignName adContext
                        ]
                      insert_ (InstagramMessage (instagramMessageExternalId msg <> "-out-" <> T.pack (show now))
                                (instagramMessageSenderId msg)
                                (instagramMessageSenderName msg)
                                (Just reply)
                                "outgoing"
                                (instagramMessageAdExternalId msg)
                                (acAdName adContext)
                                (instagramMessageCampaignExternalId msg)
                                (acCampaignName adContext)
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                                now)
                    )
                    pool
                  pure (acc + 1)

processFacebookReplies :: Env -> IO Int
processFacebookReplies Env{envPool, envConfig} = do
  pending <- runSqlPool
    (selectList
      [ ME.FacebookMessageDirection ==. "incoming"
      , ME.FacebookMessageRepliedAt ==. Nothing
      ]
      [Asc ME.FacebookMessageCreatedAt, LimitTo 200])
    envPool
  foldM (replyFacebookOne envConfig envPool) 0 pending

replyFacebookOne :: AppConfig -> ConnectionPool -> Int -> Entity ME.FacebookMessage -> IO Int
replyFacebookOne cfg pool acc (Entity key msg) = do
  now <- getCurrentTime
  let body = fromMaybe "" (ME.facebookMessageText msg)
  if T.null (T.strip body)
    then pure acc
    else do
      adContext <- runSqlPool
        (resolveAdContext (ME.facebookMessageAdExternalId msg) (ME.facebookMessageAdName msg) (ME.facebookMessageCampaignName msg))
        pool
      let channelNote = buildChannelNote "facebook" (acAdName adContext) (acCampaignName adContext)
      examples <- runSqlPool
        (loadAdExamples (isJust (acAdId adContext)) (maybe [] pure (acAdId adContext)))
        pool
      kb <- retrieveRagContext cfg pool body
      replyRes <- runRagChatWithStatus cfg kb examples body (Just channelNote)
      case replyRes of
        Left err -> do
          runSqlPool
            (update key
              [ ME.FacebookMessageReplyError =. Just err
              , ME.FacebookMessageAdName =. acAdName adContext
              , ME.FacebookMessageCampaignName =. acCampaignName adContext
              ])
            pool
          pure acc
        Right replyRaw -> do
          let reply = T.strip replyRaw
          if T.null reply
            then do
              runSqlPool
                (update key
                  [ ME.FacebookMessageReplyError =. Just "OpenAI response empty"
                  , ME.FacebookMessageAdName =. acAdName adContext
                  , ME.FacebookMessageCampaignName =. acCampaignName adContext
                  ])
                pool
              pure acc
            else do
              sendResult <- sendFacebookText cfg (ME.facebookMessageSenderId msg) reply
              case sendResult of
                Left err -> do
                  runSqlPool
                    (update key
                      [ ME.FacebookMessageReplyError =. Just err
                      , ME.FacebookMessageAdName =. acAdName adContext
                      , ME.FacebookMessageCampaignName =. acCampaignName adContext
                      ])
                    pool
                  pure acc
                Right _ -> do
                  runSqlPool
                    (do
                      update key
                        [ ME.FacebookMessageRepliedAt =. Just now
                        , ME.FacebookMessageReplyText =. Just reply
                        , ME.FacebookMessageReplyError =. Nothing
                        , ME.FacebookMessageAdName =. acAdName adContext
                        , ME.FacebookMessageCampaignName =. acCampaignName adContext
                        ]
                      insert_ (ME.FacebookMessage (ME.facebookMessageExternalId msg <> "-out-" <> T.pack (show now))
                                (ME.facebookMessageSenderId msg)
                                (ME.facebookMessageSenderName msg)
                                (Just reply)
                                "outgoing"
                                (ME.facebookMessageAdExternalId msg)
                                (acAdName adContext)
                                (ME.facebookMessageCampaignExternalId msg)
                                (acCampaignName adContext)
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                                now)
                    )
                    pool
                  pure (acc + 1)

processWhatsAppReplies :: Env -> IO Int
processWhatsAppReplies Env{envPool, envConfig} = do
  waEnv <- loadWhatsAppEnv
  pending <- runSqlPool
    (selectList
      [ ME.WhatsAppMessageDirection ==. "incoming"
      , ME.WhatsAppMessageRepliedAt ==. Nothing
      ]
      [Asc ME.WhatsAppMessageCreatedAt, LimitTo 200])
    envPool
  foldM (replyWhatsAppOne envConfig envPool waEnv) 0 pending

replyWhatsAppOne :: AppConfig -> ConnectionPool -> WhatsAppEnv -> Int -> Entity ME.WhatsAppMessage -> IO Int
replyWhatsAppOne cfg pool waEnv acc (Entity key msg) = do
  now <- getCurrentTime
  let body = fromMaybe "" (ME.whatsAppMessageText msg)
  if T.null (T.strip body)
    then pure acc
    else do
      adContext <- runSqlPool
        (resolveAdContext (ME.whatsAppMessageAdExternalId msg) (ME.whatsAppMessageAdName msg) (ME.whatsAppMessageCampaignName msg))
        pool
      let channelNote = buildChannelNote "whatsapp" (acAdName adContext) (acCampaignName adContext)
      examples <- runSqlPool
        (loadAdExamples (isJust (acAdId adContext)) (maybe [] pure (acAdId adContext)))
        pool
      kb <- retrieveRagContext cfg pool body
      replyRes <- runRagChatWithStatus cfg kb examples body (Just channelNote)
      case replyRes of
        Left err -> do
          runSqlPool
            (update key
              [ ME.WhatsAppMessageReplyError =. Just err
              , ME.WhatsAppMessageAdName =. acAdName adContext
              , ME.WhatsAppMessageCampaignName =. acCampaignName adContext
              ])
            pool
          pure acc
        Right replyRaw -> do
          let reply = T.strip replyRaw
          if T.null reply
            then do
              runSqlPool
                (update key
                  [ ME.WhatsAppMessageReplyError =. Just "OpenAI response empty"
                  , ME.WhatsAppMessageAdName =. acAdName adContext
                  , ME.WhatsAppMessageCampaignName =. acCampaignName adContext
                  ])
                pool
              pure acc
            else do
              sendResult <- sendWhatsAppAutoReply waEnv (ME.whatsAppMessageSenderId msg) reply
              case sendResult of
                Left err -> do
                  runSqlPool
                    (update key
                      [ ME.WhatsAppMessageReplyError =. Just err
                      , ME.WhatsAppMessageAdName =. acAdName adContext
                      , ME.WhatsAppMessageCampaignName =. acCampaignName adContext
                      ])
                    pool
                  pure acc
                Right _ -> do
                  runSqlPool
                    (do
                      update key
                        [ ME.WhatsAppMessageRepliedAt =. Just now
                        , ME.WhatsAppMessageReplyText =. Just reply
                        , ME.WhatsAppMessageReplyError =. Nothing
                        , ME.WhatsAppMessageAdName =. acAdName adContext
                        , ME.WhatsAppMessageCampaignName =. acCampaignName adContext
                        ]
                      insert_ (ME.WhatsAppMessage (ME.whatsAppMessageExternalId msg <> "-out-" <> T.pack (show now))
                                (ME.whatsAppMessageSenderId msg)
                                (ME.whatsAppMessageSenderName msg)
                                (Just reply)
                                "outgoing"
                                (ME.whatsAppMessageAdExternalId msg)
                                (acAdName adContext)
                                (ME.whatsAppMessageCampaignExternalId msg)
                                (acCampaignName adContext)
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                                now)
                    )
                    pool
                  pure (acc + 1)

sendWhatsAppAutoReply :: WhatsAppEnv -> Text -> Text -> IO (Either Text ())
sendWhatsAppAutoReply WhatsAppEnv{waToken = Just tok, waPhoneId = Just pid, waApiVersion = mVersion} phone reply = do
  manager <- newManager tlsManagerSettings
  let version = fromMaybe "v20.0" mVersion
  res <- sendText manager version tok pid phone reply
  pure $ case res of
    Left err -> Left (T.pack err)
    Right _ -> Right ()
sendWhatsAppAutoReply _ _ _ = pure (Left "WhatsApp config not available")

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
