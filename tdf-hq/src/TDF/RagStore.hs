{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.RagStore
  ( refreshRagIndex
  , ensureRagIndex
  , retrieveRagContext
  , getRagIndexStats
  , availabilityOverlaps
  , validateEmbeddingModelDimensions
  ) where

import           Control.Monad          (forM, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Exception.Safe (catchAny, throwM)
import           Data.Aeson             (Value, ToJSON(..), FromJSON(..), object, (.=), encode, eitherDecode, withObject, (.:))
import           Data.ByteString.Lazy   (toStrict)
import           Data.Int               (Int64)
import           Data.List              (sortOn)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Time              (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           Database.Persist       (Entity(..), SelectOpt(..), selectList, entityKey, entityVal, (==.), (!=.), (<-.), (>=.), (<=.))
import           Database.Persist.Sql   (PersistValue(..), Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool, transactionSave, transactionUndo)
import           GHC.Generics           (Generic)
import           Network.HTTP.Client    (Request(..), httpLbs, newManager, parseRequest, responseBody, responseStatus, RequestBody(..))
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import           Numeric                (showFFloat)
import           Text.Read              (readMaybe)
import           Web.PathPieces         (toPathPiece)

import           TDF.Config             (AppConfig(..), openAiEmbedDimensions)
import           TDF.DB                 (ConnectionPool)
import qualified TDF.Models             as M
import qualified TDF.ModelsExtra        as ME
import qualified TDF.Trials.Models      as Trials

data RagDocument = RagDocument
  { rdSource   :: Text
  , rdSourceId :: Maybe Text
  , rdContent  :: Text
  , rdMetadata :: Value
  } deriving (Show)

data RagChunk = RagChunk
  { rcSource     :: Text
  , rcSourceId   :: Maybe Text
  , rcChunkIndex :: Int
  , rcContent    :: Text
  , rcMetadata   :: Value
  } deriving (Show)

data RagIndexStats = RagIndexStats
  { risCount     :: Int
  , risUpdatedAt :: Maybe UTCTime
  } deriving (Show)

data EmbeddingReq = EmbeddingReq
  { model :: Text
  , input :: [Text]
  } deriving (Show, Generic)

instance ToJSON EmbeddingReq

data EmbeddingData = EmbeddingData
  { embedding :: [Double]
  , index     :: Int
  } deriving (Show, Generic)

instance FromJSON EmbeddingData

data EmbeddingResp = EmbeddingResp
  { embeddings :: [EmbeddingData]
  } deriving (Show)

instance FromJSON EmbeddingResp where
  parseJSON = withObject "EmbeddingResp" $ \o ->
    EmbeddingResp <$> o .: "data"

data OpenAIErrorBody = OpenAIErrorBody
  { oeMessage :: Text
  } deriving (Show, Generic)

instance FromJSON OpenAIErrorBody where
  parseJSON = withObject "OpenAIErrorBody" $ \o ->
    OpenAIErrorBody <$> o .: "message"

data OpenAIErrorResp = OpenAIErrorResp
  { oeError :: OpenAIErrorBody
  } deriving (Show, Generic)

instance FromJSON OpenAIErrorResp where
  parseJSON = withObject "OpenAIErrorResp" $ \o ->
    OpenAIErrorResp <$> o .: "error"

refreshRagIndex :: AppConfig -> ConnectionPool -> IO (Either Text Int)
refreshRagIndex cfg pool = do
  dimCheck <- validateRagEmbeddingDim cfg pool
  case dimCheck of
    Left err -> pure (Left err)
    Right () -> do
      docs <- runSqlPool (buildRagDocuments cfg) pool
      let chunks = concatMap (docToChunks cfg) docs
      if null chunks
        then pure (Right 0)
        else do
          embedResult <- embedTexts cfg (map rcContent chunks)
          case embedResult of
            Left err -> pure (Left err)
            Right embeddings -> do
              if length embeddings /= length chunks
                then pure (Left "Embedding response size mismatch")
                else do
                  runSqlPool (replaceRagChunks chunks embeddings) pool
                  pure (Right (length chunks))

ensureRagIndex :: AppConfig -> ConnectionPool -> IO (Either Text Bool)
ensureRagIndex cfg pool = do
  stats <- runSqlPool loadRagIndexStats pool
  now <- getCurrentTime
  let stale = case risUpdatedAt stats of
        Nothing -> True
        Just ts ->
          let elapsed = realToFrac (diffSeconds now ts) / 3600 :: Double
          in elapsed >= fromIntegral (ragRefreshHours cfg)
  if risCount stats == 0 || stale
    then do
      refreshed <- refreshRagIndex cfg pool
      case refreshed of
        Left err -> pure (Left err)
        Right _  -> pure (Right True)
    else pure (Right False)
  where
    diffSeconds :: UTCTime -> UTCTime -> Double
    diffSeconds a b = realToFrac (diffUTCTime a b)

retrieveRagContext :: AppConfig -> ConnectionPool -> Text -> IO [Text]
retrieveRagContext cfg pool query = do
  dimCheck <- validateRagEmbeddingDim cfg pool
  case dimCheck of
    Left _ -> pure []
    Right () -> do
      embedResult <- embedTexts cfg [query]
      case embedResult of
        Left _ -> pure []
        Right [] -> pure []
        Right (embedding:_) ->
          runSqlPool (selectRagChunks cfg embedding) pool

getRagIndexStats :: ConnectionPool -> IO (Int, Maybe UTCTime)
getRagIndexStats pool = do
  RagIndexStats{..} <- runSqlPool loadRagIndexStats pool
  pure (risCount, risUpdatedAt)

buildRagDocuments :: AppConfig -> SqlPersistT IO [RagDocument]
buildRagDocuments cfg = do
  now <- liftIO getCurrentTime
  courses <- selectList [] [Desc Trials.CourseUpdatedAt, LimitTo 50]
  services <- selectList [M.ServiceCatalogActive ==. True] [Asc M.ServiceCatalogName]
  campaigns <- selectList [] [Desc ME.CampaignUpdatedAt, LimitTo 200]
  ads <- selectList [] [Desc ME.AdCreativeUpdatedAt, LimitTo 200]
  rooms <- selectList [] [Asc ME.RoomName]
  brainEntries <- selectList
    [ME.StudioBrainEntryActive ==. True]
    [Desc ME.StudioBrainEntryUpdatedAt, LimitTo 200]
  resources <- selectList [M.ResourceActive ==. True] [Asc M.ResourceName]
  let courseDocs = map courseToDoc courses
      serviceDocs = map serviceToDoc services
      campaignDocs = map campaignToDoc campaigns
      adDocs = map adToDoc ads
      roomDocs = map roomToDoc rooms
      brainDocs = map brainEntryToDoc brainEntries
      resourceDocs = map resourceToDoc resources
  availabilityDocs <- buildAvailabilityDocs cfg now resources
  pure (courseDocs ++ serviceDocs ++ campaignDocs ++ adDocs ++ roomDocs ++ brainDocs ++ resourceDocs ++ availabilityDocs)

courseToDoc :: Entity Trials.Course -> RagDocument
courseToDoc (Entity _ c) =
  RagDocument
    { rdSource = "course"
    , rdSourceId = Just (Trials.courseSlug c)
    , rdContent = T.intercalate " · " (catMaybes
        [ Just ("Curso: " <> Trials.courseTitle c)
        , ("Subtítulo: " <>) <$> nonEmpty (Trials.courseSubtitle c)
        , ("Formato: " <>) <$> nonEmpty (Trials.courseFormat c)
        , ("Duración: " <>) <$> nonEmpty (Trials.courseDuration c)
        , Just ("Precio: " <> formatMoney (Trials.coursePriceCents c) (Trials.courseCurrency c))
        , Just ("Cupos: " <> T.pack (show (Trials.courseCapacity c)))
        , ("Inicio: " <>) <$> (formatStartHour <$> Trials.courseSessionStartHour c)
        , ("Duración sesión: " <>) <$> (formatHours <$> Trials.courseSessionDurationHours c)
        , ("Ubicación: " <>) <$> nonEmpty (Trials.courseLocationLabel c)
        , ("Mapa: " <>) <$> nonEmpty (Trials.courseLocationMapUrl c)
        , ("WhatsApp: " <>) <$> nonEmpty (Trials.courseWhatsappCtaUrl c)
        , ("Landing: " <>) <$> nonEmpty (Trials.courseLandingUrl c)
        , ("DAWs: " <>) <$> nonEmptyList (Trials.courseDaws c)
        , ("Incluye: " <>) <$> nonEmptyList (Trials.courseIncludes c)
        , ("Instructor: " <>) <$> nonEmpty (Trials.courseInstructorName c)
        , ("Bio instructor: " <>) <$> nonEmpty (Trials.courseInstructorBio c)
        , ("Avatar instructor: " <>) <$> nonEmpty (Trials.courseInstructorAvatarUrl c)
        ])
    , rdMetadata = object ["slug" .= Trials.courseSlug c]
    }
  where
    formatStartHour hour = T.pack (show hour) <> ":00"
    formatHours hours = T.pack (show hours) <> "h"

serviceToDoc :: Entity M.ServiceCatalog -> RagDocument
serviceToDoc (Entity sid s) =
  RagDocument
    { rdSource = "service"
    , rdSourceId = Just (T.pack (show (fromSqlKey sid)))
    , rdContent = T.intercalate " · " (catMaybes
        [ Just ("Servicio: " <> M.serviceCatalogName s)
        , Just ("Tipo: " <> T.pack (show (M.serviceCatalogKind s)))
        , Just ("Modelo: " <> T.pack (show (M.serviceCatalogPricingModel s)))
        , ("Tarifa base: " <>) <$> (formatCents <$> M.serviceCatalogDefaultRateCents s <*> pure (M.serviceCatalogCurrency s))
        , ("Unidad: " <>) <$> nonEmpty (M.serviceCatalogBillingUnit s)
        , ("Impuestos: " <>) <$> fmap formatTax (M.serviceCatalogTaxBps s)
        , Just ("Activo: " <> boolLabel (M.serviceCatalogActive s))
        ])
    , rdMetadata = object ["serviceId" .= fromSqlKey sid]
    }
  where
    formatCents cents currency = formatMoney cents currency
    formatTax bps = T.pack (show bps) <> " bps"

campaignToDoc :: Entity ME.Campaign -> RagDocument
campaignToDoc (Entity cid c) =
  RagDocument
    { rdSource = "campaign"
    , rdSourceId = Just (T.pack (show (fromSqlKey cid)))
    , rdContent = T.intercalate " · " (catMaybes
        [ Just ("Campaña: " <> ME.campaignName c)
        , ("Objetivo: " <>) <$> nonEmpty (ME.campaignObjective c)
        , ("Plataforma: " <>) <$> nonEmpty (ME.campaignPlatform c)
        , Just ("Estado: " <> ME.campaignStatus c)
        , ("Presupuesto: " <>) <$> fmap (\cents -> formatMoney cents "USD") (ME.campaignBudgetCents c)
        , ("Inicio: " <>) <$> (formatDay <$> ME.campaignStartDate c)
        , ("Fin: " <>) <$> (formatDay <$> ME.campaignEndDate c)
        ])
    , rdMetadata = object ["campaignId" .= fromSqlKey cid]
    }
  where
    formatDay day = T.pack (formatTime defaultTimeLocale "%Y-%m-%d" day)

adToDoc :: Entity ME.AdCreative -> RagDocument
adToDoc (Entity aid a) =
  RagDocument
    { rdSource = "ad"
    , rdSourceId = Just (T.pack (show (fromSqlKey aid)))
    , rdContent = T.intercalate " · " (catMaybes
        [ Just ("Anuncio: " <> ME.adCreativeName a)
        , ("Canal: " <>) <$> nonEmpty (ME.adCreativeChannel a)
        , ("Audiencia: " <>) <$> nonEmpty (ME.adCreativeAudience a)
        , ("Landing: " <>) <$> nonEmpty (ME.adCreativeLandingUrl a)
        , ("CTA: " <>) <$> nonEmpty (ME.adCreativeCta a)
        , ("Estado: " <>) <$> nonEmpty (Just (ME.adCreativeStatus a))
        , ("Notas: " <>) <$> nonEmpty (ME.adCreativeNotes a)
        ])
    , rdMetadata = object
        [ "adId" .= fromSqlKey aid
        , "campaignId" .= fmap fromSqlKey (ME.adCreativeCampaignId a)
        ]
    }

roomToDoc :: Entity ME.Room -> RagDocument
roomToDoc (Entity rid r) =
  RagDocument
    { rdSource = "room"
    , rdSourceId = Just (toPathPiece rid)
    , rdContent = T.intercalate " · " (catMaybes
        [ Just ("Sala: " <> ME.roomName r)
        , Just ("Reservable: " <> boolLabel (ME.roomIsBookable r))
        , ("Capacidad: " <>) <$> fmap (T.pack . show) (ME.roomCapacity r)
        , ("Canales: " <>) <$> fmap (T.pack . show) (ME.roomChannelCount r)
        , ("Sample rate: " <>) <$> fmap (T.pack . show) (ME.roomDefaultSampleRate r)
        , ("Patchbay: " <>) <$> nonEmpty (ME.roomPatchbayNotes r)
        ])
    , rdMetadata = object ["roomId" .= toPathPiece rid]
    }

brainEntryToDoc :: Entity ME.StudioBrainEntry -> RagDocument
brainEntryToDoc (Entity bid entry) =
  RagDocument
    { rdSource = "studio_brain"
    , rdSourceId = Just (T.pack (show (fromSqlKey bid)))
    , rdContent = T.intercalate " · " (catMaybes
        [ Just ("Tema: " <> ME.studioBrainEntryTitle entry)
        , ("Categoría: " <>) <$> nonEmpty (ME.studioBrainEntryCategory entry)
        , ("Tags: " <>) <$> nonEmptyList (ME.studioBrainEntryTags entry)
        , Just ("Detalle: " <> ME.studioBrainEntryBody entry)
        ])
    , rdMetadata = object
        [ "brainEntryId" .= fromSqlKey bid
        , "category" .= ME.studioBrainEntryCategory entry
        , "tags" .= fromMaybe [] (ME.studioBrainEntryTags entry)
        ]
    }

resourceToDoc :: Entity M.Resource -> RagDocument
resourceToDoc (Entity rid r) =
  RagDocument
    { rdSource = "resource"
    , rdSourceId = Just (T.pack (show (fromSqlKey rid)))
    , rdContent = T.intercalate " · " (catMaybes
        [ Just ("Recurso: " <> M.resourceName r)
        , Just ("Tipo: " <> T.pack (show (M.resourceResourceType r)))
        , ("Capacidad: " <>) <$> fmap (T.pack . show) (M.resourceCapacity r)
        , Just ("Activo: " <> boolLabel (M.resourceActive r))
        , Just ("Slug: " <> M.resourceSlug r)
        ])
    , rdMetadata = object ["resourceId" .= fromSqlKey rid]
    }

buildAvailabilityDocs :: AppConfig -> UTCTime -> [Entity M.Resource] -> SqlPersistT IO [RagDocument]
buildAvailabilityDocs cfg now resources = do
  let windowSeconds = fromIntegral (ragAvailabilityDays cfg) * 86400
      windowEnd = addUTCTime windowSeconds now
      resourceMap = Map.fromList [(entityKey r, entityVal r) | r <- resources]
      resourceName rid = M.resourceName <$> Map.lookup rid resourceMap
      resourceType rid = M.resourceResourceType <$> Map.lookup rid resourceMap
  bookings <- selectList
    [ M.BookingStartsAt <=. windowEnd
    , M.BookingEndsAt >=. now
    , M.BookingStatus !=. M.Cancelled
    ]
    [Asc M.BookingStartsAt]
  let bookingIds = map entityKey bookings
      bookingMap = Map.fromList [(entityKey b, entityVal b) | b <- bookings]
  bookingResources <- if null bookingIds
    then pure []
    else selectList [M.BookingResourceBookingId <-. bookingIds] []

  classSessions <- selectList
    [ Trials.ClassSessionStartAt <=. windowEnd
    , Trials.ClassSessionEndAt >=. now
    ]
    [Asc Trials.ClassSessionStartAt]
  trialAssignments <- selectList
    [ Trials.TrialAssignmentStartAt <=. windowEnd
    , Trials.TrialAssignmentEndAt >=. now
    ]
    [Asc Trials.TrialAssignmentStartAt]
  teacherAvailability <- selectList
    [ Trials.TeacherAvailabilityStartAt <=. windowEnd
    , Trials.TeacherAvailabilityEndAt >=. now
    ]
    [Asc Trials.TeacherAvailabilityStartAt]

  let subjectIds =
        foldr (\sid acc -> Map.insert sid () acc) Map.empty
          [ Trials.classSessionSubjectId (entityVal cs)
          | cs <- classSessions
          ] <> foldr (\sid acc -> Map.insert sid () acc) Map.empty
          [ Trials.teacherAvailabilitySubjectId (entityVal ta)
          | ta <- teacherAvailability
          ]
      teacherIds =
        foldr (\tid acc -> Map.insert tid () acc) Map.empty
          [ Trials.classSessionTeacherId (entityVal cs)
          | cs <- classSessions
          ] <> foldr (\tid acc -> Map.insert tid () acc) Map.empty
          [ Trials.teacherAvailabilityTeacherId (entityVal ta)
          | ta <- teacherAvailability
          ]
  subjects <- if Map.null subjectIds
    then pure []
    else selectList [Trials.SubjectId <-. Map.keys subjectIds] []
  teachers <- if Map.null teacherIds
    then pure []
    else selectList [M.PartyId <-. Map.keys teacherIds] []
  let subjectMap = Map.fromList [(entityKey s, entityVal s) | s <- subjects]
      teacherMap = Map.fromList [(entityKey t, entityVal t) | t <- teachers]
      bookingEvents =
        [ mkAvailabilityEvent
            rid
            (M.bookingStartsAt booking)
            (M.bookingEndsAt booking)
            "booking"
            (formatBookingDoc resName resType booking)
        | Entity _ br <- bookingResources
        , let rid = M.bookingResourceResourceId br
        , Just booking <- [Map.lookup (M.bookingResourceBookingId br) bookingMap]
        , let resName = resourceName rid
        , let resType = resourceType rid
        ]
      classEvents =
        [ mkAvailabilityEvent
            rid
            (Trials.classSessionStartAt sess)
            (Trials.classSessionEndAt sess)
            "class_session"
            (formatClassSessionDoc resName resType sess subjectMap teacherMap)
        | Entity _ sess <- classSessions
        , let rid = Trials.classSessionRoomId sess
        , let resName = resourceName rid
        , let resType = resourceType rid
        ]
      trialEvents =
        [ mkAvailabilityEvent
            rid
            (Trials.trialAssignmentStartAt ta)
            (Trials.trialAssignmentEndAt ta)
            "trial_assignment"
            (formatTrialAssignmentDoc resName resType ta)
        | Entity _ ta <- trialAssignments
        , let rid = Trials.trialAssignmentRoomId ta
        , let resName = resourceName rid
        , let resType = resourceType rid
        ]
      availabilityEvents =
        [ mkAvailabilityEvent
            rid
            (Trials.teacherAvailabilityStartAt ta)
            (Trials.teacherAvailabilityEndAt ta)
            "availability"
            (formatTeacherAvailabilityDoc resName resType ta subjectMap teacherMap)
        | Entity _ ta <- teacherAvailability
        , let rid = Trials.teacherAvailabilityRoomId ta
        , let resName = resourceName rid
        , let resType = resourceType rid
        ]
      allEvents = bookingEvents ++ classEvents ++ trialEvents ++ availabilityEvents
      overlapping = filter (\ev -> availabilityOverlaps now windowEnd (aeStart ev) (aeEnd ev)) allEvents
      grouped = Map.fromListWith (++)
        [ (aeResourceId ev, [ev]) | ev <- overlapping ]
      limited = concatMap (take (ragAvailabilityPerResource cfg) . sortOn aeStart) (Map.elems grouped)
  pure (map aeDoc limited)

formatBookingDoc
  :: Maybe Text
  -> Maybe M.ResourceType
  -> M.Booking
  -> Text
formatBookingDoc resName resType booking =
  T.intercalate " · " (catMaybes
    [ Just ("Recurso ocupado: " <> fromMaybe "Recurso" resName)
    , ("Tipo: " <>) <$> fmap (T.pack . show) resType
    , Just ("Título: " <> M.bookingTitle booking)
    , ("Servicio: " <>) <$> nonEmpty (M.bookingServiceType booking)
    , Just ("Estado: " <> T.pack (show (M.bookingStatus booking)))
    , Just ("Desde: " <> formatUtc (M.bookingStartsAt booking))
    , Just ("Hasta: " <> formatUtc (M.bookingEndsAt booking))
    ])

formatClassSessionDoc
  :: Maybe Text
  -> Maybe M.ResourceType
  -> Trials.ClassSession
  -> Map.Map Trials.SubjectId Trials.Subject
  -> Map.Map M.PartyId M.Party
  -> Text
formatClassSessionDoc resName resType sess subjectMap teacherMap =
  T.intercalate " · " (catMaybes
    [ Just ("Sala ocupada: " <> fromMaybe "Sala" resName)
    , ("Tipo: " <>) <$> fmap (T.pack . show) resType
    , ("Materia: " <>) <$> (Trials.subjectName <$> Map.lookup (Trials.classSessionSubjectId sess) subjectMap)
    , ("Profesor: " <>) <$> (partyName <$> Map.lookup (Trials.classSessionTeacherId sess) teacherMap)
    , Just ("Desde: " <> formatUtc (Trials.classSessionStartAt sess))
    , Just ("Hasta: " <> formatUtc (Trials.classSessionEndAt sess))
    ])

formatTrialAssignmentDoc
  :: Maybe Text
  -> Maybe M.ResourceType
  -> Trials.TrialAssignment
  -> Text
formatTrialAssignmentDoc resName resType ta =
  T.intercalate " · " (catMaybes
    [ Just ("Sala ocupada (trial): " <> fromMaybe "Sala" resName)
    , ("Tipo: " <>) <$> fmap (T.pack . show) resType
    , Just ("Desde: " <> formatUtc (Trials.trialAssignmentStartAt ta))
    , Just ("Hasta: " <> formatUtc (Trials.trialAssignmentEndAt ta))
    ])

formatTeacherAvailabilityDoc
  :: Maybe Text
  -> Maybe M.ResourceType
  -> Trials.TeacherAvailability
  -> Map.Map Trials.SubjectId Trials.Subject
  -> Map.Map M.PartyId M.Party
  -> Text
formatTeacherAvailabilityDoc resName resType ta subjectMap teacherMap =
  T.intercalate " · " (catMaybes
    [ Just ("Sala disponible: " <> fromMaybe "Sala" resName)
    , ("Tipo: " <>) <$> fmap (T.pack . show) resType
    , ("Materia: " <>) <$> (Trials.subjectName <$> Map.lookup (Trials.teacherAvailabilitySubjectId ta) subjectMap)
    , ("Profesor: " <>) <$> (partyName <$> Map.lookup (Trials.teacherAvailabilityTeacherId ta) teacherMap)
    , Just ("Desde: " <> formatUtc (Trials.teacherAvailabilityStartAt ta))
    , Just ("Hasta: " <> formatUtc (Trials.teacherAvailabilityEndAt ta))
    ])

data AvailabilityEvent = AvailabilityEvent
  { aeResourceId :: M.ResourceId
  , aeStart      :: UTCTime
  , aeEnd        :: UTCTime
  , aeDoc        :: RagDocument
  }

availabilityOverlaps :: UTCTime -> UTCTime -> UTCTime -> UTCTime -> Bool
availabilityOverlaps windowStart windowEnd eventStart eventEnd =
  eventStart <= windowEnd && eventEnd >= windowStart

mkAvailabilityEvent :: M.ResourceId -> UTCTime -> UTCTime -> Text -> Text -> AvailabilityEvent
mkAvailabilityEvent rid start end kind content =
  AvailabilityEvent
    { aeResourceId = rid
    , aeStart = start
    , aeEnd = end
    , aeDoc = RagDocument
        { rdSource = "availability"
        , rdSourceId = Just (T.pack (show (fromSqlKey rid)))
        , rdContent = content
        , rdMetadata = object
            [ "kind" .= kind
            , "resourceId" .= fromSqlKey rid
            , "startsAt" .= formatUtc start
            ]
        }
    }

docToChunks :: AppConfig -> RagDocument -> [RagChunk]
docToChunks cfg RagDocument{..} =
  let chunks = chunkWords (ragChunkWords cfg) (ragChunkOverlap cfg) rdContent
  in zipWith
      (\idx content ->
        RagChunk
          { rcSource = rdSource
          , rcSourceId = rdSourceId
          , rcChunkIndex = idx
          , rcContent = content
          , rcMetadata = rdMetadata
          })
      [0..]
      chunks

chunkWords :: Int -> Int -> Text -> [Text]
chunkWords maxWords overlap txt =
  let wordsList = T.words txt
      safeMax = max 1 maxWords
      safeOverlap = max 0 (min overlap (safeMax - 1))
      step = max 1 (safeMax - safeOverlap)
      total = length wordsList
      starts = [0, step .. total - 1]
      chunkAt start = T.unwords (take safeMax (drop start wordsList))
  in filter (not . T.null) (map chunkAt starts)

replaceRagChunks :: [RagChunk] -> [[Double]] -> SqlPersistT IO ()
replaceRagChunks chunks embeddings = do
  transactionSave
  let doReplace = do
        rawExecute "DELETE FROM rag_chunk" []
        forM_ (zip chunks embeddings) $ \(RagChunk{..}, emb) -> do
          let values =
                [ PersistText rcSource
                , maybe PersistNull PersistText rcSourceId
                , PersistInt64 (fromIntegral rcChunkIndex)
                , PersistText rcContent
                , PersistText (jsonText rcMetadata)
                , PersistText (vectorText emb)
                ]
          rawExecute
            "INSERT INTO rag_chunk (source, source_id, chunk_index, content, metadata, embedding, updated_at) \
            \VALUES (?, ?, ?, ?, ?::jsonb, ?::vector, now())"
            values
  doReplace `catchAny` \err -> do
    transactionUndo
    throwM err

loadRagEmbeddingType :: SqlPersistT IO (Maybe Text)
loadRagEmbeddingType = do
  rows <- rawSql
    "SELECT pg_catalog.format_type(atttypid, atttypmod) \
    \FROM pg_attribute \
    \WHERE attrelid = 'rag_chunk'::regclass AND attname = 'embedding'"
    [] :: SqlPersistT IO [Single Text]
  pure $ case rows of
    [Single typ] -> Just typ
    _ -> Nothing

parseVectorDim :: Text -> Maybe Int
parseVectorDim rawType =
  let trimmed = T.toLower (T.strip rawType)
  in do
    rest <- T.stripPrefix "vector(" trimmed
    numTxt <- T.stripSuffix ")" rest
    readMaybe (T.unpack numTxt)

validateEmbeddingModelDimensions :: Text -> Either Text Int
validateEmbeddingModelDimensions model =
  case openAiEmbedDimensions model of
    Nothing ->
      Left ("OPENAI_EMBED_MODEL desconocido: " <> model <> ". Configura un modelo con dimensiones conocidas.")
    Just dim -> Right dim

validateRagEmbeddingDim :: AppConfig -> ConnectionPool -> IO (Either Text ())
validateRagEmbeddingDim cfg pool =
  case validateEmbeddingModelDimensions (openAiEmbedModel cfg) of
    Left err -> pure (Left err)
    Right expected -> do
      tableReady <- runSqlPool ragTableExists pool
      if not tableReady
        then pure (Left "RAG storage not available (rag_chunk missing). Ensure pgvector is installed and migrations have run.")
        else do
          mType <- runSqlPool loadRagEmbeddingType pool
          case mType >>= parseVectorDim of
            Nothing ->
              pure (Left "No se pudo determinar la dimension de rag_chunk.embedding.")
            Just actual ->
              if actual == expected
                then pure (Right ())
                else
                  pure (Left
                    ("Dimensiones incompatibles. Modelo '" <> openAiEmbedModel cfg <>
                     "' requiere " <> T.pack (show expected) <>
                     " pero rag_chunk.embedding es vector(" <> T.pack (show actual) <>
                     "). Ejecuta migraciones o ajusta OPENAI_EMBED_MODEL."
                    ))

loadRagIndexStats :: SqlPersistT IO RagIndexStats
loadRagIndexStats = do
  exists <- ragTableExists
  if not exists
    then pure RagIndexStats { risCount = 0, risUpdatedAt = Nothing }
    else do
      rows <- rawSql
        "SELECT COUNT(*)::bigint, MAX(updated_at) FROM rag_chunk"
        [] :: SqlPersistT IO [(Single Int64, Single (Maybe UTCTime))]
      case rows of
        [(Single countVal, Single updatedAt)] ->
          pure RagIndexStats
            { risCount = fromIntegral countVal
            , risUpdatedAt = updatedAt
            }
        _ -> pure RagIndexStats { risCount = 0, risUpdatedAt = Nothing }

selectRagChunks :: AppConfig -> [Double] -> SqlPersistT IO [Text]
selectRagChunks cfg embedding =
  let vector = vectorText embedding
      limitVal = fromIntegral (ragTopK cfg) :: Int64
  in do
    exists <- ragTableExists
    if not exists
      then pure []
      else do
        rows <- rawSql
          "SELECT content FROM rag_chunk ORDER BY embedding <=> ?::vector LIMIT ?"
          [PersistText vector, PersistInt64 limitVal] :: SqlPersistT IO [Single Text]
        pure [val | Single val <- rows]

embedTexts :: AppConfig -> [Text] -> IO (Either Text [[Double]])
embedTexts cfg inputs
  | null inputs = pure (Right [])
  | otherwise = do
      case openAiApiKey cfg of
        Nothing -> pure (Left "OPENAI_API_KEY no configurada")
        Just _ -> do
          let batches = chunkList (ragEmbedBatchSize cfg) inputs
          results <- forM batches (callOpenAIEmbeddings cfg)
          pure (concatEmbeddings results)
  where
    concatEmbeddings xs =
      let failures = [err | Left err <- xs]
      in case failures of
        (err:_) -> Left err
        [] -> Right (concat [vals | Right vals <- xs])

callOpenAIEmbeddings :: AppConfig -> [Text] -> IO (Either Text [[Double]])
callOpenAIEmbeddings cfg inputs =
  case openAiApiKey cfg of
    Nothing -> pure (Left "OPENAI_API_KEY no configurada")
    Just key -> do
      manager <- newManager tlsManagerSettings
      reqBase <- parseRequest "https://api.openai.com/v1/embeddings"
      let body = encode EmbeddingReq
            { model = openAiEmbedModel cfg
            , input = inputs
            }
          req =
            reqBase
              { method = "POST"
              , requestHeaders =
                  [ ("Content-Type", "application/json")
                  , ("Authorization", "Bearer " <> TE.encodeUtf8 key)
                  ]
              , requestBody = RequestBodyLBS body
              }
      resp <- httpLbs req manager
      let status = statusCode (responseStatus resp)
      let raw = responseBody resp
      if status < 200 || status >= 300
        then case eitherDecode raw of
          Right OpenAIErrorResp{..} ->
            pure (Left ("OpenAI embeddings error: " <> oeMessage oeError))
          Left _ ->
            pure (Left ("OpenAI embeddings error (status " <> T.pack (show status) <> ")."))
        else
          case eitherDecode raw of
            Left err -> pure (Left (T.pack err))
            Right (EmbeddingResp embeddings) ->
              let ordered = map embedding (sortOn index embeddings)
              in pure (Right ordered)

formatUtc :: UTCTime -> Text
formatUtc ts = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC" ts)

formatMoney :: Int -> Text -> Text
formatMoney cents currency =
  let amount = fromIntegral cents / 100 :: Double
  in T.pack (showFFloat (Just 2) amount "") <> " " <> currency

jsonText :: Value -> Text
jsonText = TE.decodeUtf8 . toStrict . encode

vectorText :: [Double] -> Text
vectorText xs = "[" <> T.intercalate "," (map renderFloat xs) <> "]"

renderFloat :: Double -> Text
renderFloat val = T.pack (showFFloat (Just 6) val "")

chunkList :: Int -> [a] -> [[a]]
chunkList size items
  | size <= 0 = [items]
  | null items = []
  | otherwise =
      let (chunk, rest) = splitAt size items
      in chunk : chunkList size rest

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty Nothing = Nothing
nonEmpty (Just txt) =
  let trimmed = T.strip txt
  in if T.null trimmed then Nothing else Just trimmed

nonEmptyList :: Maybe [Text] -> Maybe Text
nonEmptyList Nothing = Nothing
nonEmptyList (Just xs) =
  let cleaned = map T.strip (filter (not . T.null) xs)
  in if null cleaned then Nothing else Just (T.intercalate ", " cleaned)

boolLabel :: Bool -> Text
boolLabel True = "sí"
boolLabel False = "no"

partyName :: M.Party -> Text
partyName party =
  let display = T.strip (M.partyDisplayName party)
  in if T.null display
       then fromMaybe "Sin nombre" (M.partyLegalName party)
       else display

ragTableExists :: SqlPersistT IO Bool
ragTableExists = do
  rows <- rawSql
    "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = 'public' AND table_name = 'rag_chunk')"
    [] :: SqlPersistT IO [Single Bool]
  pure $ case rows of
    [Single True] -> True
    _ -> False
