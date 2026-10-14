{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.RagStore
  ( refreshRagIndex
  , refreshRagIndexWith
  , ensureRagIndex
  , retrieveRagContext
  , getRagIndexStats
  , availabilityOverlaps
  , callOpenAIEmbeddingsWith
  , shouldUseLocalEmbeddingFallback
  , validateEmbeddingModelDimensions
  , validateEmbeddingResponseOrder
  , validateEmbeddingResponseDimensions
  ) where

import           Control.Monad          (forM, forM_)
import           Control.Exception.Safe (catchAny)
import           Data.Aeson             (Value, ToJSON(..), FromJSON(..), object, (.=), encode, eitherDecode, withObject, (.:))
import           Data.ByteString.Lazy   (ByteString, toStrict)
import           Data.Char
  ( GeneralCategory(Format, LineSeparator, ParagraphSeparator)
  , generalCategory
  , isAlphaNum
  , isSpace
  , ord
  , toLower
  )
import           Data.Int               (Int64)
import           Data.List              (foldl')
import qualified Data.IntMap.Strict     as IntMap
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Time              (UTCTime, diffUTCTime, getCurrentTime)
import           Database.Persist       (Entity(..), Key, SelectOpt(..), getEntity, selectList)
import           Database.Persist.Sql   (PersistValue(..), Single(..), SqlPersistT, toSqlKey, rawExecute, rawSql, runSqlPool)
import           GHC.Generics           (Generic)
import           Network.HTTP.Client    (Request(..), Response, httpLbs, parseRequest, responseBody, responseStatus, RequestBody(..))
import           Network.HTTP.Types.Status (statusCode)
import           Numeric                (showFFloat)
import           System.IO              (hPutStrLn, stderr)
import           Text.Read              (readMaybe)

import           TDF.Config             (AppConfig(..), openAiEmbedDimensions, ragEmbeddingDim)
import           TDF.DB                 (ConnectionPool, sharedTlsManager)
import qualified TDF.Trials.Models      as Trials

-- Only a course can become a document for the customer-facing embedding index.
-- There is no generic source/content constructor for private operational data.
data PublicCourseDocument = PublicCourseDocument
  { courseDocumentSlug :: Text
  , courseDocumentContent :: Text
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
refreshRagIndex cfg = refreshRagIndexWith (embedTexts cfg) cfg

-- The injected embedding adapter permits payload/failure checks without a
-- provider request. Production always supplies the same embedTexts adapter.
refreshRagIndexWith
  :: ([Text] -> IO (Either Text [[Double]]))
  -> AppConfig -> ConnectionPool -> IO (Either Text Int)
refreshRagIndexWith embed cfg pool = do
  dimCheck <- validateRagEmbeddingDim cfg pool
  case dimCheck of
    Left err -> pure (Left err)
    Right () -> do
      docs <- runSqlPool buildPublicCourseDocuments pool
      let chunks = concatMap (docToChunks cfg) docs
      if null chunks
        then do
          runSqlPool (replaceRagChunks [] []) pool
          pure (Right 0)
        else do
          embedResult <- embed (map rcContent chunks)
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

buildPublicCourseDocuments :: SqlPersistT IO [PublicCourseDocument]
buildPublicCourseDocuments = do
  courses <- selectList [] [Desc Trials.CourseUpdatedAt, LimitTo 50]
  pure (map courseToDoc courses)

courseToDoc :: Entity Trials.Course -> PublicCourseDocument
courseToDoc (Entity _ c) =
  PublicCourseDocument
    { courseDocumentSlug = Trials.courseSlug c
    , courseDocumentContent = T.intercalate " · " (catMaybes
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
    }
  where
    formatStartHour hour = T.pack (show hour) <> ":00"
    formatHours hours = T.pack (show hours) <> "h"

-- Retained pure compatibility helper; availability is no longer indexed.
availabilityOverlaps :: UTCTime -> UTCTime -> UTCTime -> UTCTime -> Bool
availabilityOverlaps windowStart windowEnd eventStart eventEnd =
  eventStart <= windowEnd && eventEnd >= windowStart

docToChunks :: AppConfig -> PublicCourseDocument -> [RagChunk]
docToChunks cfg PublicCourseDocument{..} =
  let chunks = chunkWords (ragChunkWords cfg) (ragChunkOverlap cfg) courseDocumentContent
  in zipWith
      (\idx content ->
        RagChunk
          { rcSource = "course"
          , rcSourceId = Just courseDocumentSlug
          , rcChunkIndex = idx
          , rcContent = content
          , rcMetadata = object ["slug" .= courseDocumentSlug]
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
-- runSqlPool owns the transaction; do not commit before deleting old rows.
replaceRagChunks chunks embeddings = do
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

validateEmbeddingResponseOrder :: Int -> [(Int, [Double])] -> Either Text [[Double]]
validateEmbeddingResponseOrder expectedCount indexedEmbeddings
  | expectedCount < 0 =
      Left "Embedding response expected count must not be negative"
  | length indexedEmbeddings /= expectedCount =
      Left
        ( "Embedding response size mismatch: expected "
          <> T.pack (show expectedCount)
          <> " but got "
          <> T.pack (show (length indexedEmbeddings))
        )
  | otherwise =
      case outOfRangeIndexes of
        idx:_ ->
          Left ("Embedding response index out of range: " <> T.pack (show idx))
        [] ->
          case duplicateIndexes of
            idx:_ ->
              Left ("Embedding response duplicate index: " <> T.pack (show idx))
            [] ->
              traverse lookupEmbedding [0 .. expectedCount - 1]
  where
    grouped =
      Map.fromListWith
        (++)
        [(idx, [embeddingValue]) | (idx, embeddingValue) <- indexedEmbeddings]
    outOfRangeIndexes =
      [idx | (idx, _) <- indexedEmbeddings, idx < 0 || idx >= expectedCount]
    duplicateIndexes =
      [idx | (idx, values) <- Map.toList grouped, length values > 1]
    lookupEmbedding idx =
      case Map.lookup idx grouped of
        Just [embeddingValue] -> Right embeddingValue
        _ ->
          Left ("Embedding response missing index: " <> T.pack (show idx))

validateEmbeddingResponseDimensions :: Int -> [[Double]] -> Either Text [[Double]]
validateEmbeddingResponseDimensions expectedDim embeddings
  | expectedDim <= 0 =
      Left "Embedding response expected dimension must be positive"
  | otherwise =
      case dimensionMismatches of
        (idx, actualDim):_ ->
          Left
            ( "Embedding response dimension mismatch at index "
              <> T.pack (show idx)
              <> ": expected "
              <> T.pack (show expectedDim)
              <> " but got "
              <> T.pack (show actualDim)
            )
        [] ->
          case nonFiniteValues of
            (idx, valueIdx):_ ->
              Left
                ( "Embedding response non-finite value at index "
                  <> T.pack (show idx)
                  <> " position "
                  <> T.pack (show valueIdx)
                )
            [] -> Right embeddings
  where
    dimensionMismatches =
      [ (idx, length embeddingValue)
      | (idx, embeddingValue) <- zip [0 :: Int ..] embeddings
      , length embeddingValue /= expectedDim
      ]
    nonFiniteValues =
      [ (idx, valueIdx)
      | (idx, embeddingValue) <- zip [0 :: Int ..] embeddings
      , (valueIdx, value) <- zip [0 :: Int ..] embeddingValue
      , not (isFiniteDouble value)
      ]

isFiniteDouble :: Double -> Bool
isFiniteDouble value = not (isNaN value || isInfinite value)

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
          -- Current callers produce customer-facing replies. Internal chunks
          -- have no object authority here. Use the index only to rank public
          -- course IDs; render current public fields, never cached raw content.
          "SELECT c.id FROM rag_chunk chunk JOIN course c ON c.slug = chunk.source_id\
          \ WHERE chunk.source = 'course'\
          \ ORDER BY embedding <=> ?::vector LIMIT ?"
          [PersistText vector, PersistInt64 limitVal] :: SqlPersistT IO [Single Int64]
        docs <- forM rows $ \(Single courseId) -> do
          course <- getEntity (toSqlKey courseId :: Key Trials.Course)
          pure (courseDocumentContent . courseToDoc <$> course)
        pure (catMaybes docs)

embedTexts :: AppConfig -> [Text] -> IO (Either Text [[Double]])
embedTexts cfg inputs
  | null inputs = pure (Right [])
  | otherwise = do
      let fallback = do
            hPutStrLn stderr "[rag] Usando embeddings locales (fallback)."
            pure (Right (localEmbeddings cfg inputs))
      case openAiApiKey cfg of
        Nothing -> fallback
        Just _ -> do
          let batches = chunkList (ragEmbedBatchSize cfg) inputs
          results <- forM batches (callOpenAIEmbeddings cfg)
          case concatEmbeddings results of
            Left err -> do
              if shouldUseLocalEmbeddingFallback err
                then do
                  hPutStrLn
                    stderr
                    ("[rag] OpenAI embeddings fallo, usando fallback. " <> T.unpack err)
                  pure (Right (localEmbeddings cfg inputs))
                else pure (Left err)
            Right values -> pure (Right values)
  where
    concatEmbeddings xs =
      let failures = [err | Left err <- xs]
      in case failures of
        (err:_) -> Left err
        [] -> Right (concat [vals | Right vals <- xs])

shouldUseLocalEmbeddingFallback :: Text -> Bool
shouldUseLocalEmbeddingFallback rawError =
  not responseShapeDrift && not credentialOrAccountFailure
  where
    msg = T.toLower (T.strip rawError)
    responseShapeDrift =
      "openai embeddings response invalid:" `T.isPrefixOf` msg
    credentialOrAccountFailure =
      any (`T.isInfixOf` msg)
        [ "(status 400)"
        , "(status 401)"
        , "(status 402)"
        , "(status 403)"
        , "(status 404)"
        , "authentication"
        , "authentication_error"
        , "bad request"
        , "invalid_request_error"
        , "invalid_api_key"
        , "invalid api key"
        , "incorrect api key"
        , "invalid model"
        , "invalid model id"
        , "invalid model name"
        , "model_not_found"
        , "model not found"
        , "does not exist or you do not have access"
        , "does not have access to model"
        , "doesn't have access to model"
        , "do not have access to model"
        , "don't have access to model"
        , "no access to model"
        , "not a valid model"
        , "unknown model"
        , "unauthorized"
        , "unauthorised"
        , "permission_denied"
        , "permission denied"
        , "insufficient_quota"
        , "quota"
        , "billing"
        , "billing_hard_limit"
        , "payment_required"
        , "payment required"
        , "payment method"
        ]

localEmbeddings :: AppConfig -> [Text] -> [[Double]]
localEmbeddings cfg inputs =
  let dim = max 1 (ragEmbeddingDim cfg)
  in map (localEmbedText dim) inputs

localEmbedText :: Int -> Text -> [Double]
localEmbedText dim txt =
  let cleaned = T.map (\c -> if isAlphaNum c then toLower c else ' ') txt
      tokens = filter (not . T.null) (T.words cleaned)
      counts = foldl' (addToken dim) IntMap.empty tokens
      rawVec = [fromIntegral (IntMap.findWithDefault 0 idx counts) | idx <- [0 .. dim - 1]]
      norm = sqrt (sum (map (\v -> v * v) rawVec))
  in if norm <= 0 then rawVec else map (/ norm) rawVec

addToken :: Int -> IntMap.IntMap Int -> Text -> IntMap.IntMap Int
addToken dim acc token =
  let idx = hashToken dim token
  in IntMap.insertWith (+) idx 1 acc

hashToken :: Int -> Text -> Int
hashToken dim token =
  let seed = 5381 :: Int
      hashed = foldl' (\h c -> h * 33 + ord c) seed (T.unpack token)
      capped = hashed `mod` max 1 dim
  in if capped < 0 then capped + dim else capped

callOpenAIEmbeddings :: AppConfig -> [Text] -> IO (Either Text [[Double]])
callOpenAIEmbeddings cfg inputs =
  callOpenAIEmbeddingsWith (\req -> do
    manager <- pure sharedTlsManager
    httpLbs req manager
  ) cfg inputs

callOpenAIEmbeddingsWith
  :: (Request -> IO (Response ByteString))
  -> AppConfig
  -> [Text]
  -> IO (Either Text [[Double]])
callOpenAIEmbeddingsWith runEmbeddingRequest cfg inputs =
  case openAiApiKey cfg of
    Nothing -> pure (Left "OPENAI_API_KEY no configurada")
    Just key -> do
      let embedBase = T.dropWhileEnd (== '/') (chatKitApiBase cfg)
      reqBase <- parseRequest (T.unpack (embedBase <> "/v1/embeddings"))
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
      respResult <-
        (Right <$> runEmbeddingRequest req) `catchAny` \err ->
          pure
            ( Left
                ( "OpenAI embeddings request failed: "
                    <> sanitizeOpenAIEmbeddingErrorMessage (T.pack (show err))
                )
            )
      case respResult of
        Left err -> pure (Left err)
        Right resp -> do
          let status = statusCode (responseStatus resp)
          let raw = responseBody resp
          if status < 200 || status >= 300
            then case eitherDecode raw of
              Right OpenAIErrorResp{..} ->
                pure
                  ( Left
                      ( "OpenAI embeddings error (status "
                          <> T.pack (show status)
                          <> "): "
                          <> sanitizeOpenAIEmbeddingErrorMessage (oeMessage oeError)
                      )
                  )
              Left _ ->
                pure (Left ("OpenAI embeddings error (status " <> T.pack (show status) <> ")."))
            else
              case eitherDecode raw of
                Left err ->
                  pure (Left ("OpenAI embeddings response invalid: " <> T.pack err))
                Right (EmbeddingResp embeddings) ->
                  pure (validateOpenAIEmbeddingPayload cfg (length inputs) embeddings)

validateOpenAIEmbeddingPayload :: AppConfig -> Int -> [EmbeddingData] -> Either Text [[Double]]
validateOpenAIEmbeddingPayload cfg expectedCount embeddings =
  case validated of
    Left err -> Left ("OpenAI embeddings response invalid: " <> err)
    Right values -> Right values
  where
    validated = do
      expectedDim <- validateEmbeddingModelDimensions (openAiEmbedModel cfg)
      orderedEmbeddings <-
        validateEmbeddingResponseOrder
          expectedCount
          [(index item, embedding item) | item <- embeddings]
      validateEmbeddingResponseDimensions expectedDim orderedEmbeddings

sanitizeOpenAIEmbeddingErrorMessage :: Text -> Text
sanitizeOpenAIEmbeddingErrorMessage raw =
  let sanitized =
        redactOpenAIEmbeddingErrorSecrets $
          T.strip (T.map sanitizeChar raw)
  in if T.length sanitized <= maxOpenAIEmbeddingErrorChars
       then sanitized
       else T.take maxOpenAIEmbeddingErrorChars sanitized <> " [truncated]"
  where
    sanitizeChar ch
      | ch == '\DEL' || ch < ' ' = ' '
      | generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator] = ' '
      | otherwise = ch

maxOpenAIEmbeddingErrorChars :: Int
maxOpenAIEmbeddingErrorChars = 500

redactOpenAIEmbeddingErrorSecrets :: Text -> Text
redactOpenAIEmbeddingErrorSecrets =
  redactOpenAIEmbeddingSecretFields . redactOpenAIEmbeddingBearerTokens

redactOpenAIEmbeddingBearerTokens :: Text -> Text
redactOpenAIEmbeddingBearerTokens = go Nothing
  where
    go _ textValue
      | T.null textValue = ""
    go previous textValue =
      case matchOpenAIEmbeddingBearerToken previous textValue of
        Just (prefix, rest) ->
          prefix <> "[redacted]" <> go Nothing rest
        Nothing ->
          let ch = T.head textValue
          in T.singleton ch <> go (Just ch) (T.tail textValue)

matchOpenAIEmbeddingBearerToken :: Maybe Char -> Text -> Maybe (Text, Text)
matchOpenAIEmbeddingBearerToken previous textValue
  | not (isOpenAIEmbeddingSecretBoundary previous) = Nothing
  | not ("bearer" `T.isPrefixOf` T.toLower textValue) = Nothing
  | otherwise =
      let bearerText = T.take 6 textValue
          afterBearer = T.drop 6 textValue
          (between, tokenStart) = T.span isSpace afterBearer
          (openingQuote, tokenText, isValueEnd) =
            consumeOpenAIEmbeddingValueOpeningQuote tokenStart
          (tokenValue, rest) = T.break isValueEnd tokenText
      in if T.null between
            || T.null tokenValue
            || not (T.any isOpenAIEmbeddingSecretAtom tokenValue)
           then Nothing
           else Just (bearerText <> between <> openingQuote, rest)

redactOpenAIEmbeddingSecretFields :: Text -> Text
redactOpenAIEmbeddingSecretFields = go Nothing
  where
    go _ textValue
      | T.null textValue = ""
    go previous textValue =
      case matchOpenAIEmbeddingSecretField previous textValue of
        Just (prefix, rest) ->
          prefix <> "[redacted]" <> go Nothing rest
        Nothing ->
          let ch = T.head textValue
          in T.singleton ch <> go (Just ch) (T.tail textValue)

matchOpenAIEmbeddingSecretField :: Maybe Char -> Text -> Maybe (Text, Text)
matchOpenAIEmbeddingSecretField previous textValue
  | not (isOpenAIEmbeddingSecretBoundary previous) = Nothing
  | otherwise = firstMatch openAIEmbeddingSecretFields
  where
    lowered = T.toLower textValue

    firstMatch [] = Nothing
    firstMatch (fieldName:rest) =
      case parseOpenAIEmbeddingSecretField fieldName lowered textValue of
        Just match -> Just match
        Nothing -> firstMatch rest

openAIEmbeddingSecretFields :: [Text]
openAIEmbeddingSecretFields =
  [ "access_token"
  , "api_key"
  , "api-key"
  , "apikey"
  , "authorization"
  , "client_secret"
  , "id_token"
  , "refresh_token"
  , "x-api-key"
  ]

parseOpenAIEmbeddingSecretField :: Text -> Text -> Text -> Maybe (Text, Text)
parseOpenAIEmbeddingSecretField fieldName lowered textValue
  | not (fieldName `T.isPrefixOf` lowered) = Nothing
  | otherwise = do
      let fieldLength = T.length fieldName
          fieldText = T.take fieldLength textValue
          afterField = T.drop fieldLength textValue
          (closingQuote, afterClosingQuote) =
            consumeOpenAIEmbeddingOptionalQuote afterField
          (beforeSeparator, separatorCandidate) = T.span isSpace afterClosingQuote
      (separator, afterSeparator) <- T.uncons separatorCandidate
      if separator == '=' || separator == ':'
        then
          let (afterSeparatorSpace, valueStart) = T.span isSpace afterSeparator
              (openingQuote, valueText, isValueEnd) =
                consumeOpenAIEmbeddingValueOpeningQuote valueStart
              (_, rest) = T.break isValueEnd valueText
              prefix =
                fieldText
                  <> closingQuote
                  <> beforeSeparator
                  <> T.singleton separator
                  <> afterSeparatorSpace
                  <> openingQuote
          in Just (prefix, rest)
        else Nothing

isOpenAIEmbeddingSecretBoundary :: Maybe Char -> Bool
isOpenAIEmbeddingSecretBoundary Nothing = True
isOpenAIEmbeddingSecretBoundary (Just ch) =
  not (isAlphaNum ch || ch == '_' || ch == '-')

isOpenAIEmbeddingSecretAtom :: Char -> Bool
isOpenAIEmbeddingSecretAtom ch =
  isAlphaNum ch || ch `elem` (".-_~+/=" :: String)

consumeOpenAIEmbeddingOptionalQuote :: Text -> (Text, Text)
consumeOpenAIEmbeddingOptionalQuote textValue =
  case T.uncons textValue of
    Just ('"', rest) -> ("\"", rest)
    Just ('\'', rest) -> ("'", rest)
    _ -> ("", textValue)

consumeOpenAIEmbeddingValueOpeningQuote :: Text -> (Text, Text, Char -> Bool)
consumeOpenAIEmbeddingValueOpeningQuote textValue =
  case T.uncons textValue of
    Just ('"', rest) -> ("\"", rest, (== '"'))
    Just ('\'', rest) -> ("'", rest, (== '\''))
    _ -> ("", textValue, isUnquotedOpenAIEmbeddingSecretValueEnd)

isUnquotedOpenAIEmbeddingSecretValueEnd :: Char -> Bool
isUnquotedOpenAIEmbeddingSecretValueEnd ch =
  isSpace ch || ch `elem` ("&,;}]" :: String)

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

ragTableExists :: SqlPersistT IO Bool
ragTableExists = do
  rows <- rawSql
    "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = 'public' AND table_name = 'rag_chunk')"
    [] :: SqlPersistT IO [Single Bool]
  pure $ case rows of
    [Single True] -> True
    _ -> False
