{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | Embeddings behind directory search (DIR-SEMANTIC-SEARCH-001).
--
-- Two effects live here, both off unless @DIRECTORY_SEMANTIC_SEARCH_ENABLED@
-- is set: the background sweep that keeps one vector per public profile and
-- classified, and the lookup of a query vector inside a search request. The
-- rule, the SQL text and every decision are in "TDF.Directory.SemanticSearch";
-- this module only runs them.
--
-- The provider is called through an injected 'Embedder', so tests exercise
-- the sweep and the lookup without a network. The production embedder never
-- substitutes a locally computed vector for a failed call.
module TDF.Directory.SemanticIndex
  ( Embedder
  , SweepStats (..)
  , emptySweepStats
  , providerEmbedder
  , semanticModel
  , sweepDirectoryEmbeddingsWith
  , directorySemanticIndexIterationWith
  , startDirectorySemanticIndexWorker
  , resolveQueryEmbedding
  , resolveQueryEmbeddingWith
  , queryCacheKey
  ) where

import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Exception.Safe   (tryAny)
import           Control.Monad            (forM, forever, void)
import           Crypto.Hash              (Digest, SHA256, hash)
import           Data.Aeson               (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Int                 (Int64)
import           Data.Maybe               (listToMaybe)
import           Data.Pool                (Pool)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Database.Persist         (PersistValue (..))
import           Database.Persist.Sql     (Single (..), SqlBackend, SqlPersistT, rawExecute,
                                           rawExecuteCount, rawSql, runSqlPool)
import           Network.HTTP.Client      (httpLbs, responseTimeout, responseTimeoutMicro)
import           System.IO                (hPutStrLn, stderr)
import           System.Timeout           (timeout)

import           TDF.Config               (AppConfig (..))
import           TDF.DB                   (Env (..), sharedTlsManager)
import           TDF.Directory.SemanticSearch
import           TDF.RagStore             (callOpenAIEmbeddingsWith, validateEmbeddingModelDimensions)

-- | Texts in, one vector per text out, in order. 'Left' is any failure.
type Embedder = [Text] -> IO (Either Text [[Double]])

-- | What one sweep did. @sweepSkipped@ counts claimed documents that got no
-- vector (provider failure, unusable vector, or the document changed or was
-- hidden before the write).
data SweepStats = SweepStats
  { sweepPurged  :: !Int
  , sweepWritten :: !Int
  , sweepSkipped :: !Int
  } deriving (Eq, Show)

emptySweepStats :: SweepStats
emptySweepStats = SweepStats 0 0 0

-- | The model whose vectors fit the migrated columns, if the configured one
-- does.
semanticModel :: AppConfig -> Maybe Text
semanticModel cfg =
  case validateEmbeddingModelDimensions (openAiEmbedModel cfg) of
    Right dimension | dimension == embeddingDimension -> Just (openAiEmbedModel cfg)
    _                                                 -> Nothing

-- | The configured provider with a total deadline. No local fallback: a
-- failed, late or malformed answer is 'Left'.
providerEmbedder :: AppConfig -> Int -> Embedder
providerEmbedder cfg micros inputs = do
  answer <- tryAny (timeout micros (callOpenAIEmbeddingsWith run cfg inputs))
  pure $ case answer of
    Right (Just result) -> result
    Right Nothing       -> Left "embedding request timed out"
    Left _              -> Left "embedding request failed"
  where
    run request =
      httpLbs request {responseTimeout = responseTimeoutMicro micros} sharedTlsManager

storageInstalled :: SqlPersistT IO Bool
storageInstalled = do
  rows <- rawSql
    "SELECT to_regclass('public.directory_search_embedding') IS NOT NULL \
    \AND to_regclass('public.directory_query_embedding') IS NOT NULL \
    \AND to_regclass('public.directory_query_embedding_budget') IS NOT NULL"
    []
  pure (rows == [Single True])

-- | One sweep: purge, prune the query cache, claim, embed outside any
-- transaction, write under the guard. A no-op before the migration.
sweepDirectoryEmbeddingsWith :: Embedder -> Text -> Int -> Pool SqlBackend -> IO SweepStats
sweepDirectoryEmbeddingsWith embed model batch pool = do
  installed <- runSqlPool storageInstalled pool
  if not installed
    then pure emptySweepStats
    else do
      purged <- flip runSqlPool pool $ do
        removed <- rawExecuteCount purgeDocumentEmbeddingsSql []
        rawExecute pruneQueryCacheSql
          [PersistInt64 (fromIntegral queryCacheDays), PersistInt64 (fromIntegral queryCacheMaxRows)]
        rawExecute pruneQueryBudgetSql []
        pure removed
      (prepared, unsendable) <- claimSendable (max 1 batch) 0 0 [] 0
      written <-
        if null prepared
          then pure 0
          else do
            answer <- embed [input | (_, _, _, input) <- prepared]
            case answer of
              Right vectors | length vectors == length prepared ->
                fmap sum . forM (zip prepared vectors) $ \((kind, ident, digest, _), vector) ->
                  if usableVector embeddingDimension vector
                    then fromIntegral <$> runSqlPool
                      (rawExecuteCount writeDocumentEmbeddingSql
                        [ PersistText digest, PersistText embeddingProviderName, PersistText model
                        , PersistText (vectorLiteral vector), PersistText kind, PersistText ident
                        , PersistText digest
                        ])
                      pool
                    else pure (0 :: Int)
              _ -> pure 0
      pure SweepStats
        { sweepPurged = fromIntegral (purged :: Int64)
        , sweepWritten = written
        , sweepSkipped = unsendable + length prepared - written
        }
  where
    -- Claim pages, oldest change first, until the batch holds documents that
    -- can be sent. A document whose public text redacts to nothing is never
    -- embedded, so it stays first in the claim order; reading past it within
    -- the sweep keeps it from holding up every later document. The page count
    -- is bounded so one sweep stays a bounded amount of work.
    claimSendable wanted pages offset kept skipped = do
      rows <- runSqlPool
        (rawSql claimDocumentsSql
          [ PersistText embeddingProviderName, PersistText model
          , PersistInt64 (fromIntegral pageSize), PersistInt64 (fromIntegral offset) ])
        pool
      let sendable =
            [ (kind, ident, digest, input)
            | (Single kind, Single ident, Single digest, Single text) <-
                rows :: [(Single Text, Single Text, Single Text, Single (Maybe Text))]
            , let input = embeddingInput (maybe "" id text)
            , not (T.null input)
            ]
          kept' = kept <> take (wanted - length kept) sendable
          skipped' = skipped + length rows - length sendable
      if length kept' >= wanted || length rows < pageSize || pages + 1 >= claimPageLimit
        then pure (kept', skipped')
        else claimSendable wanted (pages + 1 :: Int) (offset + length rows) kept' skipped'
      where
        pageSize = max wanted claimPageSize

-- | Rows read per claim page, and pages read per sweep, while looking for
-- documents that can be sent.
claimPageSize, claimPageLimit :: Int
claimPageSize = 100
claimPageLimit = 50

-- | One iteration of the loop. The error line is fixed text: no exception,
-- document or provider data reaches the log (OPS-WORKER-001).
directorySemanticIndexIterationWith :: IO SweepStats -> (String -> IO ()) -> (String -> IO ()) -> IO ()
directorySemanticIndexIterationWith tick logError logInfo = do
  result <- tryAny tick
  case result of
    Left _ -> void $ tryAny $ logError
      "{\"component\":\"directory-semantic-index\",\"level\":\"error\",\"message\":\"tick failed\"}"
    Right stats
      | stats /= emptySweepStats -> void $ tryAny $ logInfo $ BL.unpack $ encode $ object
          [ "component" .= ("directory-semantic-index" :: Text), "level" .= ("info" :: Text)
          , "purged" .= sweepPurged stats, "written" .= sweepWritten stats
          , "skipped" .= sweepSkipped stats
          ]
      | otherwise -> pure ()

-- | Start the sweep when the flag is on and the configured model fits.
startDirectorySemanticIndexWorker :: Env -> IO ()
startDirectorySemanticIndexWorker Env {envPool, envConfig}
  | not (directorySemanticSearchEnabled envConfig) =
      putStrLn "[directory-semantic-index] disabled by configuration"
  | otherwise =
      case semanticModel envConfig of
        Nothing ->
          putStrLn "[directory-semantic-index] disabled: the embedding model does not fit the index"
        Just model -> void . forkIO . forever $ do
          directorySemanticIndexIterationWith
            (sweepDirectoryEmbeddingsWith
              (providerEmbedder envConfig sweepProviderMicros) model (ragEmbedBatchSize envConfig) envPool)
            (hPutStrLn stderr)
            putStrLn
          threadDelay (sweepSeconds * 1000000)
  where
    sweepProviderMicros = 60 * 1000000

-- | SHA-256, in hexadecimal, of provider, model and normalised query.
queryCacheKey :: Text -> Text -> Text -> Text
queryCacheKey provider model query =
  T.pack (show (hash (TE.encodeUtf8 (queryCacheKeyInput provider model query)) :: Digest SHA256))

-- | The query vector for a search request, or 'Nothing', in which case the
-- request runs today's statement. Never throws.
resolveQueryEmbedding :: AppConfig -> Pool SqlBackend -> Text -> IO (Maybe QueryEmbedding)
resolveQueryEmbedding cfg =
  resolveQueryEmbeddingWith
    (providerEmbedder cfg queryTimeoutMicros)
    (directorySemanticSearchEnabled cfg)
    (semanticModel cfg)

resolveQueryEmbeddingWith
  :: Embedder -> Bool -> Maybe Text -> Pool SqlBackend -> Text -> IO (Maybe QueryEmbedding)
resolveQueryEmbeddingWith embed flagOn configuredModel pool query =
  case configuredModel of
    Just model | queryEligible flagOn query ->
      either (const Nothing) id <$> tryAny (resolve model)
    _ -> pure Nothing
  where
    resolve model = do
      let key = queryCacheKey embeddingProviderName model query
          found vector = QueryEmbedding embeddingProviderName model vector
          spaceParams = [PersistText key, PersistText embeddingProviderName, PersistText model]
      cached <- flip runSqlPool pool $ do
        installed <- storageInstalled
        if not installed
          then pure Nothing
          else do
            rows <- rawSql lookupQueryEmbeddingSql spaceParams
            case listToMaybe rows of
              Just (Single vector) -> pure (Just (Right (vector :: Text)))
              Nothing -> do
                spent <- rawSql spendQueryBudgetSql [PersistInt64 (fromIntegral queryCallsPerMinute)]
                pure (Just (Left (spent == [Single True])))
      case cached of
        Nothing -> pure Nothing
        Just (Right vector) -> pure (Just (found vector))
        Just (Left False) -> pure Nothing
        Just (Left True) -> do
          answer <- embed [normalizeQueryText query]
          case answer of
            Right [vector] | usableVector embeddingDimension vector -> do
              let literal = vectorLiteral vector
              runSqlPool (rawExecute storeQueryEmbeddingSql (spaceParams <> [PersistText literal])) pool
              pure (Just (found literal))
            _ -> pure Nothing
