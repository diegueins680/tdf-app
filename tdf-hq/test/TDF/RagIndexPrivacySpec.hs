{-# LANGUAGE OverloadedStrings #-}
module TDF.RagIndexPrivacySpec (spec) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.String (fromString)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Database.PostgreSQL.Simple as PG
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Postgresql (createPostgresqlPoolModified)
import Database.Persist.Sql (ConnectionPool, rawExecute, rawSql, runSqlPool, Single(..))
import Database.PostgreSQL.Simple (SqlError, sqlState)
import System.Environment (lookupEnv)
import Test.Hspec
import TDF.Config (AppConfig(..), loadConfig)
import TDF.RagStore (ensureRagIndex, refreshRagIndexWith)

spec :: Spec
spec = describe "rag-index-privacy-postgresql" $ do
  it "rejects ambiguous or redirected database authority before connecting" $ do
    map safeConnection
      [ "postgresql://127.0.0.1:password@external.example/production_test"
      , "postgresql://localhost:5432/fixture_test?host=external.example"
      , "postgresql://127.0.0.1:5432/a/fixture_test"
      , "postgresql://external.example/fixture_test"
      ] `shouldBe` replicate 4 False
    safeConnection "postgresql://127.0.0.1:55439/tdf_rag_index_privacy_test" `shouldBe` True
  ci <- runIO (lookupEnv "CI")
  configured <- runIO (lookupEnv "TDF_RAG_INDEX_DATABASE_URL")
  overrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  case configured of
    Nothing -> it "requires the isolated migrated PostgreSQL runner" $
      pendingWith "Run scripts/test-rag-index-privacy.sh"
    Just _ | any (maybe False (not . null)) overrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset inherited libpq routing overrides"
    Just connection | not (safeConnection connection)
      || ("postgresql://postgres:postgres@postgres:" `T.isPrefixOf` T.pack connection
          && ci /= Just "true") ->
      it "refuses a non-disposable database" $
        expectationFailure "Use the isolated integration runner"
    Just connection -> around (withFixture connection) $ do
      it "embeds only course payloads and replaces legacy chunks" $ \pool -> do
        publicCourse pool
        cfg <- testConfig
        sent <- newIORef []
        result <- refreshRagIndexWith (\inputs -> do
          writeIORef sent inputs
          pure (Right (vectors inputs))) cfg pool
        result `shouldSatisfy` either (const False) (> 0)
        inputs <- readIORef sent
        T.unwords inputs `shouldSatisfy` T.isInfixOf "PUBLIC_COURSE_SENTINEL"
        T.unwords inputs `shouldSatisfy` (not . T.isInfixOf "PRIVATE_")
        rows <- indexRows pool
        rows `shouldSatisfy` (not . null)
        map fst rows `shouldSatisfy` all (== "course")
        T.unwords (map snd rows) `shouldSatisfy` (not . T.isInfixOf "PRIVATE_")

      it "replaces an empty public corpus without calling the embedding adapter" $ \pool -> do
        cfg <- testConfig
        refreshRagIndexWith (\_ -> do
          expectationFailure "Empty public corpus must not reach an embedding provider"
          pure (Left "unexpected adapter call")) cfg pool `shouldReturn` Right 0
        indexRows pool `shouldReturn` []

      it "purges legacy chunks even when the remaining public index is fresh" $ \pool -> do
        cfg <- testConfig
        ensureRagIndex cfg pool `shouldReturn` Right False
        indexRows pool `shouldReturn` [("course", "PUBLIC_PREVIOUS_INDEX")]

      it "purges legacy chunks before rejecting invalid embedding configuration" $ \pool -> do
        cfg <- testConfig
        result <- refreshRagIndexWith (\_ -> do
          expectationFailure "Invalid configuration must not reach the provider"
          pure (Left "unexpected adapter call"))
          (cfg { openAiEmbedModel = "invalid-synthetic-model" }) pool
        result `shouldSatisfy` either (const True) (const False)
        indexRows pool `shouldReturn` [("course", "PUBLIC_PREVIOUS_INDEX")]

      it "refuses provider admission when legacy cleanup fails" $ \pool -> do
        publicCourse pool
        cfg <- testConfig
        previousRows <- indexRows pool
        called <- newIORef False
        runSqlPool (do
          rawExecute
            "CREATE FUNCTION reject_cleanup() RETURNS trigger LANGUAGE plpgsql AS $$ \
            \BEGIN RAISE EXCEPTION 'synthetic cleanup refusal'; END $$" []
          rawExecute
            "CREATE TRIGGER reject_cleanup BEFORE DELETE ON rag_chunk \
            \FOR EACH ROW EXECUTE FUNCTION reject_cleanup()" []
          ) pool
        refreshRagIndexWith (\inputs -> do
          writeIORef called True
          pure (Right (vectors inputs))) cfg pool
          `shouldThrow` (\err -> sqlState (err :: SqlError) == "P0001")
        readIORef called `shouldReturn` False
        indexRows pool `shouldReturn` previousRows

      it "preserves the previous public index and purges legacy chunks on provider refusal" $ \pool -> do
        publicCourse pool
        cfg <- testConfig
        previousRows <- filter ((== "course") . fst) <$> indexRows pool
        refreshRagIndexWith (\_ -> pure (Left "synthetic provider refusal")) cfg pool
          `shouldReturn` Left "synthetic provider refusal"
        indexRows pool `shouldReturn` previousRows

      it "preserves the previous public index on an incomplete embedding response" $ \pool -> do
        publicCourse pool
        cfg <- testConfig
        previousRows <- filter ((== "course") . fst) <$> indexRows pool
        refreshRagIndexWith (\_ -> pure (Right [])) cfg pool
          `shouldReturn` Left "Embedding response size mismatch"
        indexRows pool `shouldReturn` previousRows

      it "rolls back public replacement while retaining legacy cleanup on insertion failure" $ \pool -> do
        publicCourse pool
        cfg <- testConfig
        previousRows <- filter ((== "course") . fst) <$> indexRows pool
        runSqlPool (rawExecute
          "ALTER TABLE rag_chunk ADD CONSTRAINT synthetic_refusal CHECK (source_id <> 'public-course')"
          []) pool
        refreshRagIndexWith (pure . Right . vectors) cfg pool
          `shouldThrow` (\err -> sqlState (err :: SqlError) == "23514")
        indexRows pool `shouldReturn` previousRows

vectors :: [Text] -> [[Double]]
vectors = map (const (1 : replicate 1535 0))

testConfig :: IO AppConfig
testConfig = do
  cfg <- loadConfig
  pure cfg { openAiApiKey = Nothing, openAiEmbedModel = "text-embedding-3-small", ragChunkWords = 1000,
             ragChunkOverlap = 0 }

safeConnection :: String -> Bool
safeConnection value = case T.stripPrefix "postgresql://" (T.pack value) of
  Just rest -> case T.splitOn "/" rest of
    [authority, database] -> validAuthority authority && validDatabase database
    _ -> False
  Nothing -> False
  where
    validAuthority authority = authority == "postgres:postgres@postgres:5432"
      || case T.splitOn ":" authority of
        [host] -> host `elem` ["127.0.0.1", "localhost"]
        [host, port] -> host `elem` ["127.0.0.1", "localhost"]
          && not (T.null port) && T.all (\c -> c >= '0' && c <= '9') port
        _ -> False
    validDatabase name = "_test" `T.isSuffixOf` name
      && T.all (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
                      || (c >= '0' && c <= '9') || c == '_') name

-- A nonce schema survives pool connection replacement on SQL errors. Each
-- connection restores its search_path; no shared fixture rows are changed.
withFixture :: String -> ActionWith ConnectionPool -> IO ()
withFixture connection action = do
  suffix <- T.replace "-" "" . UUID.toText <$> UUID.nextRandom
  let schema = "rag_index_fixture_" <> T.unpack suffix
      query = fromString :: String -> PG.Query
  bracket (PG.connectPostgreSQL (BS.pack connection)) PG.close $ \admin ->
    bracket
      (PG.execute_ admin (query ("CREATE SCHEMA " <> schema)))
      (\_ -> void (PG.execute_ admin (query ("DROP SCHEMA " <> schema <> " CASCADE")))) $
      \_ -> bracket
        (runNoLoggingT (createPostgresqlPoolModified
          (\conn -> void (PG.execute_ conn (query ("SET search_path TO " <> schema <> ", public"))))
          (BS.pack connection) 1))
        destroyAllResources $ \pool -> do
          runSqlPool (do
            rawExecute "CREATE TABLE course (LIKE public.course INCLUDING DEFAULTS)" []
            rawExecute "CREATE TABLE rag_chunk (LIKE public.rag_chunk INCLUDING DEFAULTS)" []
            rawExecute
              "CREATE TABLE studio_brain_entry (LIKE public.studio_brain_entry INCLUDING DEFAULTS)" []
            rawExecute
              "INSERT INTO studio_brain_entry(title,body,active) \
              \VALUES ('PRIVATE_TITLE','PRIVATE_STUDIO_NOTE',true)" []
            rawExecute
              "INSERT INTO rag_chunk(source,source_id,chunk_index,content,metadata,embedding) \
              \VALUES ('studio_brain','legacy',0,'PRIVATE_LEGACY_INDEX','{}', \
              \('[' || repeat('0,',1535) || '1]')::vector)" []
            rawExecute
              "INSERT INTO rag_chunk(source,source_id,chunk_index,content,metadata,embedding) \
              \VALUES ('course','previous-public',0,'PUBLIC_PREVIOUS_INDEX','{}', \
              \('[' || repeat('0,',1535) || '1]')::vector)" []
            ) pool
          action pool

publicCourse :: ConnectionPool -> IO ()
publicCourse = runSqlPool (rawExecute
  "INSERT INTO course(slug,title,price_cents,currency,capacity,updated_at) \
  \VALUES ('public-course','PUBLIC_COURSE_SENTINEL',15000,'USD',12,now())" [])

indexRows :: ConnectionPool -> IO [(Text, Text)]
indexRows pool = do
  rows <- runSqlPool (rawSql "SELECT source,content FROM rag_chunk ORDER BY source,content" []) pool
  pure [(source, content) | (Single source, Single content) <- rows]
