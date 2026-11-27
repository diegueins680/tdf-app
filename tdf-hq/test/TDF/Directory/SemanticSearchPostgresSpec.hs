{-# LANGUAGE OverloadedStrings #-}

-- | DIR-SEMANTIC-SEARCH-001 on PostgreSQL with pgvector: the migration,
-- the sweep, the guarded write, the trigger, the search statements and the
-- query cache, with the provider replaced by a recording function.
--
-- Fixture rows are classified search documents with a nonce in every key and
-- are removed afterwards. Run through scripts/test-directory-semantic-search.sh.
module TDF.Directory.SemanticSearchPostgresSpec (spec) where

import           Control.Exception            (bracket)
import           Control.Monad                (forM_, void)
import           Control.Monad.Logger         (runNoLoggingT)
import qualified Data.Aeson                   as A
import qualified Data.Aeson.Key               as Key
import qualified Data.Aeson.KeyMap            as KeyMap
import qualified Data.ByteString.Char8        as BS
import           Data.Char                    (chr, digitToInt, isDigit, ord)
import           Data.IORef                   (IORef, modifyIORef', newIORef, readIORef)
import           Data.Int                     (Int64)
import           Data.Maybe                   (mapMaybe)
import           Data.Pool                    (destroyAllResources)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.UUID                    as UUID
import qualified Data.UUID.V4                 as UUID
import           Database.Persist.Postgresql  (createPostgresqlPool)
import           Database.Persist.Sql         (ConnectionPool, PersistValue (..), Single (..),
                                               SqlPersistT, rawExecute, rawExecuteCount, rawSql,
                                               runSqlPool)
import           System.Environment           (lookupEnv)
import           Test.Hspec

import qualified TDF.CMS.Models               as CMS
import           TDF.Taxonomy.Filter (emptyTaxonomyFilter, taxonomyInputParams)
import           TDF.Directory.SemanticIndex
import           TDF.Directory.SemanticSearch
import qualified TDF.Models.SocialEventsModels as Social
import           TDF.Server.Directory         (searchSql)
import           TDF.Test.DisposablePostgres  (isDisposablePostgresUrl)

model :: Text
model = "text-embedding-3-small"

-- | A unit vector along one axis, and the diagonal of two axes.
axis :: Int -> [Double]
axis i = [if j == i then 1 else 0 | j <- [0 .. embeddingDimension - 1]]

diagonal :: [Double]
diagonal = [if j < 2 then 1 else 0 | j <- [0 .. embeddingDimension - 1]]

data Fixture = Fixture
  { fxPool  :: ConnectionPool
  , fxNonce :: Text
  }

docId :: Fixture -> Text -> Text
docId fixture name = "zzsem-" <> fxNonce fixture <> "-" <> name

-- | The query every fixture document matches by text.
queryText :: Fixture -> Text
queryText fixture = "zzsem" <> fxNonce fixture <> " uno"

run :: Fixture -> SqlPersistT IO a -> IO a
run fixture action = runSqlPool action (fxPool fixture)

-- | Insert a classified search document. @status@ is its moderation status.
insertDocument :: Fixture -> Text -> Text -> Text -> Text -> IO ()
insertDocument fixture name title summary status =
  run fixture $ rawExecute
    "INSERT INTO directory_search_document \
    \(entity_kind,entity_id,slug,title,summary,search_text,search_vector,source_status,visibility,moderation_status,source_updated_at) \
    \SELECT 'classified',?,?,?,?,content,to_tsvector('simple',content),'published','public',?,'2026-01-01T00:00:00Z' \
    \FROM (SELECT directory_normalize_text(?||' '||?) content) text"
    [ PersistText (docId fixture name), PersistText (docId fixture name), PersistText title
    , PersistText summary, PersistText status, PersistText title, PersistText summary ]

-- | Store an embedding of the current text of a document, as the sweep would.
storeEmbedding :: Fixture -> Text -> [Double] -> IO ()
storeEmbedding fixture name vector =
  run fixture $ rawExecute
    ("INSERT INTO directory_search_embedding (entity_kind,entity_id,content_hash,provider,model,embedding) \
     \SELECT document.entity_kind,document.entity_id," <> documentHashSql <> ",?,?,?::vector \
     \FROM directory_search_document document WHERE document.entity_kind='classified' AND document.entity_id=?")
    [ PersistText embeddingProviderName, PersistText model, PersistText (vectorLiteral vector)
    , PersistText (docId fixture name) ]

-- | Change a document without firing the trigger, to stage what only a
-- missing trigger could leave behind.
withoutTriggers :: Fixture -> Text -> [PersistValue] -> IO ()
withoutTriggers fixture statement params = run fixture $ do
  rawExecute "SET LOCAL session_replication_role = replica" []
  rawExecute statement params

embeddingCount :: Fixture -> Text -> IO Int
embeddingCount fixture name = do
  rows <- run fixture $ rawSql
    "SELECT count(*) FROM directory_search_embedding WHERE entity_kind='classified' AND entity_id=?"
    [PersistText (docId fixture name)]
  pure (sum [fromIntegral (n :: Int64) | Single n <- rows])

recording :: IORef [[Text]] -> ([Text] -> Either Text [[Double]]) -> Embedder
recording calls answer inputs = do
  modifyIORef' calls (<> [inputs])
  pure (answer inputs)

sweep :: Fixture -> Embedder -> IO SweepStats
sweep fixture embed = sweepDirectoryEmbeddingsWith embed model 100000 (fxPool fixture)

data Hit = Hit
  { hitId       :: Text
  , hitScore    :: Double
  , hitSemantic :: Double
  , hitCursor   :: Text
  } deriving (Eq, Show)

toHit :: A.Value -> Maybe Hit
toHit (A.Object row) = do
  A.String ident <- look "id" row
  A.Number score <- look "score" row
  A.Object breakdown <- look "scoreBreakdown" row
  A.Number semantic <- look "taxonomy" breakdown
  A.String cursor <- look "cursor" row
  pure (Hit ident (realToFrac score) (realToFrac semantic) cursor)
  where
    look key = KeyMap.lookup (Key.fromText key)
toHit _ = Nothing

-- | Run a search statement over classifieds for the fixture query. The extra
-- parameters are those of a semantic plan (none for today's statement).
search :: Fixture -> SearchPlan -> Maybe Text -> Int -> SqlPersistT IO [A.Value]
search fixture plan cursor limit = do
  let (statement, extra) = plannedSearch plan (searchSql False)
      common =
        [ PersistText (queryText fixture), PersistText "classified" ]
          <> replicate 14 PersistNull <> taxonomyInputParams emptyTaxonomyFilter
  rows <- rawSql statement
    ( common <> map PersistText extra
        <> [ PersistText Social.externalEventRefSuppressedStatus
           , maybe PersistNull PersistText cursor, PersistInt64 (fromIntegral limit) ] )
  pure [CMS.unAesonValue value | Single value <- rows]

ours :: Fixture -> [A.Value] -> [Hit]
ours fixture = filter (T.isPrefixOf ("zzsem-" <> fxNonce fixture) . hitId) . mapMaybe toHit

semanticPlan :: [Double] -> SearchPlan
semanticPlan vector = SemanticPlan (QueryEmbedding embeddingProviderName model (vectorLiteral vector))

withFixture :: String -> ActionWith Fixture -> IO ()
withFixture connection action = do
  -- Letters only: a run of digits in the query would be redacted as a number.
  nonce <- T.map (\c -> if isDigit c then chr (ord 'g' + digitToInt c) else c)
             . T.take 12 . T.replace "-" "" . UUID.toText <$> UUID.nextRandom
  bracket (runNoLoggingT (createPostgresqlPool (BS.pack connection) 1)) destroyAllResources $ \pool -> do
    let fixture = Fixture pool nonce
        cleanup = run fixture $ do
          rawExecute "DELETE FROM directory_search_document WHERE entity_kind='classified' AND entity_id LIKE ?"
            [PersistText ("zzsem-" <> nonce <> "-%")]
          rawExecute "DELETE FROM directory_query_embedding" []
          rawExecute "DELETE FROM directory_query_embedding_budget" []
    bracket (pure ()) (const cleanup) (const (action fixture))

spec :: Spec
spec = describe "directory-semantic-search-postgresql" $ do
  ci <- runIO (lookupEnv "CI")
  configured <- runIO (lookupEnv "TDF_DIRECTORY_SEMANTIC_DATABASE_URL")
  overrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  case configured of
    Nothing -> it "requires the isolated migrated PostgreSQL runner" $
      pendingWith "Run scripts/test-directory-semantic-search.sh"
    Just _ | any (maybe False (not . null)) overrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset inherited libpq routing overrides"
    Just connection | not (isDisposablePostgresUrl (ci == Just "true") connection) ->
      it "refuses a non-disposable database" $
        expectationFailure "Use the isolated integration runner"
    Just connection -> around (withFixture connection) $ do
      it "embeds public documents only, without contact details, and once" $ \fixture -> do
        insertDocument fixture "public" (queryText fixture <> " bajista")
          "Sesiones de bajo. Escribe a ana@example.com o al +593 99 123 4567" "allowed"
        insertDocument fixture "hidden" (queryText fixture <> " HIDDEN_SENTINEL") "texto reservado" "blocked"
        calls <- newIORef []
        stats <- sweep fixture (recording calls (Right . map (const (axis 0))))
        sweepWritten stats `shouldSatisfy` (>= 1)
        sent <- T.unwords . concat <$> readIORef calls
        sent `shouldSatisfy` T.isInfixOf "bajista"
        sent `shouldSatisfy` T.isInfixOf "Sesiones de bajo"
        sent `shouldSatisfy` (not . T.isInfixOf "HIDDEN_SENTINEL")
        sent `shouldSatisfy` (not . T.isInfixOf "texto reservado")
        sent `shouldSatisfy` (not . T.any (== '@'))
        sent `shouldSatisfy` (not . T.isInfixOf "4567")
        embeddingCount fixture "public" `shouldReturn` 1
        embeddingCount fixture "hidden" `shouldReturn` 0
        again <- newIORef []
        void (sweep fixture (recording again (Right . map (const (axis 0)))))
        resent <- T.unwords . concat <$> readIORef again
        resent `shouldSatisfy` (not . T.isInfixOf "bajista")
        embeddingCount fixture "public" `shouldReturn` 1

      it "writes nothing when the provider fails or answers with unusable vectors" $ \fixture -> do
        insertDocument fixture "public" (queryText fixture <> " bajista") "sesiones de bajo" "allowed"
        forM_
          [ const (Left "provider down")
          , const (Right [])
          , Right . map (const [0.5, 0.5])
          , Right . map (const (replicate embeddingDimension 0))
          , Right . map (const (0 / 0 : drop 1 (axis 0)))
          ] $ \answer -> do
            calls <- newIORef []
            stats <- sweep fixture (recording calls answer)
            sweepWritten stats `shouldBe` 0
            sweepSkipped stats `shouldSatisfy` (>= 1)
            embeddingCount fixture "public" `shouldReturn` 0

      it "deletes the embedding in the statement that hides or rewrites a document" $ \fixture -> do
        forM_ ["hide", "rewrite", "keep"] $ \name -> do
          insertDocument fixture name (queryText fixture <> " " <> name) "sesiones de bajo" "allowed"
          storeEmbedding fixture name (axis 0)
        run fixture $ do
          rawExecute "UPDATE directory_search_document SET moderation_status='blocked' WHERE entity_id=?"
            [PersistText (docId fixture "hide")]
          rawExecute "UPDATE directory_search_document SET search_text=search_text||' nuevo' WHERE entity_id=?"
            [PersistText (docId fixture "rewrite")]
          rawExecute "UPDATE directory_search_document SET availability_score=1 WHERE entity_id=?"
            [PersistText (docId fixture "keep")]
        mapM (embeddingCount fixture) ["hide", "rewrite", "keep"] `shouldReturn` [0, 0, 1]

      it "deletes the embedding on an edit the search text does not show, a new source version, and an expiry" $ \fixture -> do
        forM_ ["summary", "version", "expire", "keep"] $ \name -> do
          insertDocument fixture name (queryText fixture <> " " <> name) "\12462\12479\12540 sesiones" "allowed"
          storeEmbedding fixture name (axis 0)
        before <- run fixture $ rawSql
          "SELECT search_text FROM directory_search_document WHERE entity_id=?" [PersistText (docId fixture "summary")]
        run fixture $ do
          -- Normalisation discards the non-Latin words, so search_text stays.
          rawExecute "UPDATE directory_search_document SET summary=? WHERE entity_id=?"
            [PersistText "\12500\12450\12494 sesiones", PersistText (docId fixture "summary")]
          rawExecute "UPDATE directory_search_document SET source_version=source_version+1 WHERE entity_id=?"
            [PersistText (docId fixture "version")]
          rawExecute "UPDATE directory_search_document SET expires_at=now()-interval '1 minute' WHERE entity_id=?"
            [PersistText (docId fixture "expire")]
          rawExecute "UPDATE directory_search_document SET expires_at=now()+interval '1 day' WHERE entity_id=?"
            [PersistText (docId fixture "keep")]
        after <- run fixture $ rawSql
          "SELECT search_text FROM directory_search_document WHERE entity_id=?" [PersistText (docId fixture "summary")]
        (after :: [Single Text]) `shouldBe` before
        mapM (embeddingCount fixture) ["summary", "version", "expire", "keep"] `shouldReturn` [0, 0, 0, 1]

      it "reads past the oldest documents when their text redacts to nothing" $ \fixture -> do
        -- Only contact details: nothing is left to send, so it never gets an embedding.
        insertDocument fixture "blank" "ana@example.com" "+593 99 123 4567" "allowed"
        insertDocument fixture "later" (queryText fixture <> " guitarrista") "clases de guitarra" "allowed"
        withoutTriggers fixture "UPDATE directory_search_document SET source_updated_at='1990-01-01T00:00:00Z' WHERE entity_id=?"
          [PersistText (docId fixture "blank")]
        calls <- newIORef []
        -- A batch of one used to claim the blank document on every sweep.
        stats <- sweepDirectoryEmbeddingsWith (recording calls (Right . map (const (axis 0)))) model 1 (fxPool fixture)
        sweepWritten stats `shouldBe` 1
        sweepSkipped stats `shouldSatisfy` (>= 1)
        sent <- concat <$> readIORef calls
        length sent `shouldBe` 1
        embeddingCount fixture "blank" `shouldReturn` 0

      it "neither embeds nor keeps a document that has expired or is not yet effective" $ \fixture -> do
        forM_ ["expired", "future", "live"] $ \name ->
          insertDocument fixture name (queryText fixture <> " " <> name) "sesiones de bajo" "allowed"
        forM_ ["expired", "future"] $ \name -> storeEmbedding fixture name (axis 0)
        withoutTriggers fixture "UPDATE directory_search_document SET expires_at=now()-interval '1 minute' WHERE entity_id=?"
          [PersistText (docId fixture "expired")]
        withoutTriggers fixture "UPDATE directory_search_document SET effective_at=now()+interval '1 day' WHERE entity_id=?"
          [PersistText (docId fixture "future")]
        calls <- newIORef []
        stats <- sweep fixture (recording calls (\inputs -> Right (map (const (axis 0)) inputs)))
        sweepPurged stats `shouldSatisfy` (>= 2)
        sent <- concat <$> readIORef calls
        filter (\text -> any (`T.isInfixOf` text) ["expired", "future"]) sent `shouldBe` []
        mapM (embeddingCount fixture) ["expired", "future", "live"] `shouldReturn` [0, 0, 1]

      it "refuses a write for a document hidden or rewritten after the claim" $ \fixture -> do
        forM_ ["hide", "rewrite", "same"] $ \name ->
          insertDocument fixture name (queryText fixture <> " " <> name) "sesiones de bajo" "allowed"
        claimed <- run fixture $ rawSql
          ("SELECT document.entity_id," <> documentHashSql <> " FROM directory_search_document document WHERE document.entity_id LIKE ?")
          [PersistText (docId fixture "%")]
        run fixture $ do
          rawExecute "UPDATE directory_search_document SET visibility='private' WHERE entity_id=?"
            [PersistText (docId fixture "hide")]
          rawExecute "UPDATE directory_search_document SET search_text=search_text||' nuevo' WHERE entity_id=?"
            [PersistText (docId fixture "rewrite")]
        written <- mapM (\(Single ident, Single digest) -> run fixture $ rawExecuteCount writeDocumentEmbeddingSql
            [ PersistText digest, PersistText embeddingProviderName, PersistText model
            , PersistText (vectorLiteral (axis 0)), PersistText "classified", PersistText (ident :: Text)
            , PersistText (digest :: Text) ]) claimed
        sum written `shouldBe` 1
        mapM (embeddingCount fixture) ["hide", "rewrite", "same"] `shouldReturn` [0, 0, 1]

      it "purges an embedding the trigger did not remove" $ \fixture -> do
        insertDocument fixture "hidden" (queryText fixture <> " oculto") "sesiones de bajo" "allowed"
        storeEmbedding fixture "hidden" (axis 0)
        withoutTriggers fixture "UPDATE directory_search_document SET source_status='paused' WHERE entity_id=?"
          [PersistText (docId fixture "hidden")]
        embeddingCount fixture "hidden" `shouldReturn` 1
        calls <- newIORef []
        stats <- sweep fixture (recording calls (const (Left "provider down")))
        sweepPurged stats `shouldSatisfy` (>= 1)
        embeddingCount fixture "hidden" `shouldReturn` 0

      it "P3: without a usable embedding pair the semantic statement returns today's rows" $ \fixture -> do
        forM_ [("a", " bajista"), ("b", " baterista"), ("c", "")] $ \(name, suffix) ->
          insertDocument fixture name (queryText fixture <> suffix) "sesiones de bajo" "allowed"
        let foreign' = SemanticPlan (QueryEmbedding "other" model (vectorLiteral (axis 0)))
        -- No document embedding at all.
        (lexical, semantic) <- run fixture $
          (,) <$> search fixture LexicalPlan Nothing 50 <*> search fixture (semanticPlan (axis 0)) Nothing 50
        ours fixture lexical `shouldSatisfy` ((== 3) . length)
        semantic `shouldBe` lexical
        -- Embeddings of another space, of other text, and a zero vector.
        storeEmbedding fixture "a" (axis 0)
        storeEmbedding fixture "b" (axis 0)
        storeEmbedding fixture "c" (replicate embeddingDimension 0)
        withoutTriggers fixture
          "UPDATE directory_search_embedding SET content_hash=repeat('0',64) WHERE entity_id=?"
          [PersistText (docId fixture "b")]
        (lexical', otherSpace) <- run fixture $
          (,) <$> search fixture LexicalPlan Nothing 50 <*> search fixture foreign' Nothing 50
        otherSpace `shouldBe` lexical'
        sameSpace <- run fixture (search fixture (semanticPlan (axis 0)) Nothing 50)
        [ (T.takeWhileEnd (/= '-') (hitId hit), hitSemantic hit) | hit <- ours fixture sameSpace ]
          `shouldMatchList` [("a", 1), ("b", 0), ("c", 0)]

      it "P1: similarity reorders within a title tier and never lifts a match over an exact title" $ \fixture -> do
        insertDocument fixture "exact" (queryText fixture) "sesiones de bajo" "allowed"
        insertDocument fixture "p1" (queryText fixture <> " bajista") "sesiones de bajo" "allowed"
        insertDocument fixture "p2" (queryText fixture <> " bajista") "sesiones de bajo" "allowed"
        insertDocument fixture "p3" (queryText fixture <> " bajista") "sesiones de bajo" "allowed"
        insertDocument fixture "hidden" (queryText fixture <> " bajista") "sesiones de bajo" "allowed"
        storeEmbedding fixture "p2" diagonal
        storeEmbedding fixture "p3" (axis 0)
        storeEmbedding fixture "hidden" (axis 0)
        withoutTriggers fixture "UPDATE directory_search_document SET moderation_status='blocked' WHERE entity_id=?"
          [PersistText (docId fixture "hidden")]
        lexical <- run fixture (search fixture LexicalPlan Nothing 50)
        semantic <- run fixture (search fixture (semanticPlan (axis 0)) Nothing 50)
        let names = map (T.takeWhileEnd (/= '-') . hitId) . ours fixture
        names lexical `shouldBe` ["exact", "p1", "p2", "p3"]
        names semantic `shouldBe` ["exact", "p3", "p2", "p1"]
        map hitSemantic (ours fixture semantic) `shouldBe` [0, 1, 0.7071, 0]
        map hitScore (ours fixture semantic) `shouldSatisfy` all (\score -> score >= 0 && score <= 1)

      it "P5: pages of the semantic statement neither repeat nor skip a document" $ \fixture -> do
        forM_ (zip ["a", "b", "c", "d", "e"] [axis 0, diagonal, axis 1, axis 0, diagonal]) $ \(name, vector) -> do
          insertDocument fixture name (queryText fixture <> " bajista") "sesiones de bajo" "allowed"
          storeEmbedding fixture name vector
        let plan = semanticPlan (axis 0)
            pages cursor seen = do
              page <- ours fixture <$> run fixture (search fixture plan cursor 2)
              case page of
                [] -> pure seen
                _  -> pages (Just (hitCursor (last page))) (seen <> map hitId page)
        whole <- map hitId . ours fixture <$> run fixture (search fixture plan Nothing 50)
        paged <- pages Nothing []
        length whole `shouldBe` 5
        paged `shouldBe` whole

      it "caches a query vector by hash, spends the budget on a miss and falls back on failure" $ \fixture -> do
        let query = queryText fixture <> " Bajista"
            resolve calls answer flag =
              resolveQueryEmbeddingWith (recording calls answer) flag (Just model) (fxPool fixture) query
            good = const (Right [axis 0])
        calls <- newIORef []
        resolve calls good False `shouldReturn` Nothing
        resolveQueryEmbeddingWith (recording calls good) True Nothing (fxPool fixture) query `shouldReturn` Nothing
        readIORef calls `shouldReturn` []
        forM_ [const (Left "provider down"), const (Right []), const (Right [[0.5]]), const (Right [replicate embeddingDimension 0])] $ \bad ->
          resolve calls bad True `shouldReturn` Nothing
        stored <- run fixture (rawSql "SELECT count(*) FROM directory_query_embedding" [])
        stored `shouldBe` [Single (0 :: Int64)]
        first <- resolve calls good True
        fmap qeModel first `shouldBe` Just model
        before <- length <$> readIORef calls
        second <- resolve calls (const (Left "provider down")) True
        fmap qeProvider second `shouldBe` Just embeddingProviderName
        (length <$> readIORef calls) `shouldReturn` before
        readIORef calls >>= \sent -> concat sent `shouldSatisfy` all (== T.toLower query)
        keys <- run fixture (rawSql "SELECT query_hash FROM directory_query_embedding" [])
        keys `shouldBe` [Single (queryCacheKey embeddingProviderName model query)]
        columns <- run fixture $ rawSql
          "SELECT column_name::text FROM information_schema.columns WHERE table_name='directory_query_embedding' ORDER BY 1" []
        columns `shouldBe` map Single ["created_at", "embedding", "model", "provider", "query_hash" :: Text]
        -- A used-up minute: no provider call, no vector.
        run fixture $ rawExecute
          "INSERT INTO directory_query_embedding_budget (window_started_at,calls) \
          \SELECT date_trunc('minute',now())+step*interval '1 minute',? FROM generate_series(0,1) step \
          \ON CONFLICT (window_started_at) DO UPDATE SET calls=EXCLUDED.calls"
          [PersistInt64 (fromIntegral queryCallsPerMinute)]
        resolveQueryEmbeddingWith (recording calls good) True (Just model) (fxPool fixture) (query <> " otra")
          `shouldReturn` Nothing
        (length <$> readIORef calls) `shouldReturn` before

      it "installs the guard trigger and vector columns of the reviewed migration" $ \fixture -> do
        rows <- run fixture $ rawSql
          "SELECT (SELECT count(*) FROM pg_trigger WHERE tgrelid='public.directory_search_document'::regclass \
          \AND tgname='directory_search_embedding_guard_trigger' AND NOT tgisinternal AND tgenabled<>'D'), \
          \(SELECT format_type(atttypid,atttypmod) FROM pg_attribute WHERE attrelid='public.directory_search_embedding'::regclass AND attname='embedding')"
          []
        rows `shouldBe` [(Single (1 :: Int64), Single ("vector(1536)" :: Text))]
        void (pure fixture)
