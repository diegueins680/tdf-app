{-# LANGUAGE OverloadedStrings #-}

-- | DIR-SEMANTIC-SEARCH-001: properties of the pure ranking reference,
-- the plan, the text sent to the provider and the derived SQL text.
module TDF.Directory.SemanticSearchSpec (spec) where

import           Data.Char       (isDigit)
import           Data.List       (permutations, sortBy)
import           Data.Maybe      (isJust)
import           Data.Ratio      ((%))
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Test.Hspec
import           Test.QuickCheck

import           TDF.Directory.SemanticSearch
import           TDF.Server.Directory (searchSql)

unit :: Gen Rational
unit = (% 10000) <$> choose (0, 10000)

-- | A cosine as a provider may return it, in or out of [-1, 1].
cosine :: Gen (Maybe Rational)
cosine = oneof [pure Nothing, Just . (% 1000) <$> choose (-1500, 1500)]

nonExactTitle :: Gen TitleMatch
nonExactTitle = oneof [pure TitlePrefix, pure TitleContains, TitleWeak <$> unit]

queriedTitle :: Gen TitleMatch
queriedTitle = oneof [pure TitleExact, nonExactTitle]

inputsWith :: Gen TitleMatch -> Gen RankInputs
inputsWith title =
  RankInputs
    <$> title
    <*> arbitrary
    <*> cosine
    <*> oneof [pure Nothing, Just . (% 10) <$> choose (0, 200000)]
    <*> unit
    <*> ((% 1) <$> choose (0, 400000000))
    <*> unit
    <*> unit
    <*> arbitrary

anyInputs :: Gen RankInputs
anyInputs = inputsWith (oneof [pure NoQuery, queriedTitle])

ranked :: Gen RankedDocument
ranked =
  RankedDocument
    <$> ((% 100) <$> choose (0, 3))
    <*> ((% 1) <$> choose (0, 2))
    <*> elements ["classified", "profile"]
    <*> (T.pack . show <$> choose (1 :: Int, 4))

distinctKeys :: [RankedDocument] -> [RankedDocument]
distinctKeys = go []
  where
    go _ [] = []
    go seen (d : rest)
      | (rdKind d, rdId d) `elem` seen = go seen rest
      | otherwise = d : go ((rdKind d, rdId d) : seen) rest

contactText :: Gen Text
contactText =
  T.unwords <$> listOf (elements
    [ "bajista", "con", "estudio", "en", "quito", "ana@example.com", "+593 99 123 4567"
    , "0991234567", "(02) 245-6789", "desde", "2019", "a", "2024", "100", "wa.me/593991234567"
    , "contacto:", "x@y", "24", "pistas", "8", "canales" ])

digitRuns :: Text -> [Int]
digitRuns = map (T.length . T.filter isDigit) . filter (T.any isDigit) . T.groupBy sameClass
  where
    sameClass a b = phoneChar a == phoneChar b
    phoneChar c = isDigit c || c == ' ' || c `elem` ("+-().·/" :: String)

spec :: Spec
spec = describe "directory semantic search (DIR-SEMANTIC-SEARCH-001)" $ do
  describe "ranking reference" $ do
    it "P1: an exact title match ranks above every other match, whatever the similarities" $
      forAll ((,) <$> inputsWith (pure TitleExact) <*> inputsWith nonExactTitle) $ \(exact, other) ->
        rankScore exact > rankScore other

    it "keeps the prefix and contains tiers apart as well" $
      forAll ((,,) <$> inputsWith (pure TitlePrefix) <*> inputsWith (pure TitleContains)
                <*> inputsWith (TitleWeak <$> unit)) $ \(prefix, contains, weak) ->
        rankScore prefix > rankScore contains && rankScore contains > rankScore weak

    it "P3: without a usable embedding pair the semantic term is the alias term" $
      forAll ((,) <$> queriedTitle <*> arbitrary) $ \(title, alias) ->
        semanticScore title alias Nothing == (if alias then 1 else 0)

    it "P3: a missing embedding and a similarity of zero score alike" $
      forAll anyInputs $ \inputs ->
        organicScore inputs {riSimilarity = Nothing} == organicScore inputs {riSimilarity = Just 0}

    it "the semantic term is max(alias, clamped similarity) and stays in [0, 1]" $
      forAll ((,,) <$> queriedTitle <*> arbitrary <*> cosine) $ \(title, alias, similarity) ->
        let value = semanticScore title alias similarity
        in value >= 0 && value <= 1
             && value >= (if alias then 1 else 0)
             && (alias || value == 0 || fmap (> 0) similarity == Just True)

    it "browsing has no semantic term" $
      forAll ((,) <$> arbitrary <*> cosine) $ \(alias, similarity) ->
        semanticScore NoQuery alias similarity == 0

    it "a larger similarity never lowers the score" $
      forAll ((,,) <$> anyInputs <*> unit <*> unit) $ \(inputs, a, b) ->
        organicScore inputs {riSimilarity = Just (min a b)}
          <= organicScore inputs {riSimilarity = Just (max a b)}

    it "P4: the public score stays in [0, 1]" $
      forAll anyInputs $ \inputs ->
        organicScore inputs >= 0 && organicScore inputs <= 1

    it "has the documented weights and boost" $ do
      [weightText, weightSemantic, weightProximity, weightQuality, weightActivity, weightAvailability, weightReputation]
        `shouldBe` [40 % 100, 15 % 100, 15 % 100, 10 % 100, 8 % 100, 5 % 100, 7 % 100]
      (weightDivisor, imageBoost) `shouldBe` (5, 1 % 10)
      organicScore (RankInputs TitleExact True (Just 1) (Just 0) 1 0 1 1 True) `shouldBe` 92 % 100

  describe "ordering" $ do
    it "P5: two documents compare equal only when they are the same document" $
      forAll ((,) <$> ranked <*> ranked) $ \(a, b) ->
        (compareRanked a b == EQ) == (a == b)

    it "P5: the order is antisymmetric and transitive" $
      forAll ((,,) <$> ranked <*> ranked <*> ranked) $ \(a, b, c) ->
        compareRanked a b == compare EQ (compareRanked b a)
          && (not (compareRanked a b /= GT && compareRanked b c /= GT) || compareRanked a c /= GT)

    it "P5: a page order does not depend on the order rows arrive in" $
      forAll (distinctKeys . take 5 <$> listOf ranked) $ \documents ->
        all (== sortBy compareRanked documents) (map (sortBy compareRanked) (permutations documents))

  describe "plan" $ do
    let embedding = QueryEmbedding "openai" "text-embedding-3-small" "[1,0]"
    it "P3: is lexical with the flag off or without a query embedding" $ do
      searchPlan False (Just embedding) `shouldBe` LexicalPlan
      searchPlan False Nothing `shouldBe` LexicalPlan
      searchPlan True Nothing `shouldBe` LexicalPlan
      searchPlan True (Just embedding) `shouldBe` SemanticPlan embedding

    it "P3: a lexical plan executes today's statement with no added parameter" $
      mapM_ (\sponsored -> plannedSearch LexicalPlan (searchSql sponsored) `shouldBe` (searchSql sponsored, []))
        [False, True]

    it "a semantic plan adds the vector, the provider and the model, in that order" $
      mapM_ (\sponsored -> do
          let (statement, params) = plannedSearch (SemanticPlan embedding) (searchSql sponsored)
          params `shouldBe` ["[1,0]", "openai", "text-embedding-3-small"]
          Just statement `shouldBe` semanticSearchSql (searchSql sponsored))
        [False, True]

    it "keeps today's statement when the semantic one cannot be derived" $
      plannedSearch (SemanticPlan embedding) "SELECT 1" `shouldBe` ("SELECT 1", [])

    it "embeds only with the flag on and a query of three characters or more" $ do
      queryEligible False "bajista" `shouldBe` False
      queryEligible True "ba" `shouldBe` False
      queryEligible True "  ba  " `shouldBe` False
      queryEligible True "bajo" `shouldBe` True
      queryEligible True "0991234567" `shouldBe` False

  describe "derived statement" $ do
    it "finds each anchor exactly once in today's statement" $
      mapM_ (\sponsored ->
          map (\anchor -> T.count anchor (searchSql sponsored))
            [semanticInputAnchor, semanticAliasOpenAnchor, semanticAliasCloseAnchor]
            `shouldBe` [1, 1, 1])
        [False, True]

    it "changes the input row and the alias term and nothing else" $
      mapM_ (\sponsored -> do
          let lexical = searchSql sponsored
          semantic <- maybe (expectationFailure "no semantic statement" >> pure "") pure (semanticSearchSql lexical)
          let restored =
                T.replace ("THEN 1 ELSE 0 END," <> similaritySql <> ") semantic_score ") semanticAliasCloseAnchor
                  . T.replace "text_score,greatest(CASE WHEN input.q=''" "text_score,CASE WHEN input.q=''"
                  . T.replace ",?::vector q_embedding,?::text q_provider,?::text q_model)" ")"
                  $ semantic
          restored `shouldBe` lexical
          T.count "?" semantic `shouldBe` T.count "?" lexical + 3
          T.count similaritySql semantic `shouldBe` 1)
        [False, True]

    it "reads the weights the reference uses" $ do
      let lexical = searchSql False
      lexical `shouldSatisfy` T.isInfixOf
        "round(((.40*text_score+.15*semantic_score+.15*CASE WHEN distance_km IS NULL THEN .35 ELSE 1/(1+distance_km/25) END+.10*profile_completeness+.08*least(1,1/(1+extract(epoch from (now()-source_updated_at))/2592000))+.05*availability_score+.07*reputation_score)/5)::numeric,6) organic_score"
      lexical `shouldSatisfy` T.isInfixOf "organic_score+.10*image_score rank_score"
      lexical `shouldSatisfy` T.isInfixOf
        "ORDER BY rank_score DESC,source_updated_at DESC,entity_kind,entity_id LIMIT ?"

    it "clamps the similarity, requires the current text and the same space, and joins by key" $ do
      similaritySql `shouldSatisfy` T.isInfixOf "least(1,greatest(0,"
      similaritySql `shouldSatisfy` T.isInfixOf "'NaN'::float8"
      similaritySql `shouldSatisfy` T.isInfixOf "emb.entity_kind=document.entity_kind AND emb.entity_id=document.entity_id"
      similaritySql `shouldSatisfy` T.isInfixOf "emb.provider=input.q_provider AND emb.model=input.q_model"
      similaritySql `shouldSatisfy` T.isInfixOf ("emb.content_hash=" <> documentHashSql)
      semanticSearchSql (searchSql False) `shouldSatisfy` isJust

    it "claims, writes and purges under the eligibility predicate of search" $ do
      mapM_ (`shouldSatisfy` T.isInfixOf eligibleDocumentFromSql)
        [claimDocumentsSql, writeDocumentEmbeddingSql, purgeDocumentEmbeddingsSql]
      mapM_ (\fragment -> eligibleDocumentFromSql `shouldSatisfy` T.isInfixOf fragment)
        [ "document.entity_kind IN ('profile','classified')"
        , "document.source_status='published' AND document.visibility='public' AND document.moderation_status='allowed'"
        , "profile.profile_status='published' AND profile.visibility='public' AND profile.moderation_status='allowed' AND profile.canonical_profile_id IS NULL"
        ]
      searchSql False `shouldSatisfy` T.isInfixOf
        "(profile.profile_status='published' AND profile.visibility='public' AND profile.moderation_status='allowed' AND profile.canonical_profile_id IS NULL)"
      writeDocumentEmbeddingSql `shouldSatisfy` T.isInfixOf "FOR SHARE OF document"
      -- The sweep pages through the claim so unsendable documents cannot hold it up.
      claimDocumentsSql `shouldSatisfy` T.isSuffixOf "LIMIT ? OFFSET ?"
      writeDocumentEmbeddingSql `shouldSatisfy` T.isInfixOf (documentHashSql <> "=?")

  describe "text sent to the provider" $ do
    it "removes e-mail addresses and telephone numbers" $ do
      redactContactDetails "Bajista. Escribe a ana@example.com o al +593 99 123 4567, Quito"
        `shouldBe` "Bajista. Escribe a o al , Quito"
      redactContactDetails "24 pistas, 8 canales, desde 2019" `shouldBe` "24 pistas, 8 canales, desde 2019"

    it "leaves no address and no run of seven digits, and is idempotent" $
      forAll contactText $ \text ->
        let redacted = redactContactDetails text
        in not (T.any (== '@') redacted)
             && all (< 7) (digitRuns redacted)
             && redactContactDetails redacted == redacted

    it "caps a document at the documented length" $
      forAll (choose (0, 3 * embeddedTextCap)) $ \n ->
        T.length (embeddingInput (T.replicate n "a")) == min n embeddedTextCap

    it "keys the query cache by provider, model and normalised query" $ do
      queryCacheKeyInput "openai" "m" "  Bajista   de JAZZ " `shouldBe` "openai\nm\nbajista de jazz"
      queryCacheKeyInput "openai" "m" "a" `shouldNotBe` queryCacheKeyInput "openai" "n" "a"

  describe "vectors" $
    it "accepts only finite, non-zero vectors of the expected dimension" $ do
      usableVector 3 [0.1, 0, -0.2] `shouldBe` True
      usableVector 3 [0, 0, 0] `shouldBe` False
      usableVector 3 [0.1, 0.2] `shouldBe` False
      usableVector 2 [0.1, 0 / 0] `shouldBe` False
      usableVector 2 [1 / 0, 0.1] `shouldBe` False
      vectorLiteral [0.5, -1, 2.0e-2] `shouldBe` "[0.5,-1.0,2.0e-2]"
