{-# LANGUAGE OverloadedStrings #-}

module TDF.Social.SuggestionStoreSpec (spec) where

import           Control.Exception         (bracket_)
import           Control.Monad.Logger      (runNoLoggingT)
import           Control.Monad.Reader      (runReaderT)
import           Data.Aeson                (encode)
import qualified Data.Set                  as Set
import           Database.Persist.Sql      (runSqlPool, toSqlKey)
import           Database.Persist.Sqlite   (createSqlitePool)
import           Servant                   (ServerError, err503, throwError)
import           Servant.Server.Internal.Handler (runHandler)
import           System.Environment        (setEnv, unsetEnv)
import           Test.Hspec

import           TDF.Auth                  (AuthedUser (..))
import           TDF.DB                    (Env (..))
import           TDF.DTO                   (SuggestedFriendDTO (..))
import           TDF.Social.SuggestionRank (ViewerSuggestions (..))
import           TDF.Social.SuggestionStore

viewer :: AuthedUser
viewer = AuthedUser
  { auPartyId = toSqlKey 1
  , auRoles = []
  , auModules = Set.empty
  , auApiTokenId = Nothing
  , auSessionWitness = Nothing
  }

legacy :: [SuggestedFriendDTO]
legacy = [SuggestedFriendDTO 7 3 Nothing, SuggestedFriendDTO 5 1 Nothing]

withFlag :: Maybe String -> IO a -> IO a
withFlag value =
  bracket_ (maybe (unsetEnv suggestionFlagName) (setEnv suggestionFlagName) value)
           (unsetEnv suggestionFlagName)

run :: Env -> Either ServerError [SuggestedFriendDTO] -> IO (Either ServerError [SuggestedFriendDTO])
run env fallback =
  runHandler (runReaderT (suggestions viewer (either throwError pure fallback)) env)

unusedEnv :: Env
unusedEnv = Env
  { envPool = error "the pool is not used while the flag is off"
  , envConfig = error "the configuration is not used by friend suggestions"
  }

spec :: Spec
spec = describe "TDF.Social.SuggestionStore (COMMUNITY-FRIEND-SUGGESTION-001)" $ do
  describe "flag" $ do
    it "is off unless the variable is exactly true" $ do
      withFlag Nothing suggestionsEnabled `shouldReturn` False
      withFlag (Just "1") suggestionsEnabled `shouldReturn` False
      withFlag (Just "TRUE") suggestionsEnabled `shouldReturn` False
      withFlag (Just "true") suggestionsEnabled `shouldReturn` True

    it "off: the answer is the fallback and the database is not touched" $ do
      result <- withFlag Nothing (run unusedEnv (Right legacy))
      fmap encode result `shouldBe` Right (encode legacy)

    it "off: a fallback refusal is returned as it is" $ do
      result <- withFlag Nothing (run unusedEnv (Left err503))
      either Just (const Nothing) result `shouldBe` Just err503

  describe "flag on without a servable stored result" $ do
    it "falls back when the caller has no current session token" $ do
      pool <- runNoLoggingT (createSqlitePool ":memory:" 1)
      let env = unusedEnv { envPool = pool }
      result <- withFlag (Just "true") (run env (Right legacy))
      fmap encode result `shouldBe` Right (encode legacy)

    it "reads nothing on a database that is not PostgreSQL" $ do
      pool <- runNoLoggingT (createSqlitePool ":memory:" 1)
      runSqlPool (readStoredSuggestions 1) pool `shouldReturn` Nothing

  describe "response shape" $ do
    it "keeps the previous JSON for a fallback entry" $
      encode (SuggestedFriendDTO 7 3 Nothing) `shouldBe` "{\"sfMutualCount\":3,\"sfPartyId\":7}"

    it "marks a personalized entry and carries no count" $
      encode (SuggestedFriendDTO 7 0 (Just "network"))
        `shouldBe` "{\"sfMutualCount\":0,\"sfPartyId\":7,\"sfReason\":\"network\"}"

  describe "computeSuggestions" $ do
    let eligible = Set.fromList [1, 2, 3, 4, 5]
        follows = [(1, 2), (2, 3), (2, 4), (4, 5), (3, 1)]
        viewers = map vsViewer
        candidatesOf party results =
          concat [map fst (vsCandidates r) | r <- results, vsViewer r == party]

    it "computes every eligible viewer that did not opt out" $ do
      viewers (computeSuggestions eligible Set.empty [] follows) `shouldBe` [1, 2, 3, 4, 5]
      viewers (computeSuggestions eligible (Set.fromList [1, 4]) [] follows) `shouldBe` [2, 3, 5]

    it "never suggests the viewer, a followed account or a follower" $
      -- 1 follows 2 and is followed by 3; 4 and 5 remain.
      candidatesOf 1 (computeSuggestions eligible Set.empty [] follows) `shouldBe` [4, 5]

    it "keeps an opted-out account as a candidate for others" $
      candidatesOf 1 (computeSuggestions eligible (Set.singleton 4) [] follows) `shouldBe` [4, 5]

    it "a block in either direction removes the account and what it led to" $ do
      candidatesOf 1 (computeSuggestions eligible Set.empty [(1, 4)] follows) `shouldBe` []
      candidatesOf 1 (computeSuggestions eligible Set.empty [(4, 1)] follows) `shouldBe` []
      -- The block is the viewer's own: another viewer still reaches 4 and 5.
      candidatesOf 3 (computeSuggestions eligible Set.empty [(1, 4)] follows) `shouldBe` [4, 5]

    it "an ineligible account is neither a viewer, a candidate nor a path" $ do
      let results = computeSuggestions (Set.delete 4 eligible) Set.empty [] follows
      viewers results `shouldBe` [1, 2, 3, 5]
      candidatesOf 1 results `shouldBe` []
      all (Set.notMember 4 . vsSupport) results `shouldBe` True
