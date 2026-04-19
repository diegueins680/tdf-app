{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.ServerProposalsSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import Database.Persist.Sql (SqlPersistT, rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Sqlite (createSqlitePool)
import Servant (ServerError (errBody, errHTTPCode), ServerT, (:<|>) (..))
import Test.Hspec

import TDF.API.Proposals (ProposalVersionDTO, ProposalVersionSummaryDTO, ProposalsAPI)
import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.DB (Env (..))
import TDF.Models (RoleEnum (..))
import TDF.ServerProposals (proposalsServer)

type ProposalTestM = ReaderT Env (ExceptT ServerError IO)

spec :: Spec
spec = describe "TDF.ServerProposals proposal versions" $ do
  it "rejects missing proposals instead of returning an empty version list" $ do
    result <-
      runProposalTest $
        listVersionsHandlerFor
          (mkUser [Admin])
          "550e8400-e29b-41d4-a716-446655440000"

    case result of
      Left err -> do
        errHTTPCode err `shouldBe` 404
        BL8.unpack (errBody err) `shouldContain` "Proposal not found"
      Right versions ->
        expectationFailure
          ("Expected missing proposal lookup to fail, got versions: " <> show versions)

  it "rejects missing proposals before looking up nested version rows" $ do
    result <-
      runProposalTest $
        getVersionHandlerFor
          (mkUser [Admin])
          "550e8400-e29b-41d4-a716-446655440000"
          1

    case result of
      Left err -> do
        errHTTPCode err `shouldBe` 404
        BL8.unpack (errBody err) `shouldContain` "Proposal not found"
      Right versionDto ->
        expectationFailure
          ("Expected missing proposal version lookup to fail, got: " <> show versionDto)

mkUser :: [RoleEnum] -> AuthedUser
mkUser roles =
  AuthedUser
    { auPartyId = toSqlKey 1
    , auRoles = roles
    , auModules = modulesForRoles roles
    }

runProposalTest :: ProposalTestM a -> IO (Either ServerError a)
runProposalTest action =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeProposalSchema pool
    let env =
          Env
            { envPool = pool
            , envConfig = error "envConfig should be unused in proposal tests"
            }
    liftIO $ runExceptT (runReaderT action env)

listVersionsHandlerFor :: AuthedUser -> Text -> ProposalTestM [ProposalVersionSummaryDTO]
listVersionsHandlerFor user rawId =
  case (proposalsServer user :: ServerT ProposalsAPI ProposalTestM) of
    _listProposals :<|> _createProposal :<|> proposalRoutes ->
      case proposalRoutes rawId of
        _getProposal
          :<|> _updateProposal
          :<|> listVersions
          :<|> _createVersion
          :<|> _getVersion
          :<|> _proposalPdf ->
            listVersions

getVersionHandlerFor :: AuthedUser -> Text -> Int -> ProposalTestM ProposalVersionDTO
getVersionHandlerFor user rawId versionNumber =
  case (proposalsServer user :: ServerT ProposalsAPI ProposalTestM) of
    _listProposals :<|> _createProposal :<|> proposalRoutes ->
      case proposalRoutes rawId of
        _getProposal
          :<|> _updateProposal
          :<|> _listVersions
          :<|> _createVersion
          :<|> getVersion
          :<|> _proposalPdf ->
            getVersion versionNumber

initializeProposalSchema :: SqlPersistT IO ()
initializeProposalSchema = do
  rawExecute "PRAGMA foreign_keys = ON" []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"proposal\" (\
    \\"id\" TEXT PRIMARY KEY,\
    \\"title\" VARCHAR NOT NULL,\
    \\"service_kind\" VARCHAR NULL,\
    \\"client_party_id\" INTEGER NULL,\
    \\"contact_name\" VARCHAR NULL,\
    \\"contact_email\" VARCHAR NULL,\
    \\"contact_phone\" VARCHAR NULL,\
    \\"pipeline_card_id\" TEXT NULL,\
    \\"status\" VARCHAR NOT NULL,\
    \\"notes\" VARCHAR NULL,\
    \\"created_at\" TIMESTAMP NOT NULL,\
    \\"updated_at\" TIMESTAMP NOT NULL,\
    \\"last_generated_at\" TIMESTAMP NULL,\
    \\"sent_at\" TIMESTAMP NULL\
    \)"
    []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"proposal_version\" (\
    \\"id\" TEXT PRIMARY KEY,\
    \\"proposal_id\" TEXT NOT NULL,\
    \\"version\" INTEGER NOT NULL,\
    \\"latex\" VARCHAR NOT NULL,\
    \\"created_at\" TIMESTAMP NOT NULL,\
    \\"created_by_ref\" VARCHAR NULL,\
    \\"notes\" VARCHAR NULL,\
    \CONSTRAINT \"unique_proposal_version\" UNIQUE (\"proposal_id\", \"version\")\
    \)"
    []
