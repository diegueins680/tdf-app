{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.ServerProposalsSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Database.Persist (get, insertKey)
import Database.Persist.Sql (SqlPersistT, rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Sqlite (createSqlitePool)
import Servant (ServerError (errBody, errHTTPCode), ServerT, (:<|>) (..))
import Test.Hspec
import Web.PathPieces (fromPathPiece)

import TDF.API.Proposals (ProposalDTO, ProposalUpdate (..), ProposalVersionDTO, ProposalVersionSummaryDTO, ProposalsAPI)
import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.DB (Env (..))
import TDF.Models (RoleEnum (..))
import qualified TDF.ModelsExtra as ME
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

  it "rejects empty proposal patch payloads instead of treating them as silent no-op updates" $ do
    let proposalIdText = "550e8400-e29b-41d4-a716-446655440001"
    result <-
      runProposalTest $ do
        seedProposal proposalIdText
        updateProposalHandlerFor
          (mkUser [Admin])
          proposalIdText
          (ProposalUpdate Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

    case result of
      Left err -> do
        errHTTPCode err `shouldBe` 400
        BL8.unpack (errBody err) `shouldContain` "Proposal update must include at least one field"
      Right proposalDto ->
        expectationFailure
          ("Expected empty proposal patch to fail, got: " <> show proposalDto)

  it "rejects control characters in proposal contact names before mutating persisted records" $ do
    let proposalIdText = "550e8400-e29b-41d4-a716-446655440002"
    result <-
      runProposalTest $ do
        seedProposal proposalIdText
        rejected <-
          captureProposalError $
            updateProposalHandlerFor
              (mkUser [Admin])
              proposalIdText
              (ProposalUpdate
                Nothing
                Nothing
                Nothing
                Nothing
                (Just (Just "Ops\nBcc"))
                Nothing
                Nothing
                Nothing
                Nothing
              )
        persistedContactName <-
          runProposalSql $
            fmap (fmap ME.proposalContactName) (get (fixtureProposalKey proposalIdText))
        pure (rejected, persistedContactName)

    case result of
      Left err ->
        expectationFailure ("Expected contact-name rejection to be handled in the inner proposal action, got: " <> show err)
      Right (rejected, persistedContactName) -> do
        case rejected of
          Left err -> do
            errHTTPCode err `shouldBe` 400
            BL8.unpack (errBody err) `shouldContain` "contactName must not contain control characters"
          Right proposalDto ->
            expectationFailure
              ("Expected invalid contactName patch to fail, got: " <> show proposalDto)
        persistedContactName `shouldBe` Just (Just "Ops")

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

runProposalSql :: SqlPersistT IO a -> ProposalTestM a
runProposalSql action = do
  env <- ask
  liftIO $ runSqlPool action (envPool env)

captureProposalError :: ProposalTestM a -> ProposalTestM (Either ServerError a)
captureProposalError action = do
  env <- ask
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

updateProposalHandlerFor :: AuthedUser -> Text -> ProposalUpdate -> ProposalTestM ProposalDTO
updateProposalHandlerFor user rawId payload =
  case (proposalsServer user :: ServerT ProposalsAPI ProposalTestM) of
    _listProposals :<|> _createProposal :<|> proposalRoutes ->
      case proposalRoutes rawId of
        _getProposal
          :<|> updateProposal
          :<|> _listVersions
          :<|> _createVersion
          :<|> _getVersion
          :<|> _proposalPdf ->
            updateProposal payload

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

seedProposal :: Text -> ProposalTestM ()
seedProposal rawId = do
  let proposalKey = fixtureProposalKey rawId
  let fixtureTime = UTCTime (fromGregorian 2026 4 24) (secondsToDiffTime 0)
  runProposalSql $
    insertKey proposalKey ME.Proposal
      { ME.proposalTitle = "Studio proposal"
      , ME.proposalServiceKind = Nothing
      , ME.proposalClientPartyId = Nothing
      , ME.proposalContactName = Just "Ops"
      , ME.proposalContactEmail = Just "ops@example.com"
      , ME.proposalContactPhone = Just "+593991234567"
      , ME.proposalPipelineCardId = Nothing
      , ME.proposalStatus = "draft"
      , ME.proposalNotes = Just "Initial draft"
      , ME.proposalCreatedAt = fixtureTime
      , ME.proposalUpdatedAt = fixtureTime
      , ME.proposalLastGeneratedAt = Nothing
      , ME.proposalSentAt = Nothing
      }

fixtureProposalKey :: Text -> ME.ProposalId
fixtureProposalKey rawId =
  case (fromPathPiece rawId :: Maybe ME.ProposalId) of
    Just key -> key
    Nothing -> error ("invalid proposal fixture key: " <> show rawId)

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
