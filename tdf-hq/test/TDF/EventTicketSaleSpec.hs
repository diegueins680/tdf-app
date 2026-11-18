{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module TDF.EventTicketSaleSpec (spec) where

import Control.Monad (forM_)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Char8 as BS
import Data.Either (rights)
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf, nub)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, getCurrentTime, secondsToDiffTime)
import Database.Persist (PersistValue (..))
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single (..), rawExecute, rawSql, runSqlPool, toSqlKey)
import Servant (Handler, ServerError (errHTTPCode), (:<|>) (..))
import Servant.Server.Internal.Handler (runHandler)
import System.Environment (lookupEnv)
import Test.Hspec

import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.Config (loadConfig)
import TDF.DB (Env (..))
import TDF.DTO.SocialEventsDTO
  ( EventTicketSaleDTO (..), EventTicketSaleRequestDTO (..), EventTicketSaleTermsDTO (..) )
import TDF.Models (RoleEnum (..))
import TDF.Server.EventTicketCheckout (ApprovedTicketPolicy (..), loadApprovedTicketPolicy)
import TDF.Server.EventTicketSale
  ( SalePolicyRow (..), saleStateFromRows, standardTicketSaleTerms, ticketSalePolicyVersion )
import TDF.Server.SocialEventsHandlers (socialEventsServer)

-- EVT-TICKET-SALE-001: an event manager asks to sell paid tickets under the
-- standard terms and only a strict administrator decides. The decisions run
-- against PostgreSQL because the single pending request and the single
-- active policy rest on the event row lock and the policy table's guards.
spec :: Spec
spec = do
  describe "event-ticket-sale-state" $ do
    let at day = UTCTime (fromGregorian 2026 11 day) (secondsToDiffTime 0)
        row ident approval active approved =
          SalePolicyRow ident approval active approved (at 10) True True
            (approved == Nothing) standardTicketSaleTerms
    it "prefers the active approved policy, then a pending draft" $ do
      fst (saleStateFromRows [row "b" "draft" False Nothing, row "a" "approved" True (Just (at 11))]) `shouldBe` "approved"
      fst (saleStateFromRows [row "b" "draft" False Nothing, row "a" "retired" False Nothing]) `shouldBe` "requested"
    it "reports a declined request only for the newest never-approved row" $ do
      saleStateFromRows [row "a" "retired" False Nothing] `shouldBe` ("declined", Just (row "a" "retired" False Nothing))
      fst (saleStateFromRows [row "a" "retired" False (Just (at 11))]) `shouldBe` "none"
      fst (saleStateFromRows [row "a" "approved" False (Just (at 11))]) `shouldBe` "none"
      fst (saleStateFromRows []) `shouldBe` "none"
    it "ignores unrelated or incompatible draft and retired policies" $ do
      let declined = row "request" "retired" False Nothing
          legacyDraft = (row "legacy" "draft" False Nothing) { sprWorkflowRequest = False }
          legacyRetired = legacyDraft { sprApprovalStatus = "retired" }
          incompatible = (row "changed" "draft" False Nothing) { sprStandardTerms = False }
      saleStateFromRows [legacyDraft, legacyRetired, incompatible] `shouldBe` ("none", Nothing)
      saleStateFromRows [legacyDraft, legacyRetired, incompatible, declined]
        `shouldBe` ("declined", Just declined)
    it "names the event, the requester and the instant in the policy version" $ do
      let version = ticketSalePolicyVersion 300 "7402" (at 15)
      version `shouldBe` "ticket-v1-event-300-req-7402-20261115T000000000000"
      T.length (ticketSalePolicyVersion 2147483647 "2147483647" (at 15)) `shouldSatisfy` (<= 80)

  describe "event-ticket-sale-postgresql" $ do
    configured <- runIO (lookupEnv "TDF_EVENT_TICKET_SALE_DATABASE_URL")
    routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
    case configured of
      Nothing -> it "requires the isolated fully migrated integration runner" $
        pendingWith "Run scripts/test-event-ticket-sale.sh"
      Just _ | any (maybe False (not . null)) routingOverrides ->
        it "refuses inherited libpq routing overrides" $
          expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
      Just connection | not (safeConnection connection) ->
        it "refuses a non-disposable database" $
          expectationFailure "Use the isolated integration runner"
      Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 10)) $
        afterAll (\pool -> removeFixtures pool >> destroyAllResources pool) $ do
          it "lets only the event's manager see and request the sale, with the standard terms" $ \pool -> do
            eventId <- newEvent pool 2000
            status <$> run pool (sale stranger eventId) `shouldReturn` Just 403
            status <$> run pool (request stranger eventId) `shouldReturn` Just 403
            initial <- expectRight =<< run pool (sale organizer eventId)
            (etsStatus initial, etsCanRequest initial, etsCanDecide initial) `shouldBe` ("none", True, False)
            etsTerms initial `shouldBe` standardTicketSaleTerms
            requested <- expectRight =<< run pool (request organizer eventId)
            (etsStatus requested, etsCanRequest requested, etsBlockedReason requested)
              `shouldBe` ("requested", False, Just "already_requested")
            policies pool eventId `shouldReturn` [("draft", False, 200, 200, "USD", "ticket-terms-v1")]
            -- The administrator sees the same request and may decide it.
            adminView <- expectRight =<< run pool (sale admin eventId)
            (etsStatus adminView, etsCanDecide adminView) `shouldBe` ("requested", True)

          it "refuses a request while the event has no active paid tier in USD" $ \pool -> do
            eventId <- newEvent pool 0
            initial <- expectRight =<< run pool (sale organizer eventId)
            (etsCanRequest initial, etsBlockedReason initial) `shouldBe` (False, Just "no_paid_tier")
            status <$> run pool (request organizer eventId) `shouldReturn` Just 409
            policies pool eventId `shouldReturn` []

          it "recomputes eligibility when a USD tier becomes paid or inactive" $ \pool -> do
            eventId <- newEvent pool 0
            let currentEligibility = do
                  current <- expectRight =<< run pool (sale organizer eventId)
                  pure (etsCanRequest current, etsBlockedReason current)
                changeTier statement =
                  runSqlPool (rawExecute statement [PersistInt64 eventId]) pool
            currentEligibility `shouldReturn` (False, Just "no_paid_tier")
            changeTier "UPDATE event_ticket_tier SET price_cents = 2000 WHERE event_id = ?"
            currentEligibility `shouldReturn` (True, Nothing)
            changeTier "UPDATE event_ticket_tier SET is_active = FALSE WHERE event_id = ?"
            currentEligibility `shouldReturn` (False, Just "no_paid_tier")

          it "preserves migrated drafts and activates the exact new policy used by public checkout" $ \pool -> do
            eventId <- newEvent pool 2000
            [Single legacyId] <- runSqlPool (rawSql
              "INSERT INTO event_ticket_checkout_policy(event_id, policy_version, currency,\
              \ terms_version, terms_summary, refund_policy, transfer_allowed)\
              \ VALUES (?, 'ticket-v1-db-' || ?::text, 'USD', 'ticket-terms-draft-v1',\
              \ 'Borrador migrado', 'Política de reembolso pendiente de aprobación; no activar este borrador.', FALSE)\
              \ RETURNING id::text" [PersistInt64 eventId, PersistInt64 eventId]) pool
            initial <- expectRight =<< run pool (sale organizer eventId)
            (etsStatus initial, etsCanRequest initial, etsRequestId initial)
              `shouldBe` ("none", True, Nothing)
            adminView <- expectRight =<< run pool (sale admin eventId)
            etsCanDecide adminView `shouldBe` False
            waiting <- expectRight =<< run pool (queue admin)
            map etsrRequestId waiting `shouldNotContain` [legacyId]
            status <$> run pool (approve admin legacyId) `shouldReturn` Just 404
            status <$> run pool (decline admin legacyId) `shouldReturn` Just 404
            now <- getCurrentTime
            runSqlPool (loadApprovedTicketPolicy now (toSqlKey eventId)) pool `shouldReturn` Nothing

            requested <- expectRight =<< run pool (request organizer eventId)
            requestId <- maybe (fail "missing request id") pure (etsRequestId requested)
            requestId `shouldNotBe` legacyId
            reviewed <- expectRight =<< run pool (queue admin)
            [etsrTerms item | item <- reviewed, etsrRequestId item == requestId]
              `shouldBe` [etsTerms requested]
            approved <- expectRight =<< run pool (approve admin requestId)
            etsTerms approved `shouldBe` etsTerms requested

            checkoutTime <- getCurrentTime
            checkoutPolicy <- runSqlPool (loadApprovedTicketPolicy checkoutTime (toSqlKey eventId)) pool
            let EventTicketSaleTermsDTO{..} = etsTerms requested
            versionRows <- runSqlPool (rawSql
              "SELECT policy_version FROM event_ticket_checkout_policy WHERE id = ?::uuid"
              [PersistText requestId]) pool
            case versionRows of
              [Single version] -> checkoutPolicy `shouldBe` Just ApprovedTicketPolicy
                { atpId = requestId
                , atpVersion = version
                , atpCurrency = etstCurrency
                , atpBuyerFeeBps = etstBuyerFeeBps
                , atpOrganizerFeeBps = etstOrganizerFeeBps
                , atpTaxIncluded = False
                , atpTaxBps = etstTaxBps
                , atpHoldMinutes = etstHoldMinutes
                , atpTermsVersion = "ticket-terms-v1"
                , atpTermsSummary = etstTermsSummary
                , atpRefundPolicy = etstRefundPolicy
                , atpTransferAllowed = True
                , atpMaxTicketsPerOrder = etstMaxTicketsPerOrder
                , atpManualTransferHoldMinutes = Nothing
                , atpManualTransferCutoffAt = Nothing
                , atpTaxInvoiceRequired = False
                }
              _ -> expectationFailure "Expected the approved policy version"
            policies pool eventId `shouldReturn`
              [ ("draft", False, 200, 200, "USD", "ticket-terms-draft-v1")
              , ("approved", True, 200, 200, "USD", "ticket-terms-v1")
              ]

          it "rejects incompatible drafts with a genuine request version" $ \pool -> do
            forM_
              [ "buyer_fee_bps = 900"
              , "organizer_fee_bps = 900"
              , "currency = 'EUR'"
              , "tax_bps = 100"
              , "terms_version = 'ticket-terms-draft-v1'"
              -- The terms identity trigger forbids one terms_version naming two
              -- texts for an event, so different texts need their own version.
              , "terms_version = 'ticket-terms-draft-v2', terms_summary = 'Condiciones distintas'"
              , "terms_version = 'ticket-terms-draft-v3', refund_policy = 'Sin reembolsos'"
              , "hold_minutes = 30"
              , "max_tickets_per_order = 8"
              , "tax_included = TRUE"
              , "tax_invoice_required = TRUE"
              , "transfer_allowed = FALSE"
              , "transfer_deadline = now()"
              , "manual_transfer_hold_minutes = 120, manual_transfer_cutoff_at = now()"
              , "effective_until = now() - interval '1 day'"
              ] $ \change -> do
                eventId <- newEvent pool 2000
                requested <- expectRight =<< run pool (request organizer eventId)
                requestId <- maybe (fail "missing request id") pure (etsRequestId requested)
                runSqlPool (rawExecute
                  ("UPDATE event_ticket_checkout_policy SET " <> change <> " WHERE id = ?::uuid")
                  [PersistText requestId]) pool
                waiting <- expectRight =<< run pool (queue admin)
                map etsrRequestId waiting `shouldNotContain` [requestId]
                adminView <- expectRight =<< run pool (sale admin eventId)
                etsCanDecide adminView `shouldBe` False
                status <$> run pool (approve admin requestId) `shouldReturn` Just 409
                now <- getCurrentTime
                runSqlPool (loadApprovedTicketPolicy now (toSqlKey eventId)) pool `shouldReturn` Nothing
                replacement <- expectRight =<< run pool (request organizer eventId)
                etsStatus replacement `shouldBe` "requested"
                etsRequestId replacement `shouldNotBe` Just requestId

          it "keeps one pending request when the manager asks many times at once" $ \pool -> do
            eventId <- newEvent pool 2000
            answers <- mapConcurrently (const (run pool (request organizer eventId))) [1 :: Int .. 8]
            length (rights answers) `shouldBe` 8
            nub (map etsRequestId (rights answers)) `shouldSatisfy` ((== 1) . length)
            length <$> policies pool eventId `shouldReturn` 1

          it "lists pending requests to strict administrators only and approves once" $ \pool -> do
            eventId <- newEvent pool 2000
            requested <- expectRight =<< run pool (request organizer eventId)
            requestId <- maybe (fail "missing request id") pure (etsRequestId requested)
            status <$> run pool (queue organizer) `shouldReturn` Just 403
            status <$> run pool (approve organizer requestId) `shouldReturn` Just 403
            status <$> run pool (approve manager requestId) `shouldReturn` Just 403
            waiting <- expectRight =<< run pool (queue admin)
            [etsrEventTitle item | item <- waiting, etsrRequestId item == requestId] `shouldBe` [fixtureTitle]
            approved <- expectRight =<< run pool (approve admin requestId)
            (etsStatus approved, etsCanRequest approved, etsCanDecide approved) `shouldBe` ("approved", False, False)
            policies pool eventId `shouldReturn` [("approved", True, 200, 200, "USD", "ticket-terms-v1")]
            approvers <- runSqlPool (rawSql
              "SELECT approved_by FROM event_ticket_checkout_policy WHERE event_id = ?" [PersistInt64 eventId]) pool
            approvers `shouldBe` [Single ("party:7403" :: Text)]
            again <- expectRight =<< run pool (approve admin requestId)
            etsStatus again `shouldBe` "approved"
            status <$> run pool (decline admin requestId) `shouldReturn` Just 409
            remaining <- expectRight =<< run pool (queue admin)
            [() | item <- remaining, etsrRequestId item == requestId] `shouldBe` []
            -- A second request is not needed and creates nothing.
            repeated <- expectRight =<< run pool (request organizer eventId)
            etsStatus repeated `shouldBe` "approved"
            length <$> policies pool eventId `shouldReturn` 1

          it "declines without opening checkout and lets the manager ask again" $ \pool -> do
            eventId <- newEvent pool 2000
            first <- expectRight =<< run pool (request organizer eventId)
            firstId <- maybe (fail "missing request id") pure (etsRequestId first)
            declined <- expectRight =<< run pool (decline admin firstId)
            -- The answer describes the deciding administrator, who cannot request.
            (etsStatus declined, etsCanRequest declined, etsBlockedReason declined)
              `shouldBe` ("declined", False, Just "not_manager")
            seenByOrganizer <- expectRight =<< run pool (sale organizer eventId)
            (etsStatus seenByOrganizer, etsCanRequest seenByOrganizer) `shouldBe` ("declined", True)
            status <$> run pool (approve admin firstId) `shouldReturn` Just 409
            second <- expectRight =<< run pool (request organizer eventId)
            etsStatus second `shouldBe` "requested"
            etsRequestId second `shouldSatisfy` (/= Just firstId)
            map (\(approval, active, _, _, _, _) -> (approval, active)) <$> policies pool eventId
              `shouldReturn` [("retired", False), ("draft", False)]

          it "refuses approval once the paid tier is gone and for unknown requests" $ \pool -> do
            eventId <- newEvent pool 2000
            requested <- expectRight =<< run pool (request organizer eventId)
            requestId <- maybe (fail "missing request id") pure (etsRequestId requested)
            runSqlPool (rawExecute "UPDATE event_ticket_tier SET is_active = FALSE WHERE event_id = ?" [PersistInt64 eventId]) pool
            status <$> run pool (approve admin requestId) `shouldReturn` Just 409
            policies pool eventId `shouldReturn` [("draft", False, 200, 200, "USD", "ticket-terms-v1")]
            status <$> run pool (approve admin "00000000-0000-4000-8000-000000000000") `shouldReturn` Just 404
            status <$> run pool (approve admin "not-a-uuid") `shouldReturn` Just 404

type App = ReaderT Env Handler

data SaleHandlers = SaleHandlers
  { hSale :: Text -> App EventTicketSaleDTO
  , hRequest :: Text -> App EventTicketSaleDTO
  , hQueue :: App [EventTicketSaleRequestDTO]
  , hApprove :: Text -> App EventTicketSaleDTO
  , hDecline :: Text -> App EventTicketSaleDTO
  }

handlersFor :: AuthedUser -> SaleHandlers
handlersFor actor =
  case socialEventsServer actor of
    _events :<|> _cities :<|> _sources :<|> _research :<|> _venues :<|> _artists :<|> _rsvps
      :<|> _invitations :<|> _moments :<|> _live :<|> _tickets :<|> _budget :<|> _finance
      :<|> _logistics :<|> _admins :<|> _memory
      :<|> (saleH :<|> requestH :<|> queueH :<|> approveH :<|> declineH) ->
        SaleHandlers saleH requestH queueH approveH declineH

sale, request :: AuthedUser -> Int64 -> App EventTicketSaleDTO
sale actor = hSale (handlersFor actor) . textId
request actor = hRequest (handlersFor actor) . textId

queue :: AuthedUser -> App [EventTicketSaleRequestDTO]
queue = hQueue . handlersFor

approve, decline :: AuthedUser -> Text -> App EventTicketSaleDTO
approve = hApprove . handlersFor
decline = hDecline . handlersFor

run :: ConnectionPool -> App a -> IO (Either ServerError a)
run pool action = do
  cfg <- loadConfig
  runHandler (runReaderT action (Env pool cfg))

status :: Either ServerError a -> Maybe Int
status = either (Just . errHTTPCode) (const Nothing)

expectRight :: Either ServerError a -> IO a
expectRight = either (\err -> fail ("unexpected refusal: " <> show err)) pure

userWith :: [RoleEnum] -> Int64 -> AuthedUser
userWith roles party = AuthedUser
  { auPartyId = toSqlKey party
  , auRoles = roles
  , auModules = modulesForRoles roles
  , auApiTokenId = Nothing
  , auSessionWitness = Nothing
  }

-- A strict administrator holds Admin and nothing outside the strict scope;
-- Admin together with Manager is an ordinary staff account.
organizer, stranger, admin, manager :: AuthedUser
organizer = userWith [Fan] 7401
stranger = userWith [Fan] 7402
admin = userWith [Admin] 7403
manager = userWith [Admin, Manager] 7404

textId :: Int64 -> Text
textId = T.pack . show

fixtureTitle :: Text
fixtureTitle = "Event ticket sale fixture"

-- An organized event with one active USD tier at the given price.
newEvent :: ConnectionPool -> Int64 -> IO Int64
newEvent pool priceCents = do
  events <- runSqlPool (rawSql
    "INSERT INTO social_event(organizer_party_id,title,start_time,end_time,workflow_state_id,event_type_id,created_at,updated_at)\
    \ SELECT '7401', ?, now()+interval '30 days', now()+interval '30 days 3 hours',\
    \ '00000000-0000-4000-8000-000000000231', (SELECT id FROM event_type ORDER BY id LIMIT 1), now(), now()\
    \ RETURNING id" [PersistText fixtureTitle]) pool
  eventId <- case events of
    [Single key] -> pure (key :: Int64)
    _ -> fail "Expected one event"
  runSqlPool (rawExecute
    "INSERT INTO event_ticket_tier(event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active,\
    \ created_at,updated_at,enable_waitlist,allow_transfers,refund_policy)\
    \ VALUES (?, 'general', 'General', ?, 'USD', 5, 0, TRUE, now(), now(), FALSE, FALSE, 'none')"
    [PersistInt64 eventId, PersistInt64 priceCents]) pool
  pure eventId

-- Oldest first: approval state, active, both fees, currency and terms version.
policies :: ConnectionPool -> Int64 -> IO [(Text, Bool, Int, Int, Text, Text)]
policies pool eventId = do
  rows <- runSqlPool (rawSql
    "SELECT approval_status, active, buyer_fee_bps, organizer_fee_bps, currency, terms_version\
    \ FROM event_ticket_checkout_policy WHERE event_id = ? ORDER BY created_at, id" [PersistInt64 eventId]) pool
  pure [ (approval, active, buyer, organizerFee, currency, terms)
       | (Single approval, Single active, Single buyer, Single organizerFee, Single currency, Single terms) <- rows ]

removeFixtures :: ConnectionPool -> IO ()
removeFixtures = runSqlPool (mapM_ (\statement -> rawExecute statement [PersistText fixtureTitle])
  [ "DELETE FROM event_ticket_checkout_policy WHERE event_id IN (SELECT id FROM social_event WHERE title = ?)"
  , "DELETE FROM event_ticket_tier WHERE event_id IN (SELECT id FROM social_event WHERE title = ?)"
  , "DELETE FROM social_event WHERE title = ?"
  ])

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://127.0.0.1:", "postgresql://localhost/",
   "postgresql://localhost:", "postgresql://postgres@127.0.0.1:", "postgresql://postgres:postgres@postgres:5432/"]
