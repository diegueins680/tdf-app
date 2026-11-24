{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.EventAdminDelegationSpec (spec) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist (PersistValue(..))
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), fromSqlKey, rawExecute, rawSql, runSqlPool, toSqlKey)
import Servant (Handler, NoContent, ServerError(errHTTPCode), (:<|>)(..))
import Servant.Server.Internal.Handler (runHandler)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Hspec

import TDF.Auth (AuthedUser(..), modulesForRoles)
import TDF.Config (loadConfig)
import TDF.DB (Env(..))
import TDF.DTO.SocialEventsDTO
  ( EventAdminAccessDTO(..), EventAdminInvitationCreateDTO(..), EventAdminInvitationDTO(..)
  , EventAdminInvitationIssuedDTO(..), EventAdminInvitationPreviewDTO(..)
  , EventAdminInvitationTokenDTO(..), EventAdminMemberDTO(..), EventAdminRosterDTO(..)
  , EventCourtesyTicketDTO(..), EventLogisticsPlanDTO(..), TicketOrderDTO(..) )
import TDF.Models (RoleEnum(..))
import TDF.Server.SocialEventsHandlers (socialEventsServer)
import qualified TDF.Ticketing.Admission as Admission

-- EVT-ADMIN-DELEGATION-001: the organizer delegates one event through a
-- single-use link; the accepting account administers that event only, and
-- only while the grant exists. Runs against PostgreSQL because single use and
-- revocation ordering rest on the event row lock.
spec :: Spec
spec = describe "event-admin-delegation-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_EVENT_ADMIN_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-event-admin-delegation.sh"
    Just _ | any (maybe False (not . null)) routingOverrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $
        expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 10)) $
      afterAll (\pool -> removeFixtures pool >> destroyAllResources pool) $ do
        it "lets only the organizer issue a link and never stores the raw token" $ \pool -> do
          (eventId, _) <- newEvent pool
          stranger <- run pool (issue guest eventId (EventAdminInvitationCreateDTO "Invitada"))
          status stranger `shouldBe` Just 403
          blank <- run pool (issue organizer eventId (EventAdminInvitationCreateDTO "   "))
          status blank `shouldBe` Just 400
          issued <- expectRight =<< run pool (issue organizer eventId (EventAdminInvitationCreateDTO " Invitada Uno "))
          T.length (eaisToken issued) `shouldBe` 64
          eaiInviteeName (eaisInvitation issued) `shouldBe` "Invitada Uno"
          eaiStatus (eaisInvitation issued) `shouldBe` "pending"
          stored <- runSqlPool (rawSql
            "SELECT count(*) FROM event_admin_invitation WHERE event_id = ? AND token_sha256 = ?"
            [PersistInt64 eventId, PersistText (eaisToken issued)]) pool
          stored `shouldBe` [Single (0 :: Int64)]

        it "grants administration to the accepting account and is idempotent for it" $ \pool -> do
          (eventId, _) <- newEvent pool
          issued <- expectRight =<< run pool (issue organizer eventId (EventAdminInvitationCreateDTO "Invitada"))
          let token = EventAdminInvitationTokenDTO (eaisToken issued)
          before <- expectRight =<< run pool (access guest eventId)
          eaaAccess before `shouldBe` "none"
          previewed <- expectRight =<< run pool (preview guest token)
          eaipEventId previewed `shouldBe` textId eventId
          eaipInviteeName previewed `shouldBe` "Invitada"
          accepted <- expectRight =<< run pool (accept guest token)
          accepted `shouldBe` EventAdminAccessDTO (textId eventId) "admin"
          again <- expectRight =<< run pool (accept guest token)
          again `shouldBe` accepted
          ownerRoster <- expectRight =<< run pool (roster organizer eventId)
          map eamPartyId (earAdmins ownerRoster) `shouldBe` [partyText guest]
          map eaiStatus (earInvitations ownerRoster) `shouldBe` ["accepted"]
          asAdmin <- expectRight =<< run pool (roster guest eventId)
          (earAccess asAdmin, earInvitations asAdmin) `shouldBe` ("admin", [])

        it "binds a link to one account, one event and its issuer's authority" $ \pool -> do
          (eventId, _) <- newEvent pool
          (otherEvent, _) <- newEvent pool
          issued <- expectRight =<< run pool (issue organizer eventId (EventAdminInvitationCreateDTO "Invitada"))
          let token = EventAdminInvitationTokenDTO (eaisToken issued)
          own <- run pool (accept organizer token)
          status own `shouldBe` Just 409
          _ <- expectRight =<< run pool (accept guest token)
          second <- run pool (accept otherGuest token)
          status second `shouldBe` Just 404
          elsewhere <- expectRight =<< run pool (access guest otherEvent)
          eaaAccess elsewhere `shouldBe` "none"
          malformed <- run pool (accept otherGuest (EventAdminInvitationTokenDTO "not-a-token"))
          status malformed `shouldBe` Just 404
          -- A link issued by a former organizer stops working.
          later <- expectRight =<< run pool (issue organizer otherEvent (EventAdminInvitationCreateDTO "Otra"))
          runSqlPool (rawExecute "UPDATE social_event SET organizer_party_id = '7399' WHERE id = ?"
            [PersistInt64 otherEvent]) pool
          orphaned <- run pool (accept otherGuest (EventAdminInvitationTokenDTO (eaisToken later)))
          status orphaned `shouldBe` Just 404

        it "admits exactly one of several accounts racing for the same link" $ \pool -> do
          (eventId, _) <- newEvent pool
          issued <- expectRight =<< run pool (issue organizer eventId (EventAdminInvitationCreateDTO "Invitada"))
          let token = EventAdminInvitationTokenDTO (eaisToken issued)
              racers = map user [7310 .. 7317]
          outcomes <- within (mapConcurrently (\racer -> run pool (accept racer token)) racers)
          length (filter isRight outcomes) `shouldBe` 1
          [code | Left err <- outcomes, let code = errHTTPCode err] `shouldBe` replicate 7 404
          members <- runSqlPool (rawSql
            "SELECT count(*) FROM event_admin_member WHERE event_id = ?" [PersistInt64 eventId]) pool
          members `shouldBe` [Single (1 :: Int64)]

        it "refuses revoked links and keeps a removed administrator out" $ \pool -> do
          (eventId, _) <- newEvent pool
          first <- expectRight =<< run pool (issue organizer eventId (EventAdminInvitationCreateDTO "Uno"))
          byGuest <- run pool (revoke guest eventId (eaiId (eaisInvitation first)))
          status byGuest `shouldBe` Just 403
          _ <- expectRight =<< run pool (revoke organizer eventId (eaiId (eaisInvitation first)))
          revoked <- run pool (accept guest (EventAdminInvitationTokenDTO (eaisToken first)))
          status revoked `shouldBe` Just 404
          second <- expectRight =<< run pool (issue organizer eventId (EventAdminInvitationCreateDTO "Dos"))
          let token = EventAdminInvitationTokenDTO (eaisToken second)
          _ <- expectRight =<< run pool (accept guest token)
          late <- run pool (revoke organizer eventId (eaiId (eaisInvitation second)))
          status late `shouldBe` Just 409
          selfService <- run pool (remove guest eventId (partyText guest))
          status selfService `shouldBe` Just 403
          _ <- expectRight =<< run pool (remove organizer eventId (partyText guest))
          afterRemoval <- expectRight =<< run pool (access guest eventId)
          eaaAccess afterRemoval `shouldBe` "none"
          replay <- run pool (accept guest token)
          status replay `shouldBe` Just 404
          delegatedIssue <- run pool (issue guest eventId (EventAdminInvitationCreateDTO "Tres"))
          status delegatedIssue `shouldBe` Just 403

        it "lets a delegated administrator plan logistics, issue courtesy tickets and admit them" $ \pool -> do
          (eventId, tierId) <- newEvent pool
          let courtesy = EventCourtesyTicketDTO (textId tierId) 2 "Invitado de Cortesía" Nothing
          refusedBefore <- run pool (issueCourtesy guest eventId courtesy)
          status refusedBefore `shouldBe` Just 403
          planBefore <- run pool (logisticsPlan guest eventId)
          status planBefore `shouldBe` Just 403
          issued <- expectRight =<< run pool (issue organizer eventId (EventAdminInvitationCreateDTO "Invitada"))
          _ <- expectRight =<< run pool (accept guest (EventAdminInvitationTokenDTO (eaisToken issued)))
          plan <- expectRight =<< run pool (logisticsPlan guest eventId)
          elgAccessRole plan `shouldBe` "owner"
          order <- expectRight =<< run pool (issueCourtesy guest eventId courtesy)
          (ticketOrderAmountCents order, ticketOrderStatusValue order, ticketOrderQuantity order)
            `shouldBe` (0, "paid", 2)
          length (ticketOrderTickets order) `shouldBe` 2
          sold <- runSqlPool (rawSql "SELECT quantity_sold FROM event_ticket_tier WHERE id = ?"
            [PersistInt64 tierId]) pool
          sold `shouldBe` [Single (2 :: Int64)]
          tooMany <- run pool (issueCourtesy guest eventId courtesy{ectQuantity = 11})
          status tooMany `shouldBe` Just 400
          nameless <- run pool (issueCourtesy organizer eventId courtesy{ectHolderName = "  "})
          status nameless `shouldBe` Just 400
          soldOut <- run pool (issueCourtesy organizer eventId courtesy{ectQuantity = 4})
          status soldOut `shouldBe` Just 409
          -- A refusal for a closed tier must not count against its inventory.
          runSqlPool (rawExecute "UPDATE event_ticket_tier SET is_active = false WHERE id = ?"
            [PersistInt64 tierId]) pool
          closed <- run pool (issueCourtesy organizer eventId courtesy{ectQuantity = 1})
          status closed `shouldBe` Just 409
          runSqlPool (rawExecute "UPDATE event_ticket_tier SET is_active = true WHERE id = ?"
            [PersistInt64 tierId]) pool
          soldAfterRefusals <- runSqlPool (rawSql "SELECT quantity_sold FROM event_ticket_tier WHERE id = ?"
            [PersistInt64 tierId]) pool
          soldAfterRefusals `shouldBe` [Single (2 :: Int64)]
          now <- getCurrentTime
          codes <- runSqlPool (rawSql
            "SELECT code FROM event_ticket WHERE event_id = ? ORDER BY id" [PersistInt64 eventId]) pool
          code <- case codes of
            (Single value : _) -> pure (value :: Text)
            [] -> fail "courtesy tickets were not issued"
          byStranger <- runSqlPool (Admission.admitTicket (partyText otherGuest) (toSqlKey eventId)
            (Admission.AdmissionByCode code) now) pool
          either Just (const Nothing) byStranger `shouldBe` Just Admission.AdmissionForbidden
          byAdmin <- runSqlPool (Admission.admitTicket (partyText guest) (toSqlKey eventId)
            (Admission.AdmissionByCode code) now) pool
          isRight byAdmin `shouldBe` True
          _ <- expectRight =<< run pool (remove organizer eventId (partyText guest))
          afterRemoval <- run pool (issueCourtesy guest eventId courtesy{ectQuantity = 1})
          status afterRemoval `shouldBe` Just 403

type App = ReaderT Env Handler

data AdminHandlers = AdminHandlers
  { hRoster :: Text -> App EventAdminRosterDTO
  , hAccess :: Text -> App EventAdminAccessDTO
  , hIssue :: Text -> EventAdminInvitationCreateDTO -> App EventAdminInvitationIssuedDTO
  , hRevoke :: Text -> Text -> App NoContent
  , hRemove :: Text -> Text -> App NoContent
  , hPreview :: EventAdminInvitationTokenDTO -> App EventAdminInvitationPreviewDTO
  , hAccept :: EventAdminInvitationTokenDTO -> App EventAdminAccessDTO
  , hCourtesy :: Text -> EventCourtesyTicketDTO -> App TicketOrderDTO
  , hLogisticsPlan :: Text -> App EventLogisticsPlanDTO
  }

handlersFor :: AuthedUser -> AdminHandlers
handlersFor actor =
  case socialEventsServer actor of
    _events :<|> _cities :<|> _sources :<|> _research :<|> _venues :<|> _artists :<|> _rsvps
      :<|> _invitations :<|> _moments :<|> _live :<|> _tickets :<|> _budget :<|> _finance
      :<|> logistics :<|> admins :<|> _memory ->
        case admins of
          rosterH :<|> accessH :<|> issueH :<|> revokeH :<|> removeH :<|> previewH :<|> acceptH :<|> courtesyH ->
            AdminHandlers rosterH accessH issueH revokeH removeH previewH acceptH courtesyH
              (\eventId -> case logistics eventId of planH :<|> _ -> planH)

roster :: AuthedUser -> Int64 -> App EventAdminRosterDTO
roster actor eventId = hRoster (handlersFor actor) (textId eventId)

access :: AuthedUser -> Int64 -> App EventAdminAccessDTO
access actor eventId = hAccess (handlersFor actor) (textId eventId)

issue :: AuthedUser -> Int64 -> EventAdminInvitationCreateDTO -> App EventAdminInvitationIssuedDTO
issue actor eventId = hIssue (handlersFor actor) (textId eventId)

revoke :: AuthedUser -> Int64 -> Text -> App NoContent
revoke actor eventId = hRevoke (handlersFor actor) (textId eventId)

remove :: AuthedUser -> Int64 -> Text -> App NoContent
remove actor eventId = hRemove (handlersFor actor) (textId eventId)

preview :: AuthedUser -> EventAdminInvitationTokenDTO -> App EventAdminInvitationPreviewDTO
preview actor = hPreview (handlersFor actor)

accept :: AuthedUser -> EventAdminInvitationTokenDTO -> App EventAdminAccessDTO
accept actor = hAccept (handlersFor actor)

issueCourtesy :: AuthedUser -> Int64 -> EventCourtesyTicketDTO -> App TicketOrderDTO
issueCourtesy actor eventId = hCourtesy (handlersFor actor) (textId eventId)

logisticsPlan :: AuthedUser -> Int64 -> App EventLogisticsPlanDTO
logisticsPlan actor eventId = hLogisticsPlan (handlersFor actor) (textId eventId)

run :: ConnectionPool -> App a -> IO (Either ServerError a)
run pool action = do
  cfg <- loadConfig
  runHandler (runReaderT action (Env pool cfg))

status :: Either ServerError a -> Maybe Int
status = either (Just . errHTTPCode) (const Nothing)

expectRight :: Either ServerError a -> IO a
expectRight = either (\err -> fail ("unexpected refusal: " <> show err)) pure

user :: Int64 -> AuthedUser
user party = AuthedUser
  { auPartyId = toSqlKey party
  , auRoles = [Fan]
  , auModules = modulesForRoles [Fan]
  , auApiTokenId = Nothing
  , auSessionWitness = Nothing
  }

organizer, guest, otherGuest :: AuthedUser
organizer = user 7301
guest = user 7302
otherGuest = user 7303

partyText :: AuthedUser -> Text
partyText = textId . fromSqlKey . auPartyId

textId :: Int64 -> Text
textId = T.pack . show

fixtureTitle :: Text
fixtureTitle = "Event admin delegation fixture"

-- An organized event with one five-seat tier priced above zero: a courtesy
-- never depends on a free tier or on the event being on sale.
newEvent :: ConnectionPool -> IO (Int64, Int64)
newEvent pool = do
  events <- runSqlPool (rawSql
    "INSERT INTO social_event(organizer_party_id,title,start_time,end_time,workflow_state_id,event_type_id,created_at,updated_at)\
    \ SELECT '7301', ?, now()+interval '30 days', now()+interval '30 days 3 hours',\
    \ '00000000-0000-4000-8000-000000000231', (SELECT id FROM event_type ORDER BY id LIMIT 1), now(), now()\
    \ RETURNING id" [PersistText fixtureTitle]) pool
  eventId <- case events of
    [Single key] -> pure (key :: Int64)
    _ -> fail "Expected one event"
  tiers <- runSqlPool (rawSql
    "INSERT INTO event_ticket_tier(event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active,\
    \ created_at,updated_at,enable_waitlist,allow_transfers,refund_policy)\
    \ VALUES (?, 'general', 'General', 2000, 'USD', 5, 0, TRUE, now(), now(), FALSE, FALSE, 'none') RETURNING id"
    [PersistInt64 eventId]) pool
  case tiers of
    [Single key] -> pure (eventId, key)
    _ -> fail "Expected one tier"

removeFixtures :: ConnectionPool -> IO ()
removeFixtures = runSqlPool (mapM_ (\statement -> rawExecute statement [PersistText fixtureTitle])
  [ "DELETE FROM event_ticket_admission_audit WHERE event_id IN (SELECT id FROM social_event WHERE title = ?)"
  , "DELETE FROM event_ticket WHERE event_id IN (SELECT id FROM social_event WHERE title = ?)"
  , "DELETE FROM event_ticket_order WHERE event_id IN (SELECT id FROM social_event WHERE title = ?)"
  , "DELETE FROM event_ticket_tier WHERE event_id IN (SELECT id FROM social_event WHERE title = ?)"
  , "DELETE FROM social_event WHERE title = ?"
  ])

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://127.0.0.1:", "postgresql://localhost/",
   "postgresql://localhost:", "postgresql://postgres@127.0.0.1:", "postgresql://postgres:postgres@postgres:5432/"]

within :: IO a -> IO a
within action = timeout 30000000 action >>= maybe (fail "Delegation case timed out") pure
