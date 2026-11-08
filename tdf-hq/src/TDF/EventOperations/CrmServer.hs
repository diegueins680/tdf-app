{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.EventOperations.CrmServer (eventCrmServer) where

import Control.Exception.Safe (tryAny)
import Control.Exception (fromException)
import Control.Monad (unless)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value(..), encode, eitherDecodeStrict', object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist (PersistValue(..), Entity(..), getEntity, toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql, runSqlPool, fromSqlKey)
import Database.PostgreSQL.Simple (SqlError(..))
import Servant
import System.Entropy (getEntropy)
import TDF.Auth (AuthedUser(..), withCurrentAuthSession)
import TDF.Config (AppConfig(..))
import TDF.DB (Env(..))
import qualified TDF.Email as Email
import TDF.EventOperations.CrmAPI
import TDF.EventOperations.CrmPayments (manualCrmCommand)
import qualified TDF.Models as M

type AppM = ReaderT Env Handler
type Result = Headers '[Header "Cache-Control" Text] Value

eventCrmServer :: AuthedUser -> ServerT EventCrmAPI AppM
eventCrmServer user = eventRoutes :<|> invitation :<|> requestProof :<|> confirmProof
  where
    actor = fromSqlKey (auPartyId user)
    eventRoutes e = readCrm e :<|> command e :<|> exportCrm e
    readCrm :: Int64 -> Maybe Text -> Maybe Int64 -> Maybe Bool -> AppM Result
    readCrm e stage responsible overdue = private $ do
      validEvent e
      query user "SELECT event_crm_snapshot(?,?,?::jsonb)::text"
        [PersistInt64 e,PersistInt64 actor,jsonValue (object
          ["stage" .= stage,"responsibleId" .= responsible,"overdue" .= overdue])]
    exportCrm e = readCrm e Nothing Nothing Nothing
    command e key requestBody = private $ do
      validEvent e
      unless (BL.length (encode requestBody)<=1048576) $ throwError (failure "invalid_request")
      body <- privateCommand requestBody
      let operation = case body of Object o -> KM.lookup "operation" o; _ -> Nothing
      if operation `elem` [Just (String "manual.submit"),Just (String "manual.review")] then do
        outcome <- runSession user (manualCrmCommand e actor key body)
        either (throwError . failure) pure outcome
      else issueCommand e key body
    issueCommand e key body = do
      secret <- newSecret
      outcome <- query user "SELECT event_crm_command(?,?,?,?::jsonb,?)::text"
        [PersistInt64 e,PersistInt64 actor,toPersistValue key,jsonValue body,PersistText (fingerprint secret)]
      -- Only the first successful issue returns the secret; receipts keep no raw token.
      pure $ case (body,outcome) of
        (Object request,Object result)
          | KM.lookup "operation" request == Just (String "invitation.create")
          , KM.lookup "created" result == Just (Bool True)
          , KM.lookup "replayed" result /= Just (Bool True) ->
              Object (KM.insert "token" (String secret) result)
        _ -> outcome
    invitation (InviteToken token accept) = private $ do
      validToken token
      query user "SELECT event_crm_invitation_action(?,?,?)::text"
        [PersistInt64 actor,PersistText (fingerprint token),PersistBool accept]
    requestProof = private $ do
      Env _ cfg <- ask
      emailCfg <- maybe (throwError (failure "delivery_unavailable")) pure (emailConfig cfg)
      secret <- newSecret
      -- Read the recipient while the proof function's account/email locks are
      -- still held; a concurrent email edit must not redirect verification.
      (proofRows,recipient) <- runSession user $ do
        rows <- rawSql "SELECT event_crm_email_proof(?,?,false)::text"
          [PersistInt64 actor,PersistText (fingerprint secret)]
        party <- getEntity (auPartyId user)
        pure (rows,party)
      outcome <- decodeRows proofRows
      party <- maybe (throwError (failure "email_required")) (pure . entityVal) recipient
      email <- maybe (throwError (failure "email_required")) pure (M.partyPrimaryEmail party)
      -- No URL containing a secret; the short-lived proof is pasted from the email.
      sent <- liftIO $ tryAny $ Email.sendTransactionalEmail emailCfg
        (M.partyDisplayName party) email "Verifica tu correo en TDF Records"
        "Verificación de correo" ["Pega este código en la pantalla de invitación de TDF. Caduca en 30 minutos.",secret] Nothing
      case sent of
        Left _ -> throwError (failure "delivery_unavailable")
        Right () -> pure outcome
    confirmProof (EmailProof token) = private $ do
      validToken token
      query user "SELECT event_crm_email_proof(?,?,true)::text"
        [PersistInt64 actor,PersistText (fingerprint token)]

-- Invitation secrets and recipient emails never become SQL bind parameters.
-- Database diagnostic settings cannot expose a raw invitation or recipient address.
privateCommand :: Value -> AppM Value
privateCommand (Object request) = case KM.lookup "operation" request of
  Just (String "invitation.create") -> case KM.lookup "recipientEmail" request of
    Just (String raw) -> do
      email <- canonicalEmail raw
      unless (not (KM.member "recipientEmailHash" request)) $ throwError (failure "invalid_request")
      pure (Object (KM.insert "recipientEmailHash" (String (fingerprint email)) (KM.delete "recipientEmail" request)))
    Nothing -> pure (Object request)
    _ -> throwError (failure "invalid_request")
  Just (String "recipient.lookup") -> case KM.lookup "query" request of
    Just (String raw) | T.any (=='@') raw -> do
      email <- canonicalEmail raw
      pure (Object (KM.insert "emailHash" (String (fingerprint email)) (KM.delete "query" request)))
    _ -> pure (Object request)
  _ -> pure (Object request)
  where
    canonicalEmail :: Text -> AppM Text
    canonicalEmail raw = do
      let email = T.toLower (T.strip raw)
          parts = T.splitOn "@" email
      unless (T.length email<=254 && not (T.any (\c -> c<=' ' || c=='\DEL') email)
        && case parts of [local,domain] -> not (T.null local) && T.any (=='.') domain && not (T.null domain); _ -> False)
        $ throwError (failure "invalid_request")
      pure email
privateCommand body = pure body

validEvent :: Int64 -> AppM ()
validEvent e = unless (e>0 && e<=9007199254740991) $ throwError (failure "invalid_request")
validToken :: Text -> AppM ()
validToken t = unless (T.length t==43 && T.all (\c -> c `elem`
  (['A'..'Z']++['a'..'z']++['0'..'9']++"_-")) t) $ throwError (failure "invitation_unavailable")
newSecret :: AppM Text
newSecret = liftIO $ T.dropWhileEnd (=='=') . TE.decodeUtf8 . B64.encode <$> getEntropy 32
fingerprint :: Text -> Text
fingerprint t = T.pack (show (hash (TE.encodeUtf8 t) :: Digest SHA256))
jsonValue :: Value -> PersistValue
jsonValue = PersistText . TE.decodeUtf8 . BL.toStrict . encode

runSession :: AuthedUser -> SqlPersistT IO a -> AppM a
runSession user action = do
  Env pool _ <- ask
  result <- liftIO $ tryAny (runSqlPool (withCurrentAuthSession user action) pool)
  case result of
    Left exception -> case fromException exception :: Maybe SqlError of
      Just sqlError | sqlState sqlError `elem` ["22023","22P02","22007","22008","22003"] -> throwError (failure "invalid_request")
      Just sqlError | sqlState sqlError `elem` ["23505","40001","40P01"] -> throwError (failure "transaction_conflict")
      _ -> throwError (failure "database_unavailable")
    Right Nothing -> throwError err401 {errBody="Invalid or inactive session"}
    Right (Just value) -> pure value

query :: AuthedUser -> Text -> [PersistValue] -> AppM Value
query user sql parameters = do
  rows <- runSession user (rawSql sql parameters :: SqlPersistT IO [Single Text])
  decodeRows rows

decodeRows :: [Single Text] -> AppM Value
decodeRows rows = do
  case rows of
    [Single raw] -> case eitherDecodeStrict' (TE.encodeUtf8 raw) of
      Right (Object result) -> case KM.lookup "error" result of
        Just (String code) -> throwError (failure code)
        Nothing -> pure (Object result)
        _ -> throwError (failure "database_unavailable")
      _ -> throwError (failure "database_unavailable")
    _ -> throwError (failure "database_unavailable")

private :: AppM Value -> AppM Result
private action = (addHeader ("private, no-store" :: Text) <$> action) `catchError`
  (\err -> throwError err {errHeaders=("Cache-Control","private, no-store"):errHeaders err})
failure :: Text -> ServerError
failure code = base {errBody=encode(object ["code" .= code]),errHeaders=[("Content-Type","application/json")]}
  where
    base | code `elem` ["not_found","invitation_unavailable"] = err404
         | code `elem` ["forbidden","recipient_verification_required"] = err403
         | code `elem` ["invalid_request","email_required"] = err400
         | code=="rate_limited" = err429
         | code `elem` ["database_unavailable","delivery_unavailable"] = err503
         | otherwise = err409
