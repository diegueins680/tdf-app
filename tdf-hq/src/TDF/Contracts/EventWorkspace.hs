{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module TDF.Contracts.EventWorkspace (eventWorkspaceServer, workspaceDocumentLines, escapeDocumentText, workspacePdfResponse) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.UUID as UUID
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql, runSqlPool)
import Database.PostgreSQL.Simple (SqlError)
import Servant

import TDF.Auth (AuthedUser(..), withCurrentAuthSession)
import TDF.Contracts.EventWorkspaceAPI
import TDF.DB (Env(..))
import qualified TDF.Handlers.InputList as InputList

type AppM = ReaderT Env Handler

eventWorkspaceServer :: AuthedUser -> ServerT EventWorkspaceAPI AppM
eventWorkspaceServer user ev = readH :<|> writeH :<|> planPdf :<|> agreementPdf :<|> acceptancePdf
 where
  query :: Text -> [PersistValue] -> AppM A.Value
  query statement values = do
    unless (ev>0 && ev<=9223372036854775807) $ throwError err400{errBody="Evento inválido."}
    Env{..} <- ask
    response <- liftIO $ try $ runSqlPool (withCurrentAuthSession user $ do
      installed <- rawSql "SELECT to_regprocedure('event_workspace_read(bigint,bigint)') IS NOT NULL" [] :: SqlPersistT IO [Single Bool]
      if installed == [Single True] then rawSql statement values else pure []) envPool
    rows <- case response of
      Left (_ :: SqlError) -> throwError err503{errBody="No pudimos completar la operación. Conserva tus cambios y vuelve a intentarlo.",errHeaders=[("Cache-Control","no-store")]}
      Right Nothing -> throwError err401
      Right (Just value) -> pure (value :: [Single Text])
    case rows of
      [Single raw] -> case A.eitherDecodeStrict' (TE.encodeUtf8 raw) of
        Right value@(A.Object fields) -> case KM.lookup "error" fields of
          Nothing -> pure value
          Just (A.String message) -> do
            let status = case KM.lookup "status" fields >>= resultInt of
                  Just 400 -> err400
                  Just 403 -> err403
                  Just 404 -> err404
                  Just 409 -> err409
                  _ -> err503
            throwError status{errBody=BL.fromStrict (TE.encodeUtf8 message),errHeaders=[("Cache-Control","no-store")]}
          _ -> throwError err503
        _ -> throwError err503
      _ -> throwError err503{errBody="Los acuerdos del evento están pendientes de activar. El borrador se conserva en pantalla.",errHeaders=[("Cache-Control","no-store")]}
  args = [toPersistValue (auPartyId user), PersistInt64 (fromInteger ev)]
  readValue :: AppM A.Value
  readValue = query "SELECT event_workspace_read(?,?)::text" args
  readH :: AppM WorkspaceJSON
  readH = addHeader ("private, no-store" :: Text) <$> readValue
  writeH :: A.Value -> AppM WorkspaceJSON
  writeH payload = do
    unless (BL.length (A.encode payload)<=262144) $ throwError err400{errBody="El documento supera el tamaño permitido."}
    value <- query "SELECT event_workspace_mutate(?,?,?::jsonb)::text" (args<>[PersistText (TE.decodeUtf8 (BL.toStrict (A.encode payload)))])
    pure (addHeader ("private, no-store" :: Text) value)
  planPdf = do
    state <- readValue
    plan <- requireField "plan" state
    content <- requireField "content" plan
    renderPdf "plan-evento.pdf" (workspaceDocumentLines content <> metadataLines plan)
  agreementPdf agreementId revision = do
    unless (revision>0) $ throwError err400
    state <- readValue
    agreement <- findAgreement agreementId state
    version <- maybe (throwError err404) pure $ F.find (\v -> (field "version" v >>= resultInt) == Just revision) (arrayField "versions" agreement)
    content <- requireField "content" version
    renderPdf "acuerdo-evento.pdf" (agreementLines content <> ["Metadatos de la propuesta"] <> metadataLines version)
  acceptancePdf agreementId = do
    state <- readValue
    agreement <- findAgreement agreementId state
    acceptance <- requireField "acceptance" agreement
    revision <- maybe (throwError err404) pure (field "version" acceptance >>= resultInt)
    version <- maybe (throwError err404) pure $ F.find (\v -> (field "version" v >>= resultInt) == Just revision) (arrayField "versions" agreement)
    content <- requireField "content" version
    renderPdf "constancia-aceptacion.pdf" $
      ["Constancia de aceptación del acuerdo", "Evento: "<>T.pack(show ev), "Acuerdo: "<>UUID.toText agreementId
      ,"Versión: "<>T.pack(show revision),"Cuenta aceptante: "<>textField "accepted_by" acceptance
      ,"Nombre: "<>textField "accepted_name" acceptance,"Fecha UTC: "<>documentTimestamp (textField "accepted_at" acceptance)
      ] <> hashLines acceptance <>
      ["Aceptación registrada desde la cuenta TDF del destinatario.", ""] <> agreementLines content <> ["Metadatos de la propuesta"] <> metadataLines version
  renderPdf :: Text -> [Text] -> AppM WorkspacePDF
  renderPdf name linesValue = lift (workspacePdfResponse InputList.generateInputListPdf name linesValue)
  findAgreement :: UUID.UUID -> A.Value -> AppM A.Value
  findAgreement uuid state = maybe (throwError err404) pure $ F.find (\a -> textField "id" a == UUID.toText uuid) (arrayField "agreements" state)
  requireField :: Text -> A.Value -> AppM A.Value
  requireField key value = case field key value of Just found | found/=A.Null -> pure found; _ -> throwError err404

-- The injectable boundary is shared by the real workspace handlers and isolated
-- tests. It performs no record lookup or authorization bypass.
workspacePdfResponse :: (Text -> IO (Either Text BL.ByteString)) -> Text -> [Text] -> Handler WorkspacePDF
workspacePdfResponse renderer name linesValue = do
  let latex = T.unlines (["\\documentclass[a4paper,11pt]{article}","\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}","\\usepackage[margin=22mm]{geometry}","\\usepackage{parskip}","\\begin{document}"]
         <> map (\line -> escapeDocumentText line<>"\\par") linesValue <> ["\\end{document}"])
  rendered <- liftIO (try (renderer latex))
  let result = case rendered of
        Left (_ :: IOException) -> Left "PDF renderer unavailable"
        Right value -> value
  case result of
    Left _ -> throwError err503{errBody="No se pudo generar el PDF. El acuerdo permanece guardado; reintenta la descarga.",errHeaders=[("Cache-Control","no-store")]}
    Right pdf -> pure (addHeader ("private, no-store" :: Text) (addHeader ("attachment; filename=\""<>name<>"\"" :: Text) pdf))

field :: Text -> A.Value -> Maybe A.Value
field key (A.Object fields) = KM.lookup (K.fromText key) fields
field _ _ = Nothing
textField :: Text -> A.Value -> Text
textField key value = case field key value of
 Just (A.String text) -> text
 Just number@(A.Number _) -> case A.fromJSON number :: A.Result Integer of A.Success n -> T.pack(show n); A.Error _ -> TE.decodeUtf8 (BL.toStrict (A.encode number))
 _ -> ""
resultInt :: A.Value -> Maybe Int
resultInt value = case A.fromJSON value of A.Success n -> Just n; A.Error _ -> Nothing
arrayField :: Text -> A.Value -> [A.Value]
arrayField key value = case field key value of Just (A.Array values) -> F.toList values; _ -> []

metadataLines :: A.Value -> [Text]
metadataLines value = ["Versión: "<>textField "version" value<>textField "revision" value]
 <> hashLines value <> ["Fecha UTC: "<>documentTimestamp (textField "created_at" value)]
hashLines :: A.Value -> [Text]
hashLines value = ["Huella SHA-256 (unir ambas líneas):"] <> T.chunksOf 32 (textField "document_hash" value)
documentTimestamp :: Text -> Text
documentTimestamp raw = maybe raw (T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC") (iso8601ParseM (T.unpack raw) :: Maybe UTCTime)
agreementLines :: A.Value -> [Text]
agreementLines value = [textField "title" value]<>T.lines(textField "body" value)
 <>maybe [] (\plan -> ["Anexo: plan del evento"] <> workspaceDocumentLines (fromMaybe A.Null (field "content" plan)) <> ["Metadatos del plan anexo"] <> metadataLines plan) (field "plan" value)

moneyCents :: A.Value -> Text
moneyCents value = case A.fromJSON value :: A.Result Integer of
 A.Success cents -> T.pack(show (cents `div` 100))<>"."<>T.justifyRight 2 '0' (T.pack(show (cents `mod` 100)))
 A.Error _ -> "Por cotizar"

workspaceDocumentLines :: A.Value -> [Text]
workspaceDocumentLines value = [textField "title" value]<>T.lines(textField "overview" value)
 <>["Cronograma y responsables"]<>concatMap (\a->[textField "start" a<>" - "<>textField "end" a<>" | "<>textField "title" a,"Responsable: "<>textField "responsible" a,textField "notes" a]) (arrayField "activities" value)
 <>["Presupuesto ("<>textField "currency" value<>")"]<>map (\b->textField "label" b<>" | "<>case field "amountCents" b of
      Just amount@(A.Number _) -> moneyCents amount<>" | "<>textField "basis" b
      _ -> "Por cotizar | "<>textField "basis" b) (arrayField "budget" value)
 <>["Máximo recuperable ("<>textField "currency" value<>"): "<>case field "budgetCapCents" value of Just A.Null->"Pendiente de aprobación"; Just amount->moneyCents amount; _->"Pendiente de aprobación"]
 <>["Entregables"]<>map valueText (arrayField "deliverables" value)
 <>["Pendientes"]<>map valueText (arrayField "pending" value)
 where valueText (A.String text)=text; valueText _=""

escapeDocumentText :: Text -> Text
escapeDocumentText = T.concatMap escape
 where
  escape '\\'="\\textbackslash{}"
  escape '{'="\\{"
  escape '}'="\\}"
  escape '$'="\\$"
  escape '&'="\\&"
  escape '%'="\\%"
  escape '#'="\\#"
  escape '_'="\\_"
  escape '^'="\\textasciicircum{}"
  escape '~'="\\textasciitilde{}"
  escape ch=T.singleton ch
