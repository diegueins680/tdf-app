{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module TDF.Contracts.Server
  ( ContractsDeps (..)
  , defaultRenderPdf
  , server
  , loadContract
  , storedFromRecord
  , decodeStoredContract
  , decodeStoredContractFor
  , validateContractId
  , validateContractPayload
  , validateContractSendPayload
  ) where

import           Control.Exception (SomeException, displayException, try)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (fromMaybe)
import           Database.Persist (get, insert, insertKey, update, (=.))
import           Database.Persist.Sql (ConnectionPool, SqlPersistT, fromSqlKey, runSqlPool)
import           TDF.Models (PartyId)
import qualified TDF.ModelsExtra as ME
import qualified Data.Aeson as A
import           Data.Aeson ((.=), (.:))
import           Data.Aeson.Types (withObject)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isAsciiLower, isDigit, isSpace)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import           GHC.Generics (Generic)
import           Servant
import           System.Directory (doesFileExist)
import           System.FilePath ((</>))
import           TDF.Contracts.API
import qualified TDF.Handlers.InputList as InputList
import           Data.UUID (toText)
import qualified Data.UUID as UUID
import           Data.UUID.V4 (nextRandom)

data StoredContract = StoredContract
  { scId        :: Text
  , scKind      :: Text
  , scPayload   :: A.Value
  , scCreatedAt :: UTCTime
  } deriving (Show, Generic)

instance A.ToJSON StoredContract where
  toJSON StoredContract{..} = A.object
    [ "id" .= scId
    , "kind" .= scKind
    , "payload" .= scPayload
    , "created_at" .= scCreatedAt
    ]

instance A.FromJSON StoredContract where
  parseJSON = withObject "StoredContract" $ \o -> do
    case filter (`notElem` storedContractAllowedKeys) (map K.toText (KM.keys o)) of
      [] -> pure ()
      unexpected ->
        fail ("Unexpected StoredContract keys: " <> show unexpected)
    StoredContract
      <$> o .: "id"
      <*> o .: "kind"
      <*> o .: "payload"
      <*> o .: "created_at"
    where
      storedContractAllowedKeys =
        [ "id"
        , "kind"
        , "payload"
        , "created_at"
        ]

contractsDir :: FilePath
contractsDir = "contracts" </> "store"

contractPath :: Text -> FilePath
contractPath cid = contractsDir </> T.unpack cid <> ".json"

-- | What the contract handlers need from the application: the database, an
-- email sender when SMTP is configured, and the acting party.
data ContractsDeps = ContractsDeps
  { cdPool  :: ConnectionPool
  , cdEmail :: Maybe (Text -> Text -> Text -> BL.ByteString -> IO ())
    -- ^ recipient, kind, contract id, PDF; throws when SMTP refuses
  , cdActor :: Maybe PartyId
  , cdRenderPdf :: Text -> IO (Either Text BL.ByteString)
    -- ^ LaTeX to PDF; 'defaultRenderPdf' in production
  }

defaultRenderPdf :: Text -> IO (Either Text BL.ByteString)
defaultRenderPdf = InputList.generateInputListPdf

server :: ContractsDeps -> Server ContractsAPI
server deps = createH :<|> pdfH :<|> sendH
  where
    runDb :: SqlPersistT IO a -> Handler a
    runDb action = liftIO (runSqlPool action (cdPool deps))

    createH :: A.Value -> Handler A.Value
    createH payload = do
      (kindText, normalizedPayload) <- either throwError pure (validateContractPayload payload)
      uuid <- liftIO nextRandom
      now <- liftIO getCurrentTime
      let cid = toText uuid
      runDb $ insertKey (ME.ContractKey uuid) ME.Contract
        { ME.contractKind = kindText
        , ME.contractPayload = encodeContractPayload normalizedPayload
        , ME.contractCreatedByPartyId = cdActor deps
        , ME.contractCreatedAt = now
        }
      pure (A.object
        [ "status" .= ("created" :: Text)
        , "id" .= cid
        , "kind" .= kindText
        , "payload" .= normalizedPayload
        ])

    pdfH :: Text -> Handler BL.ByteString
    pdfH cid = do
      stored <- loadOr404 cid
      renderPdfOr500 stored

    -- CONTRACT-SEND-001: "sent" means SMTP accepted a message carrying the
    -- contract PDF. The delivery row is written before SMTP is contacted.
    sendH :: Text -> A.Value -> Handler A.Value
    sendH cid body = do
      recipientEmail <- either throwError pure (validateContractSendPayload body)
      stored <- loadOr404 cid
      sendEmail <- maybe
        (throwError err503 { errBody = "Contract email is not configured" })
        pure
        (cdEmail deps)
      pdf <- renderPdfOr500 stored
      requestedAt <- liftIO getCurrentTime
      deliveryKey <- runDb $ insert ME.ContractDelivery
        { ME.contractDeliveryContractId = contractKeyFor stored
        , ME.contractDeliveryRecipientEmail = recipientEmail
        , ME.contractDeliveryRequestedByPartyId = cdActor deps
        , ME.contractDeliveryRequestedAt = requestedAt
        , ME.contractDeliveryAcceptedAt = Nothing
        , ME.contractDeliveryFailedAt = Nothing
        , ME.contractDeliveryFailure = Nothing
        }
      outcome <- liftIO (try (sendEmail recipientEmail (scKind stored) (scId stored) pdf))
      finishedAt <- liftIO getCurrentTime
      case outcome of
        Left err -> do
          runDb $ update deliveryKey
            [ ME.ContractDeliveryFailedAt =. Just finishedAt
            , ME.ContractDeliveryFailure =. Just (T.take 300 (T.pack (displayException (err :: SomeException))))
            ]
          throwError err502 { errBody = "Contract email could not be sent" }
        Right () -> do
          runDb $ update deliveryKey [ME.ContractDeliveryAcceptedAt =. Just finishedAt]
          pure (A.object
            [ "status" .= ("sent" :: Text)
            , "id" .= scId stored
            , "email" .= recipientEmail
            , "deliveryId" .= fromSqlKey deliveryKey
            ])

    loadOr404 :: Text -> Handler StoredContract
    loadOr404 cid = do
      contractId <- either throwError pure (validateContractId cid)
      storedResult <- liftIO (loadContract (cdPool deps) contractId)
      case storedResult of
        Left errMsg -> throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 errMsg) }
        Right Nothing -> throwError err404 { errBody = "Contract not found" }
        Right (Just stored) -> pure stored

    renderPdfOr500 :: StoredContract -> Handler BL.ByteString
    renderPdfOr500 stored = do
      pdfResult <- liftIO (cdRenderPdf deps (renderContractLatex stored))
      either (\errMsg -> throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 errMsg) }) pure pdfResult

contractKeyFor :: StoredContract -> ME.ContractId
contractKeyFor stored = ME.ContractKey (fromMaybe UUID.nil (UUID.fromText (scId stored)))

encodeContractPayload :: A.Value -> Text
encodeContractPayload = TE.decodeUtf8 . BL.toStrict . A.encode

-- | Load a contract from the database. A contract that only exists in the
-- legacy file store (written before CONTRACT-STORE-001) is validated and
-- imported on first read, so contracts created before the migration survive.
loadContract :: ConnectionPool -> Text -> IO (Either Text (Maybe StoredContract))
loadContract pool cid =
  case UUID.fromText cid of
    Nothing -> pure (Left "Requested contract id is invalid")
    Just uuid -> do
      row <- runSqlPool (get (ME.ContractKey uuid)) pool
      case row of
        Just record -> pure (Just <$> storedFromRecord cid record)
        Nothing -> do
          legacy <- loadLegacyContract cid
          case legacy of
            Right (Just stored) -> do
              runSqlPool (importLegacy uuid stored) pool
              pure (Right (Just stored))
            other -> pure other
  where
    importLegacy uuid stored = do
      existing <- get (ME.ContractKey uuid)
      case existing of
        Just _ -> pure ()
        Nothing -> insertKey (ME.ContractKey uuid) ME.Contract
          { ME.contractKind = scKind stored
          , ME.contractPayload = encodeContractPayload (scPayload stored)
          , ME.contractCreatedByPartyId = Nothing
          , ME.contractCreatedAt = scCreatedAt stored
          }

storedFromRecord :: Text -> ME.Contract -> Either Text StoredContract
storedFromRecord cid record =
  case A.eitherDecodeStrict (TE.encodeUtf8 (ME.contractPayload record)) of
    Left _ -> Left "Stored contract payload is unreadable"
    Right payload -> validateStoredContract StoredContract
      { scId = cid
      , scKind = ME.contractKind record
      , scPayload = payload
      , scCreatedAt = ME.contractCreatedAt record
      }

loadLegacyContract :: Text -> IO (Either Text (Maybe StoredContract))
loadLegacyContract cid = do
  let path = contractPath cid
  exists <- doesFileExist path
  if not exists
    then pure (Right Nothing)
    else do
      bytes <- BL.readFile path
      pure (Just <$> decodeStoredContractFor cid bytes)

decodeStoredContract :: BL.ByteString -> Either Text StoredContract
decodeStoredContract bytes =
  case A.eitherDecode bytes of
    Left _ -> Left "Stored contract payload is unreadable"
    Right stored -> validateStoredContract stored

decodeStoredContractFor :: Text -> BL.ByteString -> Either Text StoredContract
decodeStoredContractFor rawExpectedId bytes = do
  expectedId <- validateRequestedContractId rawExpectedId
  stored <- decodeStoredContract bytes
  if scId stored == expectedId
    then Right stored
    else Left "Stored contract id does not match requested contract id"

validateStoredContract :: StoredContract -> Either Text StoredContract
validateStoredContract stored@StoredContract{..} = do
  contractId <- validateCanonicalStoredContractId scId
  storedKind <- validateCanonicalStoredContractKind scKind
  (payloadKind, normalizedPayload) <- firstServerErrorText (validateContractPayload scPayload)
  if scPayload /= normalizedPayload
    then Left "Stored contract payload is not canonical"
    else Right ()
  if storedKind /= payloadKind
    then Left "Stored contract kind does not match payload kind"
    else
      Right
        stored
          { scId = contractId
          , scKind = storedKind
          , scPayload = normalizedPayload
          }

validateCanonicalStoredContractId :: Text -> Either Text Text
validateCanonicalStoredContractId rawId = do
  contractId <- validateStoredContractId rawId
  if rawId == contractId
    then Right contractId
    else Left "Stored contract id is not canonical"

validateStoredContractId :: Text -> Either Text Text
validateStoredContractId rawId =
  case UUID.fromText (T.strip rawId) of
    Just uuid
      | isNilUuid uuid -> Left "Stored contract id is invalid"
      | otherwise -> Right (toText uuid)
    Nothing -> Left "Stored contract id is invalid"

validateRequestedContractId :: Text -> Either Text Text
validateRequestedContractId rawId =
  case validateStoredContractId rawId of
    Left _ -> Left "Requested contract id is invalid"
    Right contractId
      | rawId == contractId -> Right contractId
      | otherwise -> Left "Requested contract id is not canonical"

validateCanonicalStoredContractKind :: Text -> Either Text Text
validateCanonicalStoredContractKind rawKind = do
  kind <- validateStoredContractKind rawKind
  if rawKind == kind
    then Right kind
    else Left "Stored contract kind is not canonical"

validateStoredContractKind :: Text -> Either Text Text
validateStoredContractKind rawKind =
  case normalizeContractKind rawKind of
    Left _ -> Left "Stored contract kind is invalid"
    Right kindText -> Right kindText

firstServerErrorText :: Either ServerError a -> Either Text a
firstServerErrorText =
  either (Left . TE.decodeUtf8 . BL.toStrict . errBody) Right

validateContractId :: Text -> Either ServerError Text
validateContractId raw =
  case validateRequestedContractId raw of
    Right contractId -> Right contractId
    Left _ -> invalidContractId
  where
    invalidContractId =
      Left err400
        { errBody = "Invalid contract id"
        }

isNilUuid :: UUID.UUID -> Bool
isNilUuid uuid = toText uuid == "00000000-0000-0000-0000-000000000000"

validateContractSendPayload :: A.Value -> Either ServerError Text
validateContractSendPayload (A.Object payloadObj)
  | not (KM.null (KM.delete "email" payloadObj)) =
      Left err400
        { errBody = "Contract send payload only supports the email field"
        }
  | otherwise =
      case KM.lookup "email" payloadObj of
        Just (A.String rawEmail) ->
          case normalizeContractEmail rawEmail of
            Just email -> Right email
            Nothing -> invalidEmail
        _ -> invalidEmail
  where
    invalidEmail =
      Left err400
        { errBody = "Contract send payload must include a valid email"
        }
validateContractSendPayload _ =
  Left err400
    { errBody = "Contract send payload must be a JSON object"
    }

validateContractPayload :: A.Value -> Either ServerError (Text, A.Value)
validateContractPayload (A.Object payloadObj) =
  case reservedPayloadKey of
    Just reservedKey ->
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 ("Contract payload must not include server-managed field: " <> reservedKey))
        }
    Nothing ->
      case KM.lookup "kind" payloadObj of
        Nothing ->
          Left err400
            { errBody = "Contract payload must include a kind field"
            }
        Just (A.String rawKind) ->
          case normalizeContractKind rawKind of
            Left err -> Left err
            Right kindText ->
              let normalizedPayload = A.Object (KM.insert "kind" (A.String kindText) payloadObj)
              in do
                validateContractPayloadForPdf normalizedPayload
                Right (kindText, normalizedPayload)
        Just _ ->
          invalidKind
  where
    reservedPayloadKey =
      case filter (`KM.member` payloadObj) ["id", "created_at"] of
        [] -> Nothing
        (key:_) -> Just (K.toText key)
    invalidKind =
      Left err400
        { errBody = "Contract payload kind must be a non-empty slug using ASCII letters, numbers, hyphens, or underscores"
        }
validateContractPayload _ =
  Left err400
    { errBody = "Contract payload must be a JSON object"
    }

validateContractPayloadForPdf :: A.Value -> Either ServerError ()
validateContractPayloadForPdf payload
  | BL.length (A.encode payload) > fromIntegral maxContractPayloadBytes =
      Left err400
        { errBody =
            BL.fromStrict $
              TE.encodeUtf8 $
                "Contract payload must be "
                  <> T.pack (show maxContractPayloadBytes)
                  <> " bytes or fewer"
        }
  | containsLatexVerbatimTerminator payload =
      Left err400
        { errBody = "Contract payload text must not include the LaTeX verbatim terminator"
        }
  | otherwise =
      Right ()

maxContractPayloadBytes :: Int
maxContractPayloadBytes = 256 * 1024

containsLatexVerbatimTerminator :: A.Value -> Bool
containsLatexVerbatimTerminator (A.String value) =
  any isLatexVerbatimTerminatorAt (T.tails value)
containsLatexVerbatimTerminator (A.Object payloadObj) =
  any (containsLatexVerbatimTerminator . A.String . K.toText) (KM.keys payloadObj)
    || any containsLatexVerbatimTerminator (KM.elems payloadObj)
containsLatexVerbatimTerminator (A.Array payloadValues) =
  any containsLatexVerbatimTerminator payloadValues
containsLatexVerbatimTerminator _ =
  False

isLatexVerbatimTerminatorAt :: Text -> Bool
isLatexVerbatimTerminatorAt value =
  case T.stripPrefix "\\end" value of
    Just rest ->
      "{verbatim}" `T.isPrefixOf` T.dropWhile isSpace rest
    Nothing ->
      False

normalizeContractEmail :: Text -> Maybe Text
normalizeContractEmail rawEmail =
  let normalized = T.toLower (T.strip rawEmail)
  in if isValidContractEmail normalized then Just normalized else Nothing

isValidContractEmail :: Text -> Bool
isValidContractEmail candidate =
  case T.splitOn "@" candidate of
    [localPart, domain] ->
      T.length candidate <= 254
        && isValidContractEmailLocalPart localPart
        && not (T.any (`elem` [' ', '\t', '\n', '\r']) candidate)
        && isValidContractEmailDomain domain
    _ -> False

isValidContractEmailLocalPart :: Text -> Bool
isValidContractEmailLocalPart localPart =
  not (T.null localPart)
    && T.length localPart <= 64
    && not (T.isPrefixOf "." localPart)
    && not (T.isSuffixOf "." localPart)
    && not (".." `T.isInfixOf` localPart)
    && T.all isValidContractEmailLocalChar localPart

isValidContractEmailLocalChar :: Char -> Bool
isValidContractEmailLocalChar c =
  isAsciiLower c || isDigit c || c `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

isValidContractEmailDomain :: Text -> Bool
isValidContractEmailDomain domain =
  not (T.null domain)
    && T.isInfixOf "." domain
    && all isValidContractEmailDomainLabel labels
    && case reverse labels of
      finalLabel : _ -> isValidContractEmailFinalDomainLabel finalLabel
      [] -> False
  where
    labels = T.splitOn "." domain

isValidContractEmailDomainLabel :: Text -> Bool
isValidContractEmailDomainLabel label =
  not (T.null label)
    && T.length label <= 63
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidContractEmailDomainChar label

isValidContractEmailFinalDomainLabel :: Text -> Bool
isValidContractEmailFinalDomainLabel label =
  T.length label >= 2 && T.any isAsciiLower label

isValidContractEmailDomainChar :: Char -> Bool
isValidContractEmailDomainChar c = isAsciiLower c || isDigit c || c == '-'

normalizeContractKind :: Text -> Either ServerError Text
normalizeContractKind rawKind
  | T.null kindText =
      Left err400
        { errBody = "Contract payload kind must be a non-empty slug using ASCII letters, numbers, hyphens, or underscores"
        }
  | T.length kindText > maxContractKindLength =
      Left err400
        { errBody = "Contract payload kind must be 64 characters or fewer"
        }
  | not (T.any isKindMeaningfulChar kindText) =
      Left err400
        { errBody = "Contract payload kind must include at least one ASCII letter or number"
        }
  | not (T.all validKindChar kindText) =
      Left err400
        { errBody = "Contract payload kind must be a non-empty slug using ASCII letters, numbers, hyphens, or underscores"
        }
  | not (isKindMeaningfulChar (T.head kindText) && isKindMeaningfulChar (T.last kindText)) =
      Left err400
        { errBody = "Contract payload kind must start and end with an ASCII letter or number"
        }
  | otherwise =
      Right kindText
  where
    kindText = T.toLower (T.strip rawKind)
    isKindMeaningfulChar c = isAsciiLower c || isDigit c
    validKindChar c = isAsciiLower c || isDigit c || c == '-' || c == '_'

maxContractKindLength :: Int
maxContractKindLength = 64

renderContractLatex :: StoredContract -> Text
renderContractLatex StoredContract{..} =
  let createdTxt = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" scCreatedAt)
      lineBreak = " \\\\"
   in T.unlines
        [ "\\documentclass{article}"
        , "\\usepackage[margin=1in]{geometry}"
        , "\\usepackage{parskip}"
        , "\\begin{document}"
        , "\\section*{Contract " <> latexEscape scId <> "}"
        , "\\textbf{Kind:} " <> latexEscape scKind <> lineBreak
        , "\\textbf{Created:} " <> latexEscape createdTxt <> lineBreak
        , "\\subsection*{Payload}"
        , "\\begin{verbatim}"
        , TE.decodeUtf8 (BL.toStrict (A.encode scPayload))
        , "\\end{verbatim}"
        , "\\end{document}"
        ]

latexEscape :: Text -> Text
latexEscape = T.concatMap escapeChar
  where
    escapeChar c = case c of
      '&'  -> "\\&"
      '%'  -> "\\%"
      '$'  -> "\\$"
      '#'  -> "\\#"
      '_'  -> "\\_"
      '{'  -> "\\{"
      '}'  -> "\\}"
      '~'  -> "\\textasciitilde{}"
      '^'  -> "\\textasciicircum{}"
      '\\' -> "\\textbackslash{}"
      _    -> T.singleton c
