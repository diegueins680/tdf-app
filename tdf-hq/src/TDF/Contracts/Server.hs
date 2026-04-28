{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module TDF.Contracts.Server
  ( server
  , decodeStoredContract
  , decodeStoredContractFor
  , validateContractId
  , validateContractPayload
  , validateContractSendPayload
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import           Data.Aeson ((.=), (.:))
import           Data.Aeson.Types (withObject)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isAsciiLower, isDigit)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import           GHC.Generics (Generic)
import           Servant
import           System.Directory (createDirectoryIfMissing, doesFileExist)
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

server :: Server ContractsAPI
server = createH :<|> pdfH :<|> sendH
  where
    createH :: A.Value -> Handler A.Value
    createH payload = do
      cid <- liftIO (toText <$> nextRandom)
      now <- liftIO getCurrentTime
      (kindText, normalizedPayload) <- either throwError pure (validateContractPayload payload)
      let
          stored = StoredContract
            { scId = cid
            , scKind = kindText
            , scPayload = normalizedPayload
            , scCreatedAt = now
            }
      liftIO (persistContract stored)
      pure (A.object
        [ "status" .= ("created" :: Text)
        , "id" .= cid
        , "kind" .= kindText
        , "payload" .= normalizedPayload
        ])

    pdfH :: Text -> Handler BL.ByteString
    pdfH cid = do
      contractId <- either throwError pure (validateContractId cid)
      storedResult <- liftIO (loadContract contractId)
      case storedResult of
        Left errMsg ->
          throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 errMsg) }
        Right Nothing -> throwError err404 { errBody = "Contract not found" }
        Right (Just stored) -> do
          let latex = renderContractLatex stored
          pdfResult <- liftIO (InputList.generateInputListPdf latex)
          case pdfResult of
            Left errMsg -> throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 errMsg) }
            Right pdf -> pure pdf

    sendH :: Text -> A.Value -> Handler A.Value
    sendH cid body = do
      contractId <- either throwError pure (validateContractId cid)
      recipientEmail <- either throwError pure (validateContractSendPayload body)
      storedResult <- liftIO (loadContract contractId)
      case storedResult of
        Left errMsg ->
          throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 errMsg) }
        Right Nothing -> throwError err404 { errBody = "Contract not found" }
        Right (Just _)  ->
          pure (A.object ["status" .= ("sent" :: Text), "id" .= contractId, "email" .= recipientEmail])

persistContract :: StoredContract -> IO ()
persistContract stored = do
  createDirectoryIfMissing True contractsDir
  BL.writeFile (contractPath (scId stored)) (A.encode stored)

loadContract :: Text -> IO (Either Text (Maybe StoredContract))
loadContract cid = do
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
  expectedId <- validateStoredContractId rawExpectedId
  stored <- decodeStoredContract bytes
  if scId stored == expectedId
    then Right stored
    else Left "Stored contract id does not match requested contract id"

validateStoredContract :: StoredContract -> Either Text StoredContract
validateStoredContract stored@StoredContract{..} = do
  contractId <- validateStoredContractId scId
  storedKind <- validateStoredContractKind scKind
  (payloadKind, normalizedPayload) <- firstServerErrorText (validateContractPayload scPayload)
  if storedKind /= payloadKind
    then Left "Stored contract kind does not match payload kind"
    else
      Right
        stored
          { scId = contractId
          , scKind = storedKind
          , scPayload = normalizedPayload
          }

validateStoredContractId :: Text -> Either Text Text
validateStoredContractId rawId =
  case UUID.fromText (T.strip rawId) of
    Just uuid -> Right (toText uuid)
    Nothing -> Left "Stored contract id is invalid"

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
  case UUID.fromText (T.strip raw) of
    Just uuid -> Right (toText uuid)
    Nothing ->
      Left err400
        { errBody = "Invalid contract id"
        }

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
  | containsLatexVerbatimTerminator payload =
      Left err400
        { errBody = "Contract payload text must not include the LaTeX verbatim terminator"
        }
  | otherwise =
      Right ()

containsLatexVerbatimTerminator :: A.Value -> Bool
containsLatexVerbatimTerminator (A.String value) =
  "\\end{verbatim}" `T.isInfixOf` value
containsLatexVerbatimTerminator (A.Object payloadObj) =
  any containsLatexVerbatimTerminator (KM.elems payloadObj)
containsLatexVerbatimTerminator (A.Array payloadValues) =
  any containsLatexVerbatimTerminator payloadValues
containsLatexVerbatimTerminator _ =
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
        && not (T.null domain)
        && not (T.any (`elem` [' ', '\t', '\n', '\r']) candidate)
        && T.isInfixOf "." domain
        && all isValidContractEmailDomainLabel (T.splitOn "." domain)
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

isValidContractEmailDomainLabel :: Text -> Bool
isValidContractEmailDomainLabel label =
  not (T.null label)
    && T.length label <= 63
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidContractEmailDomainChar label

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
  | T.all validKindChar kindText =
      Right kindText
  | otherwise =
      Left err400
        { errBody = "Contract payload kind must be a non-empty slug using ASCII letters, numbers, hyphens, or underscores"
        }
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
