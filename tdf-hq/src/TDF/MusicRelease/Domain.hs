{-# LANGUAGE OverloadedStrings #-}

module TDF.MusicRelease.Domain
  ( ReleaseState(..)
  , ReleaseGates(..)
  , TransitionContext(..)
  , transitionRelease
  , RightsScope(..)
  , splitTotalValid
  , IdentifierType(..)
  , IdentifierError(..)
  , validateIdentifier
  , validateMoney
  , ListeningPolicy(..)
  , TerritoryMode(..)
  , AccessRule(..)
  , AccessDecision(..)
  , decidePlaybackAccess
  , PlaybackEvidence(..)
  , eligiblePlay
  , basicFraudFlags
  , DdexCompatibility(..)
  , supportedDdexCompatibility
  , validateDdexExportPrerequisites
  , CorrectionFailure(..)
  , classifyCorrectionFailure
  ) where

import Data.Char (digitToInt, intToDigit, isAlphaNum, isAscii, isDigit, isLetter, toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)

-- Only known correction failures become client errors. Never expose arbitrary
-- database messages, object locators, SQL details or authority evidence.
data CorrectionFailure = CorrectionFailure
  { correctionFailureStatus :: Int
  , correctionFailureCode :: Text
  , correctionFailureField :: Text
  , correctionFailureMessage :: Text
  } deriving (Eq, Show)

classifyCorrectionFailure :: Text -> Text -> Maybe CorrectionFailure
classifyCorrectionFailure state message
  | state == "23514"
  , message == "correction asset graph has a cycle or a parent outside the source resources" =
      Just (CorrectionFailure 422 "correction_resource_graph_invalid" "assets"
        "Los recursos de la versión de origen tienen un ciclo o un padre externo. Solicita a soporte revisar su procedencia antes de reintentar; no se creó ninguna corrección.")
  | state == "23514"
  , "correction asset " `T.isPrefixOf` message
  , " references a recording outside the source version" `T.isSuffixOf` message =
      Just (CorrectionFailure 422 "correction_recording_reference_invalid" "assets.recordingId"
        "Un recurso apunta a una grabación ajena a la versión de origen. Solicita a soporte revisar esa relación; no se creó ninguna corrección.")
  | state == "23514"
  , message == "source music release version is not an immutable correction source" =
      Just (CorrectionFailure 409 "correction_source_unavailable" "sourceVersionId"
        "La versión de origen ya no admite correcciones. Actualiza el lanzamiento y selecciona una versión aprobada.")
  | state `elem` ["40001", "40P01"] =
      Just (CorrectionFailure 409 "correction_retry_required" "sourceVersionId"
        "Otra operación modificó el lanzamiento. Reintenta con la misma clave de idempotencia; esta operación se revirtió.")
  | otherwise = Nothing

data ReleaseState
  = Draft
  | Uploading
  | Processing
  | ValidationFailed
  | ReadyForReview
  | InReview
  | ChangesRequested
  | Approved
  | Scheduled
  | Published
  | Suspended
  | Cancelled
  | ReplacementPending
  | TakedownScheduled
  | Withdrawn
  deriving (Bounded, Enum, Eq, Ord, Show)

data ReleaseGates = ReleaseGates
  { metadataValid :: Bool
  , assetsValid :: Bool
  , rightsValid :: Bool
  , accessValid :: Bool
  , hasTracks :: Bool
  , hasMainArtist :: Bool
  , hasMasterRights :: Bool
  , hasCompositionRights :: Bool
  , authorityTermsAccepted :: Bool
  , openChangeRequests :: Int
  } deriving (Eq, Show)

data TransitionContext = TransitionContext
  { transitionNow :: UTCTime
  , transitionGates :: ReleaseGates
  , reviewerAuthorized :: Bool
  , scheduledReleaseAt :: Maybe UTCTime
  , originalTimeZone :: Maybe Text
  , immutableSnapshotSha256 :: Maybe Text
  } deriving (Eq, Show)

transitionRelease
  :: TransitionContext
  -> ReleaseState
  -> ReleaseState
  -> Either Text ReleaseState
transitionRelease context current next
  | current == next = Right next
  | (current, next) `notElem` allowedTransitions =
      Left ("Transición editorial inválida: " <> stateName current <> " → " <> stateName next <> ".")
  | next `elem` submissionGatedStates
      , Just problem <- firstGateProblem (transitionGates context) = Left problem
  | next == Approved && not (reviewerAuthorized context) =
      Left "La aprobación final requiere personal autorizado de TDF."
  | next == Scheduled && not (reviewerAuthorized context) =
      Left "La programación requiere personal autorizado de TDF."
  | next == Scheduled
      , Nothing <- scheduledReleaseAt context =
          Left "Selecciona una fecha y hora de publicación."
  | next == Scheduled
      , Just at <- scheduledReleaseAt context
      , at <= transitionNow context =
          Left "La fecha programada debe estar en el futuro."
  | next == Scheduled
      , maybe True T.null (T.strip <$> originalTimeZone context) =
          Left "Conserva la zona horaria original de la fecha programada."
  | next == Published
      , Just at <- scheduledReleaseAt context
      , at > transitionNow context =
          Left "El embargo sigue vigente; todavía no se puede publicar."
  | next == Published
      , not (maybe False validSha256 (immutableSnapshotSha256 context)) =
          Left "La publicación requiere un snapshot inmutable con SHA-256."
  | otherwise = Right next

submissionGatedStates :: [ReleaseState]
submissionGatedStates =
  [ ReadyForReview, InReview, Approved, Scheduled, Published ]

firstGateProblem :: ReleaseGates -> Maybe Text
firstGateProblem gates
  | not (metadataValid gates) = Just "Completa y valida los metadatos editoriales."
  | not (assetsValid gates) = Just "Carga y procesa los másteres y la portada."
  | not (rightsValid gates) = Just "Corrige las declaraciones y splits de derechos."
  | not (accessValid gates) = Just "Configura escucha, territorios, compra y descarga."
  | not (hasTracks gates) = Just "Añade al menos una pista."
  | not (hasMainArtist gates) = Just "Añade al menos un artista principal."
  | not (hasMasterRights gates) = Just "Declara por separado los derechos de máster."
  | not (hasCompositionRights gates) = Just "Declara por separado los derechos de composición."
  | not (authorityTermsAccepted gates) = Just "Acepta la declaración de autoridad para publicar."
  | openChangeRequests gates > 0 = Just "Resuelve todas las solicitudes de cambio abiertas."
  | otherwise = Nothing

allowedTransitions :: [(ReleaseState, ReleaseState)]
allowedTransitions =
  [ (Draft, Uploading), (Draft, Processing), (Draft, ValidationFailed), (Draft, ReadyForReview), (Draft, Cancelled)
  , (Uploading, Processing), (Uploading, ValidationFailed), (Uploading, Draft), (Uploading, Cancelled)
  , (Processing, ReadyForReview), (Processing, ValidationFailed), (Processing, Cancelled)
  , (ValidationFailed, Draft), (ValidationFailed, Uploading), (ValidationFailed, Processing), (ValidationFailed, ReadyForReview), (ValidationFailed, Cancelled)
  , (ReadyForReview, InReview), (ReadyForReview, ValidationFailed), (ReadyForReview, Draft), (ReadyForReview, Cancelled)
  , (InReview, ChangesRequested), (InReview, Approved), (InReview, Suspended)
  , (ChangesRequested, Draft), (ChangesRequested, Uploading), (ChangesRequested, Processing), (ChangesRequested, ReadyForReview), (ChangesRequested, Cancelled)
  , (Approved, Scheduled), (Approved, ReplacementPending), (Approved, Suspended), (Approved, Cancelled)
  , (Scheduled, Published), (Scheduled, Approved), (Scheduled, Suspended), (Scheduled, Cancelled)
  , (Published, Suspended), (Published, ReplacementPending), (Published, TakedownScheduled)
  , (Suspended, Approved), (Suspended, TakedownScheduled), (Suspended, Withdrawn)
  , (ReplacementPending, Published), (ReplacementPending, TakedownScheduled)
  , (TakedownScheduled, Published), (TakedownScheduled, Withdrawn)
  ]

stateName :: ReleaseState -> Text
stateName = T.pack . show

data RightsScope = MasterRights | CompositionRights
  deriving (Bounded, Enum, Eq, Ord, Show)

splitTotalValid :: [(RightsScope, Int)] -> Bool
splitTotalValid splits =
  all validShare splits
    && scopeTotal MasterRights splits == 10000
    && scopeTotal CompositionRights splits == 10000
  where
    validShare (_, share) = share > 0 && share <= 10000
    scopeTotal scope = sum . map snd . filter ((== scope) . fst)

data IdentifierType
  = ISRC
  | UPC
  | EAN
  | GRid
  | ISNI
  | IPI
  | DPID
  | Proprietary
  deriving (Bounded, Enum, Eq, Ord, Show)

data IdentifierError = IdentifierError
  { identifierErrorCode :: Text
  , identifierErrorMessage :: Text
  } deriving (Eq, Show)

validateIdentifier :: IdentifierType -> Text -> Either IdentifierError Text
validateIdentifier identifierType raw =
  let trimmed = T.strip raw
      compact = T.toUpper (T.filter (`notElem` ['-', ' ']) trimmed)
      invalid code message = Left (IdentifierError code message)
  in case identifierType of
      ISRC
        | T.length compact == 12
          && T.all isAsciiAlpha (T.take 2 compact)
          && T.all isAsciiAlphaNum (T.take 3 (T.drop 2 compact))
          && T.all isDigit (T.drop 5 compact) -> Right compact
        | otherwise -> invalid "invalid_isrc" "El ISRC debe contener 12 caracteres: país, registrante, año y designación."
      UPC
        | validGtin 12 compact -> Right compact
        | otherwise -> invalid "invalid_upc" "El UPC debe contener 12 dígitos y un dígito verificador válido."
      EAN
        | validGtin 13 compact -> Right compact
        | otherwise -> invalid "invalid_ean" "El EAN debe contener 13 dígitos y un dígito verificador válido."
      GRid
        | T.length compact == 18 && T.all isAsciiAlphaNum compact -> Right compact
        | otherwise -> invalid "invalid_grid" "El GRid debe contener 18 caracteres alfanuméricos (se permiten guiones de presentación)."
      ISNI
        | validIsni compact -> Right compact
        | otherwise -> invalid "invalid_isni" "El ISNI debe contener 16 caracteres y un dígito verificador MOD 11-2 válido."
      IPI
        | T.length compact == 11 && T.all isDigit compact -> Right compact
        | otherwise -> invalid "invalid_ipi" "El IPI Name Number debe contener 11 dígitos."
      DPID
        | Just suffix <- T.stripPrefix "PADPIDA" trimmed
        , not (T.null suffix)
        , T.all isAsciiAlphaNum suffix -> Right trimmed
        | otherwise -> invalid "invalid_dpid" "El DPID debe usar la forma PADPIDA seguida de caracteres alfanuméricos, sin separadores."
      Proprietary
        | T.null trimmed -> invalid "empty_identifier" "El identificador no puede estar vacío."
        | T.length trimmed > 200 -> invalid "identifier_too_long" "El identificador no puede superar 200 caracteres."
        | otherwise -> Right trimmed

validGtin :: Int -> Text -> Bool
validGtin expectedLength value
  | T.length value /= expectedLength || not (T.all isDigit value) = False
  | otherwise =
      case reverse (map digitToInt (T.unpack value)) of
        [] -> False
        supplied : reversedBody ->
          let weighted = sum (zipWith (*) reversedBody (cycle [3, 1]))
              expected = (10 - weighted `mod` 10) `mod` 10
          in supplied == expected

validIsni :: Text -> Bool
validIsni value
  | T.length value /= 16 = False
  | not (T.all isDigit (T.take 15 value)) = False
  | otherwise =
      let total = foldl' (\acc char -> ((acc + digitToInt char) * 2) `mod` 11) 0 (T.unpack (T.take 15 value))
          expectedValue = (12 - total) `mod` 11
          expected = if expectedValue == 10 then 'X' else intToDigit expectedValue
      in T.last value == expected

isAsciiAlpha :: Char -> Bool
isAsciiAlpha char = isAscii char && isLetter char

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum char = isAscii char && isAlphaNum char

validateMoney :: [Text] -> Bool -> Text -> Integer -> Either Text (Text, Integer)
validateMoney supportedCurrencies requirePositive rawCurrency minorUnits
  | not (T.length currency == 3 && T.all isAsciiAlpha currency) =
      Left "La moneda debe ser un código ISO 4217 de tres letras."
  | currency `notElem` map (T.map toUpper . T.strip) supportedCurrencies =
      Left "La moneda no está habilitada para este entorno."
  | requirePositive && minorUnits <= 0 = Left "El precio debe ser mayor que cero."
  | not requirePositive && minorUnits < 0 = Left "El monto no puede ser negativo."
  | otherwise = Right (currency, minorUnits)
  where
    currency = T.map toUpper (T.strip rawCurrency)

data ListeningPolicy = NoListening | PreviewListening | FullListening
  deriving (Bounded, Enum, Eq, Ord, Show)

data TerritoryMode = IncludeTerritories | ExcludeTerritories
  deriving (Bounded, Enum, Eq, Ord, Show)

data AccessRule = AccessRule
  { accessTerritoryMode :: TerritoryMode
  , accessTerritories :: [Text]
  , accessStartsAt :: Maybe UTCTime
  , accessEndsAt :: Maybe UTCTime
  , accessListeningPolicy :: ListeningPolicy
  } deriving (Eq, Show)

data AccessDecision
  = AccessDenied Text
  | AccessPreview
  | AccessFull
  deriving (Eq, Show)

decidePlaybackAccess :: UTCTime -> Text -> AccessRule -> AccessDecision
decidePlaybackAccess now territory rule
  | maybe False (> now) (accessStartsAt rule) = AccessDenied "El contenido todavía no está disponible."
  | maybe False (<= now) (accessEndsAt rule) = AccessDenied "El periodo de disponibilidad terminó."
  | not (territoryAllowed territory rule) = AccessDenied "El contenido no está disponible en tu territorio."
  | accessListeningPolicy rule == NoListening = AccessDenied "La escucha no está autorizada."
  | accessListeningPolicy rule == PreviewListening = AccessPreview
  | otherwise = AccessFull

territoryAllowed :: Text -> AccessRule -> Bool
territoryAllowed rawTerritory rule =
  let territory = T.toUpper (T.strip rawTerritory)
      configured = map (T.toUpper . T.strip) (accessTerritories rule)
      matches = "WORLDWIDE" `elem` configured || territory `elem` configured
  in case accessTerritoryMode rule of
      IncludeTerritories -> matches
      ExcludeTerritories -> not matches

data PlaybackEvidence = PlaybackEvidence
  { trackDurationMs :: Integer
  , cumulativeListenedMs :: Integer
  , maxContinuousListenedMs :: Integer
  , wallClockElapsedMs :: Integer
  , playbackRate :: Double
  , seekCount :: Int
  , startsInWindow :: Int
  , automationDetected :: Bool
  } deriving (Eq, Show)

-- TDF business metric, not a royalty-accounting rule: 30 seconds of actual
-- listening, or 80% of a recording shorter than 30 seconds. Continuous audio
-- evidence is required and time gained through seeking is excluded upstream.
eligiblePlay :: PlaybackEvidence -> Bool
eligiblePlay evidence =
  not (automationDetected evidence)
    && trackDurationMs evidence > 0
    && cumulativeListenedMs evidence >= threshold
    && maxContinuousListenedMs evidence >= min threshold 10000
    && wallClockElapsedMs evidence >= threshold
  where
    threshold
      | trackDurationMs evidence < 30000 = max 1000 ((trackDurationMs evidence * 8) `div` 10)
      | otherwise = 30000

basicFraudFlags :: PlaybackEvidence -> [Text]
basicFraudFlags evidence = concat
  [ ["automation_signal" | automationDetected evidence]
  , ["impossible_listen_time" | cumulativeListenedMs evidence > wallClockElapsedMs evidence + 2000]
  , ["unsupported_playback_rate" | playbackRate evidence < 0.5 || playbackRate evidence > 2.0]
  , ["seek_burst" | seekCount evidence > 20]
  , ["start_burst" | startsInWindow evidence > 30]
  ]

data DdexCompatibility = DdexCompatibility
  { ernVersion :: Text
  , releaseProfile :: Text
  , releaseProfileVersion :: Text
  , businessProfileVersion :: Maybe Text
  , allowedValueSetsVersion :: Text
  , structuralDataDictionary :: Text
  , choreography :: Text
  , choreographyVersion :: Text
  } deriving (Eq, Show)

supportedDdexCompatibility :: DdexCompatibility
supportedDdexCompatibility = DdexCompatibility
  { ernVersion = "4.3.2"
  , releaseProfile = "Audio"
  , releaseProfileVersion = "2.3.1"
  , businessProfileVersion = Nothing
  , allowedValueSetsVersion = "011"
  , structuralDataDictionary = "DD-ERN-432"
  , choreography = "Cloud Storage"
  , choreographyVersion = "1.8.1"
  }

validateDdexExportPrerequisites
  :: Maybe Text
  -> Maybe Text
  -> [(IdentifierType, Text)]
  -> Either [Text] ()
validateDdexExportPrerequisites sender recipient identifiers =
  case concat
    [ validateDpid "senderDpid" sender
    , validateDpid "recipientDpid" recipient
    , ["Cada grabación exportada requiere un ISRC proporcionado y sintácticamente válido."
      | not (any validIsrc identifiers)]
    , ["El release principal requiere UPC, EAN o GRid proporcionado y sintácticamente válido."
      | not (any validReleaseId identifiers)]
    ] of
      [] -> Right ()
      errors -> Left errors
  where
    validateDpid field value = case value of
      Nothing -> [field <> ": proporciona un DPID real; TDF no genera identificadores oficiales."]
      Just candidate -> case validateIdentifier DPID candidate of
        Left err -> [field <> ": " <> identifierErrorMessage err]
        Right _ -> []
    validIsrc (ISRC, candidate) = either (const False) (const True) (validateIdentifier ISRC candidate)
    validIsrc _ = False
    validReleaseId (kind, candidate) =
      kind `elem` [UPC, EAN, GRid]
        && either (const False) (const True) (validateIdentifier kind candidate)

validSha256 :: Text -> Bool
validSha256 value =
  T.length value == 64
    && T.all (\character -> isDigit character || character `elem` ['a'..'f']) value
