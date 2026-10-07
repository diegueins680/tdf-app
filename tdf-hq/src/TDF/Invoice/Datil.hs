{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Ecuador electronic invoices for paid ticket orders through Dátil, an
-- authorized provider that signs (XAdES-BES) and submits to the SRI. TDF owns the
-- sequential number and the access key, so a retried submission carries the same
-- SRI identity. A submission whose outcome is unknown is never resent blindly; it
-- becomes 'uncertain' for reconciliation in the provider dashboard.
module TDF.Invoice.Datil
  ( DatilConfig(..)
  , InvoiceInput(..)
  , InvoiceLine(..)
  , BuyerIdentity(..)
  , loadDatilConfig
  , datilEnvironmentFor
  , invoicingReady
  , invoicePayload
  , creditNotePayload
  , ModifiedInvoice(..)
  , paymentMedium
  , interpretDatilDocument
  , DatilOutcome(..)
  , processNextTaxDocument
  , startTaxInvoiceWorker
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception (evaluate)
import           Control.Exception.Safe (tryAny)
import           Control.Monad (forever, void, when)
import           Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isDigit)
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Scientific (Scientific, scientific)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time
  ( Day, UTCTime, addUTCTime, defaultTimeLocale, getCurrentTime, hoursToTimeZone
  , localDay, localTimeToUTC, parseTimeM, utcToLocalTime )
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Database.Persist.Sql
  ( ConnectionPool, PersistValue(..), Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool )
import           Network.HTTP.Client
  ( Request(..), RequestBody(..), brRead, parseRequest, responseBody
  , responseStatus, responseTimeoutMicro, withResponse )
import           Network.HTTP.Types.Status (statusCode)
import           System.Environment (lookupEnv)
import           System.IO (hPutStrLn, stderr)
import           System.Timeout (timeout)

import           TDF.Commerce.ProviderAdapter.Http (sharedProviderManager)
import           TDF.DB (Env(..))
import           TDF.Invoice.AccessKey

data DatilConfig = DatilConfig
  { dcApiKey              :: Text
  , dcCertificatePassword :: Text
  , dcEnvironment         :: Int   -- ^ 1 pruebas, 2 producción
  , dcRuc                 :: Text
  , dcLegalName           :: Text
  , dcTradeName           :: Text
  , dcAddress             :: Text
  , dcEstablishmentAddress :: Text
  , dcAccountingRequired  :: Bool
  , dcSpecialTaxpayer     :: Maybe Text
  }

-- | All values are server secrets/configuration; absence disables invoicing.
loadDatilConfig :: IO (Either Text DatilConfig)
loadDatilConfig = do
  let required name = maybe (Left (T.pack name <> " is not configured")) Right
        . (>>= nonEmpty) <$> lookupEnv name
      nonEmpty raw = let clean = T.strip (T.pack raw) in if T.null clean then Nothing else Just clean
  apiKey <- required "DATIL_API_KEY"
  password <- required "DATIL_CERTIFICATE_PASSWORD"
  environment <- required "DATIL_ENVIRONMENT"
  ruc <- required "TAX_ISSUER_RUC"
  legalName <- required "TAX_ISSUER_LEGAL_NAME"
  address <- required "TAX_ISSUER_ADDRESS"
  accounting <- required "TAX_ISSUER_ACCOUNTING_REQUIRED"
  tradeName <- fmap (>>= nonEmpty) (lookupEnv "TAX_ISSUER_TRADE_NAME")
  establishmentAddress <- fmap (>>= nonEmpty) (lookupEnv "TAX_ISSUER_ESTABLISHMENT_ADDRESS")
  special <- fmap (>>= nonEmpty) (lookupEnv "TAX_ISSUER_SPECIAL_TAXPAYER")
  pure $ do
    dcApiKey <- apiKey
    dcCertificatePassword <- password
    dcEnvironment <- environment >>= \value -> case value of
      "1" -> Right 1
      "2" -> Right 2
      _ -> Left "DATIL_ENVIRONMENT must be 1 (pruebas) or 2 (producción)"
    dcRuc <- ruc >>= \value ->
      if T.length value == 13 && T.all isDigit value && "001" `T.isSuffixOf` value
        then Right value else Left "TAX_ISSUER_RUC must be a 13-digit RUC"
    dcLegalName <- legalName
    dcAddress <- address
    dcAccountingRequired <- accounting >>= \value -> case T.toLower value of
      "true" -> Right True
      "false" -> Right False
      _ -> Left "TAX_ISSUER_ACCOUNTING_REQUIRED must be true or false"
    let dcTradeName = fromMaybe dcLegalName tradeName
        dcEstablishmentAddress = fromMaybe dcAddress establishmentAddress
        dcSpecialTaxpayer = special
    pure DatilConfig{..}

-- | SRI test documents belong to sandbox checkouts and production documents to
-- production checkouts; a mismatched pairing is never processed.
datilEnvironmentFor :: Text -> Maybe Int
datilEnvironmentFor "sandbox" = Just 1
datilEnvironmentFor "production" = Just 2
datilEnvironmentFor _ = Nothing

-- | Ready only with complete credentials matching the checkout environment and
-- an enabled issuer point for it.
invoicingReady :: ConnectionPool -> Text -> IO Bool
invoicingReady pool checkoutEnvironment = do
  configured <- loadDatilConfig
  case configured of
    Right config | datilEnvironmentFor checkoutEnvironment == Just (dcEnvironment config) -> do
      rows <- runSqlPool (rawSql
        "SELECT EXISTS (SELECT 1 FROM commerce_tax_issuer_point WHERE environment = ? AND enabled)"
        [PersistText checkoutEnvironment] :: SqlPersistT IO [Single Bool]) pool
      pure (rows == [Single True])
    _ -> pure False

data BuyerIdentity
  = ConsumidorFinal
  | Identified Text Text Text  -- ^ SRI type code, number, legal name
  deriving (Eq, Show)

data InvoiceLine = InvoiceLine
  { ilCode        :: Text
  , ilDescription :: Text
  , ilQuantity    :: Int
  , ilUnitMinor   :: Int64
  , ilDiscountMinor :: Int64
  } deriving (Eq, Show)

data InvoiceInput = InvoiceInput
  { iiSequential    :: Int64
  , iiAccessKey     :: Text
  , iiIssuedAt      :: Text   -- ^ ISO-8601 with offset, on the access-key date
  , iiEstablishment :: Text
  , iiEmissionPoint :: Text
  , iiBuyer         :: BuyerIdentity
  , iiBuyerEmail    :: Maybe Text
  , iiLines         :: [InvoiceLine]
  , iiTotalMinor    :: Int64
  , iiPaymentMedium :: Text
  , iiOrderReference :: Text
  } deriving (Eq, Show)

money :: Int64 -> Scientific
money minor = scientific (toInteger minor) (-2)

lineNetMinor :: InvoiceLine -> Int64
lineNetMinor line = ilUnitMinor line * fromIntegral (ilQuantity line) - ilDiscountMinor line

-- | IVA 0% invoice payload. Other tax rates are rejected rather than guessed.
invoicePayload :: DatilConfig -> InvoiceInput -> Either Text A.Value
invoicePayload DatilConfig{..} InvoiceInput{..}
  | null iiLines = Left "Invoice has no lines"
  | any (\line -> ilQuantity line < 1 || ilUnitMinor line < 0 || ilDiscountMinor line < 0
        || lineNetMinor line < 0) iiLines = Left "Invoice line amounts are invalid"
  | subtotal /= iiTotalMinor = Left "Invoice lines do not add up to the paid total"
  | otherwise = Right $ A.object
      [ "ambiente" .= dcEnvironment
      , "tipo_emision" .= (1 :: Int)
      , "secuencial" .= iiSequential
      , "clave_acceso" .= iiAccessKey
      , "fecha_emision" .= iiIssuedAt
      , "moneda" .= ("USD" :: Text)
      , "emisor" .= A.object
          ([ "ruc" .= dcRuc
           , "obligado_contabilidad" .= dcAccountingRequired
           , "nombre_comercial" .= dcTradeName
           , "razon_social" .= dcLegalName
           , "direccion" .= dcAddress
           , "establecimiento" .= A.object
               [ "codigo" .= iiEstablishment
               , "punto_emision" .= iiEmissionPoint
               , "direccion" .= dcEstablishmentAddress
               ]
           ] <> maybe [] (\code -> ["contribuyente_especial" .= code]) dcSpecialTaxpayer)
      , "comprador" .= A.object (buyerFields <> maybe [] (\email -> ["email" .= email]) iiBuyerEmail)
      , "totales" .= A.object
          [ "total_sin_impuestos" .= money subtotal
          , "descuento" .= money (sum (map ilDiscountMinor iiLines))
          , "propina" .= money 0
          , "impuestos" .= [zeroIva subtotal]
          , "importe_total" .= money iiTotalMinor
          ]
      , "items" .= map item iiLines
      , "pagos" .= [A.object [ "medio" .= iiPaymentMedium, "total" .= money iiTotalMinor ]]
      , "info_adicional" .= [A.object [ "nombre" .= ("Orden" :: Text), "valor" .= iiOrderReference ]]
      ]
  where
    subtotal = sum (map lineNetMinor iiLines)
    buyerFields = case iiBuyer of
      ConsumidorFinal ->
        [ "tipo_identificacion" .= ("07" :: Text)
        , "identificacion" .= ("9999999999999" :: Text)
        , "razon_social" .= ("CONSUMIDOR FINAL" :: Text)
        ]
      Identified code number name ->
        [ "tipo_identificacion" .= code, "identificacion" .= number, "razon_social" .= name ]
    zeroIva base = A.object
      [ "codigo" .= ("2" :: Text), "codigo_porcentaje" .= ("0" :: Text)
      , "base_imponible" .= money base, "valor" .= money 0 ]
    item line = A.object
      [ "cantidad" .= ilQuantity line
      , "codigo_principal" .= ilCode line
      , "descripcion" .= ilDescription line
      , "precio_unitario" .= money (ilUnitMinor line)
      , "descuento" .= money (ilDiscountMinor line)
      , "precio_total_sin_impuestos" .= money (lineNetMinor line)
      , "impuestos" .= [A.object
          [ "codigo" .= ("2" :: Text), "codigo_porcentaje" .= ("0" :: Text)
          , "tarifa" .= (0 :: Int), "base_imponible" .= money (lineNetMinor line)
          , "valor" .= money 0 ]]
      ]

-- | The authorized invoice a credit note modifies.
data ModifiedInvoice = ModifiedInvoice
  { miNumber   :: Text   -- ^ 001-002-000000123
  , miIssuedOn :: Text   -- ^ ISO-8601 with offset
  , miReason   :: Text
  } deriving (Eq, Show)

-- | Credit note: same issuer, buyer, lines and totals rules as an invoice, bound
-- to the modified invoice; payment media do not apply.
creditNotePayload :: DatilConfig -> InvoiceInput -> ModifiedInvoice -> Either Text A.Value
creditNotePayload config input ModifiedInvoice{..} = do
  base <- invoicePayload config input
  case base of
    -- Dátil's credit-note schema (checked 2026-10-06) rejects invoice-only
    -- totals and requires the special-taxpayer field, empty when not applicable.
    A.Object fields -> Right $ A.Object $
      KM.insert "fecha_emision_documento_modificado" (A.String miIssuedOn) $
      KM.insert "numero_documento_modificado" (A.String miNumber) $
      KM.insert "tipo_documento_modificado" (A.String "01") $
      KM.insert "motivo" (A.String (T.take 300 miReason)) $
      updateField "totales" withoutInvoiceTotals $
      updateField "emisor" withSpecialTaxpayer $
      KM.delete "pagos" fields
    _ -> Left "Credit note payload is not an object"
  where
    updateField key f object = maybe object (\value -> KM.insert key (f value) object) (KM.lookup key object)
    withoutInvoiceTotals (A.Object totals) = A.Object (KM.delete "descuento" (KM.delete "propina" totals))
    withoutInvoiceTotals other = other
    withSpecialTaxpayer (A.Object issuer) =
      A.Object (if KM.member "contribuyente_especial" issuer
        then issuer else KM.insert "contribuyente_especial" (A.String "") issuer)
    withSpecialTaxpayer other = other

-- | Dátil payment medium for the settling rail of the order.
paymentMedium :: Text -> Text
paymentMedium "bank_transfer" = "transferencia"
paymentMedium "datafast" = "tarjeta_credito"
paymentMedium _ = "otros"

data DatilOutcome
  = DatilAuthorized Text Text (Maybe UTCTime)  -- ^ provider id, authorization number, time
  | DatilPending Text                  -- ^ provider id, still being processed
  | DatilRejected Text Text            -- ^ provider id, redacted SRI messages
  deriving (Eq, Show)

interpretDatilDocument :: A.Value -> Either Text DatilOutcome
interpretDatilDocument = either (Left . T.pack) Right . A.parseEither (A.withObject "DatilDocument" $ \document -> do
  providerId <- document .: "id"
  status <- document .: "estado"
  authorization <- document .:? "autorizacion"
  number <- maybe (pure Nothing) (\obj -> A.withObject "autorizacion" (.:? "numero") obj) authorization
  authorizedAt <- maybe (pure Nothing) (\obj -> A.withObject "autorizacion" (.:? "fecha") obj) authorization
    :: A.Parser (Maybe Text)
  messages <- maybe (pure Nothing) (\obj -> A.withObject "autorizacion" (.:? "mensajes") obj) authorization
  let validId = T.length providerId <= 80 && T.all (\c -> c == '-' || c == '_' || c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || isDigit c) providerId
  when (T.null providerId || not validId) $ fail "Invalid provider document id"
  case (T.toUpper (T.strip status) :: Text, number, authorizedAt) of
    ("AUTORIZADO", Just numberText, at) | T.all isDigit numberText && T.length numberText >= 10 ->
      pure (DatilAuthorized providerId numberText (at >>= parseProviderTime))
    ("AUTORIZADO", _, _) -> pure (DatilPending providerId)
    (state, _, _) | state `elem` ["NO AUTORIZADO", "DEVUELTO", "ERROR"] ->
      pure (DatilRejected providerId (summarize state messages))
    _ -> pure (DatilPending providerId))
  where
    summarize :: Text -> Maybe A.Value -> Text
    summarize state messages = T.take 500 $ state <> maybe "" (\value -> ": " <> TE.decodeUtf8 (BL.toStrict (A.encode value))) messages

-- | Dátil reports SRI timestamps with or without an offset; the SRI clock is Ecuador time.
parseProviderTime :: Text -> Maybe UTCTime
parseProviderTime raw = case mapMaybeFirst attempt formats of
  Just value -> Just value
  Nothing -> Nothing
  where
    clean = T.unpack (T.strip raw)
    formats =
      [ ("%Y-%m-%dT%H:%M:%S%Q%Ez", False), ("%Y-%m-%dT%H:%M:%S%QZ", False)
      , ("%Y-%m-%dT%H:%M:%S%Q", True) ]
    attempt (format, local)
      | local = localTimeToUTC (hoursToTimeZone (-5)) <$> parseTimeM True defaultTimeLocale format clean
      | otherwise = parseTimeM True defaultTimeLocale format clean
    mapMaybeFirst f = foldr (\x acc -> maybe acc Just (f x)) Nothing

-- Transport --------------------------------------------------------------------

datilBaseUrl :: String
datilBaseUrl = "https://link.datil.co"

maximumResponseBytes :: Int
maximumResponseBytes = 256 * 1024

data TransportResult
  = TransportOk A.Value
  | TransportRejected Int Text   -- ^ provider refused before creating anything
  | TransportUnknown             -- ^ outcome cannot be known

datilRequest :: DatilConfig -> BS.ByteString -> String -> Maybe A.Value -> IO TransportResult
datilRequest DatilConfig{..} httpMethod path body = do
  parsed <- tryAny (parseRequest (datilBaseUrl <> path))
  case parsed of
    Left _ -> pure TransportUnknown
    Right base -> do
      let request = base
            { method = httpMethod
            , requestHeaders =
                [ ("X-Key", TE.encodeUtf8 dcApiKey)
                , ("Content-Type", "application/json")
                , ("Accept", "application/json")
                ] <> [ ("X-Password", TE.encodeUtf8 dcCertificatePassword) | httpMethod == "POST" ]
            , requestBody = RequestBodyLBS (maybe "" A.encode body)
            , redirectCount = 0
            , responseTimeout = responseTimeoutMicro 30000000
            }
      if not (secure request && host request == "link.datil.co" && port request == 443)
        then pure TransportUnknown
        else do
          result <- tryAny $ timeout 45000000 $ withResponse request sharedProviderManager $ \response -> do
            bytes <- readBounded (responseBody response)
            pure (statusCode (responseStatus response), bytes)
          pure $ case result of
            Right (Just (code, Just bytes))
              | code >= 200 && code < 300 ->
                  maybe TransportUnknown TransportOk (A.decodeStrict bytes)
              | code >= 400 && code < 500 && code /= 408 && code /= 429 ->
                  TransportRejected code (redactedError bytes)
            _ -> TransportUnknown
  where
    readBounded reader = go 0 []
      where
        go total chunks = do
          chunk <- brRead reader
          if BS.null chunk
            then Just <$> evaluate (BS.concat (reverse chunks))
            else if total + BS.length chunk > maximumResponseBytes
              then pure Nothing
              else go (total + BS.length chunk) (chunk : chunks)
    -- Provider validation messages help staff fix configuration; secrets are never echoed.
    redactedError bytes = T.take 500 $ T.replace dcCertificatePassword "[redacted]" $
      T.replace dcApiKey "[redacted]" $ TE.decodeUtf8With (\_ _ -> Just '?') bytes

-- Worker -----------------------------------------------------------------------

data ClaimedDocument = ClaimedDocument
  { cdId :: Text
  , cdKind :: Text
  , cdStatus :: Text
  , cdSubmissionStarted :: Bool
  , cdLease :: Text
  , cdProviderId :: Maybe Text
  }

claimSql :: Text
claimSql =
  "WITH next_document AS (\
  \ SELECT id FROM commerce_tax_document\
  \ WHERE status IN ('pending','submitted') AND next_attempt_at <= clock_timestamp()\
  \ AND environment = ? AND (lease_expires_at IS NULL OR lease_expires_at < clock_timestamp())\
  \ ORDER BY next_attempt_at FOR UPDATE SKIP LOCKED LIMIT 1)\
  \ UPDATE commerce_tax_document document\
  \ SET lease_token = ?::uuid, lease_expires_at = clock_timestamp() + INTERVAL '2 minutes',\
  \ attempts = document.attempts + 1\
  \ FROM next_document WHERE document.id = next_document.id\
  \ RETURNING document.id::text, document.kind, document.status, document.submitted_at IS NOT NULL,\
  \ document.provider_document_id"

-- | Process at most one due document. Returns whether one was claimed.
processNextTaxDocument :: Env -> DatilConfig -> IO Bool
processNextTaxDocument Env{envPool} config = do
  lease <- UUID.toText <$> UUID.nextRandom
  let checkoutEnvironment = if dcEnvironment config == 1 then "sandbox" else "production" :: Text
  claimed <- runSqlPool (rawSql claimSql [PersistText checkoutEnvironment, PersistText lease]
    :: SqlPersistT IO [(Single Text, Single Text, Single Text, Single Bool, Single (Maybe Text))]) envPool
  case claimed of
    [(Single cdId, Single cdKind, Single cdStatus, Single cdSubmissionStarted, Single cdProviderId)] -> do
      let document = ClaimedDocument{ cdLease = lease, .. }
      outcome <- tryAny (handleDocument envPool config document)
      case outcome of
        Right () -> pure ()
        Left _ -> finish envPool document "pending" Nothing Nothing Nothing
          (Just "Internal invoicing error; retrying") (Just 300)
      pure True
    _ -> pure False

handleDocument :: ConnectionPool -> DatilConfig -> ClaimedDocument -> IO ()
handleDocument pool config document@ClaimedDocument{..}
  | cdStatus == "submitted", Just providerId <- cdProviderId = do
      result <- datilRequest config "GET" (resource <> "/" <> T.unpack providerId) Nothing
      applyResult pool document result
  | cdStatus == "pending" && cdSubmissionStarted =
      -- A previous submission may have reached Dátil before the worker stopped.
      finish pool document "uncertain" Nothing Nothing Nothing
        (Just "A previous submission outcome is unknown; reconcile in Dátil before resending") Nothing
  | otherwise = do
      prepared <- if cdKind == "credit_note"
        then prepareCreditNote pool config document
        else prepareInvoice pool config document
      case prepared of
        Left (Deferred reason) -> finish pool document "pending" Nothing Nothing Nothing (Just reason) (Just 600)
        Left (Refused problem) -> finish pool document "failed" Nothing Nothing Nothing (Just problem) Nothing
        Right payload -> do
          started <- markSubmissionStarted pool document
          when started $ do
            result <- datilRequest config "POST" (resource <> "/issue") (Just payload)
            applyResult pool document result
  where
    resource = if cdKind == "credit_note" then "/credit-notes" else "/invoices"

data PreparationProblem = Deferred Text | Refused Text

-- Persist that a provider request is about to be sent; only then may it be sent.
markSubmissionStarted :: ConnectionPool -> ClaimedDocument -> IO Bool
markSubmissionStarted pool ClaimedDocument{..} = do
  rows <- runSqlPool (rawSql
    "UPDATE commerce_tax_document SET submitted_at = clock_timestamp()\
    \ WHERE id = ?::uuid AND lease_token = ?::uuid AND submitted_at IS NULL RETURNING id::text"
    [PersistText cdId, PersistText cdLease] :: SqlPersistT IO [Single Text]) pool
  pure (length rows == 1)

applyResult :: ConnectionPool -> ClaimedDocument -> TransportResult -> IO ()
applyResult pool document result = case result of
  TransportUnknown
    | cdStatus document == "submitted" ->
        finish pool document "submitted" Nothing Nothing Nothing (Just "Status query failed; retrying") (Just 120)
    | otherwise ->
        finish pool document "uncertain" Nothing Nothing Nothing
          (Just "Submission outcome is unknown; reconcile in Dátil before resending") Nothing
  TransportRejected code message ->
    finish pool document (if cdStatus document == "submitted" then "submitted" else "failed")
      Nothing Nothing Nothing (Just ("HTTP " <> T.pack (show code) <> ": " <> message))
      (if cdStatus document == "submitted" then Just 600 else Nothing)
  TransportOk value -> case interpretDatilDocument value of
    Left _ -> finish pool document "uncertain" Nothing Nothing Nothing
      (Just "Dátil returned an unreadable document; reconcile before resending") Nothing
    Right (DatilAuthorized providerId number at) -> do
      now <- getCurrentTime
      finish pool document "authorized" (Just providerId) (Just number) (Just (fromMaybe now at)) Nothing Nothing
    Right (DatilPending providerId) ->
      finish pool document "submitted" (Just providerId) Nothing Nothing Nothing (Just 30)
    Right (DatilRejected providerId message) ->
      finish pool document "rejected" (Just providerId) Nothing Nothing (Just message) Nothing


-- | Lease-fenced completion; a stale worker cannot overwrite a newer result.
finish
  :: ConnectionPool -> ClaimedDocument -> Text -> Maybe Text -> Maybe Text -> Maybe UTCTime
  -> Maybe Text -> Maybe Int -> IO ()
finish pool ClaimedDocument{..} status providerId number authorizedAt problem retrySeconds = do
  now <- getCurrentTime
  let nextAttempt = maybe now (\seconds -> addUTCTime (fromIntegral seconds) now) retrySeconds
  void $ runSqlPool (rawExecute
    "UPDATE commerce_tax_document SET status = ?,\
    \ provider_document_id = COALESCE(?, provider_document_id),\
    \ authorization_number = COALESCE(?, authorization_number),\
    \ authorized_at = COALESCE(?, authorized_at), last_error = ?, next_attempt_at = ?,\
    \ lease_token = NULL, lease_expires_at = NULL\
    \ WHERE id = ?::uuid AND lease_token = ?::uuid"
    [ PersistText status, maybe PersistNull PersistText providerId
    , maybe PersistNull PersistText number, maybe PersistNull PersistUTCTime authorizedAt
    , maybe PersistNull PersistText problem, PersistUTCTime nextAttempt
    , PersistText cdId, PersistText cdLease ]) pool

ecuadorDay :: UTCTime -> Day
ecuadorDay = localDay . utcToLocalTime (hoursToTimeZone (-5))

-- | Bind the issue date and access key on first submission, then build the
-- payload from the immutable checkout snapshot.
prepareInvoice :: ConnectionPool -> DatilConfig -> ClaimedDocument -> IO (Either PreparationProblem A.Value)
prepareInvoice pool config ClaimedDocument{..} = do
  now <- getCurrentTime
  rows <- runSqlPool (rawSql
    "SELECT document.sequential, document.establishment, document.emission_point,\
    \ document.access_key, document.issued_on, document.amount_minor, document.domain_order_id,\
    \ runtime.quantity, runtime.unit_price_minor, runtime.discount_minor, runtime.buyer_fee_minor,\
    \ runtime.tax_bps, runtime.checkout_total_minor, event.title, tier.name,\
    \ ticket_order.buyer_email, billing.id_type, billing.id_number, billing.legal_name,\
    \ (SELECT attempt.provider FROM commerce_payment_attempt attempt\
    \   WHERE attempt.checkout_id = document.checkout_id AND attempt.status = 'succeeded'\
    \   ORDER BY attempt.updated_at DESC LIMIT 1)\
    \ FROM commerce_tax_document document\
    \ JOIN event_ticket_checkout_runtime runtime ON runtime.checkout_id = document.checkout_id\
    \ JOIN event_ticket_order ticket_order ON ticket_order.id = runtime.order_id\
    \ JOIN social_event event ON event.id = runtime.event_id\
    \ JOIN event_ticket_tier tier ON tier.id = runtime.tier_id\
    \ LEFT JOIN event_ticket_billing_identity billing ON billing.order_id = runtime.order_id\
    \ WHERE document.id = ?::uuid AND document.lease_token = ?::uuid"
    [PersistText cdId, PersistText cdLease]
    :: SqlPersistT IO
      [( Single Int64, Single Text, Single Text, Single (Maybe Text), Single (Maybe Day)
       , Single Int64, Single Text, Single Int, Single Int64, Single Int64, Single Int64
       , Single Int, Single Int64, Single Text, Single Text, Single (Maybe Text)
       , Single (Maybe Text), Single (Maybe Text), Single (Maybe Text), Single (Maybe Text)
       )]) pool
  case rows of
    [( Single sequential, Single establishment, Single emissionPoint, Single storedKey
     , Single storedDay, Single amountMinor, Single orderId, Single quantity, Single unitMinor
     , Single discountMinor, Single buyerFeeMinor, Single taxBps, Single totalMinor
     , Single eventTitle, Single tierName, Single buyerEmail, Single idType, Single idNumber
     , Single legalName, Single provider
     )]
      | taxBps /= 0 -> pure (Left (Refused "Only IVA 0% ticket invoices are supported"))
      | amountMinor /= totalMinor -> pure (Left (Refused "Invoice amount differs from the checkout total"))
      | otherwise -> do
          let issuedOn = fromMaybe (ecuadorDay now) storedDay
              keyResult = maybe (accessKey AccessKeyInput
                { akiIssuedOn = issuedOn, akiDocumentType = "01", akiRuc = dcRuc config
                , akiEnvironment = dcEnvironment config, akiEstablishment = establishment
                , akiEmissionPoint = emissionPoint, akiSequential = sequential
                , akiNumericCode = numericCodeFromSeed cdId }) Right storedKey
          case (keyResult, buyerIdentityFrom idType idNumber legalName) of
            (Left problem, _) -> pure (Left (Refused problem))
            (_, Left problem) -> pure (Left (Refused problem))
            (Right key, Right buyer) -> do
              when (storedKey /= Just key || storedDay /= Just issuedOn) $
                void $ runSqlPool (rawExecute
                  "UPDATE commerce_tax_document SET access_key = ?, issued_on = ?\
                  \ WHERE id = ?::uuid AND lease_token = ?::uuid"
                  [PersistText key, PersistDay issuedOn, PersistText cdId, PersistText cdLease]) pool
              let reference = "TDF-" <> orderId
                  ticketLine = InvoiceLine
                    { ilCode = "ENTRADA", ilDescription = T.take 300 ("Entrada " <> tierName <> " - " <> eventTitle)
                    , ilQuantity = quantity, ilUnitMinor = unitMinor, ilDiscountMinor = discountMinor }
                  feeLine = InvoiceLine
                    { ilCode = "SERVICIO", ilDescription = "Tarifa de servicio TDF"
                    , ilQuantity = 1, ilUnitMinor = buyerFeeMinor, ilDiscountMinor = 0 }
              pure $ either (Left . Refused) Right $ invoicePayload config InvoiceInput
                { iiSequential = sequential, iiAccessKey = key
                , iiIssuedAt = T.pack (show issuedOn) <> "T12:00:00.000-05:00"
                , iiEstablishment = establishment, iiEmissionPoint = emissionPoint
                , iiBuyer = buyer, iiBuyerEmail = buyerEmail
                , iiLines = ticketLine : [feeLine | buyerFeeMinor > 0]
                , iiTotalMinor = totalMinor
                , iiPaymentMedium = paymentMedium (fromMaybe "" provider)
                , iiOrderReference = reference }
    _ -> pure (Left (Refused "Invoice source data is missing"))

-- | A credit note waits until its invoice is authorized, then reuses the
-- invoice's buyer and refunds the exact verified amount.
prepareCreditNote :: ConnectionPool -> DatilConfig -> ClaimedDocument -> IO (Either PreparationProblem A.Value)
prepareCreditNote pool config ClaimedDocument{..} = do
  now <- getCurrentTime
  rows <- runSqlPool (rawSql
    "SELECT note.sequential, note.establishment, note.emission_point, note.access_key,\
    \ note.issued_on, note.amount_minor, note.domain_order_id, invoice.status,\
    \ invoice.establishment, invoice.emission_point, invoice.sequential, invoice.issued_on,\
    \ event.title, ticket_order.buyer_email, billing.id_type, billing.id_number, billing.legal_name,\
    \ (SELECT count(*) FROM event_ticket_refund_allocation allocation WHERE allocation.refund_id = note.refund_id)\
    \ FROM commerce_tax_document note\
    \ JOIN commerce_tax_document invoice ON invoice.id = note.related_document_id AND invoice.kind = 'invoice'\
    \ JOIN event_ticket_checkout_runtime runtime ON runtime.checkout_id = note.checkout_id\
    \ JOIN event_ticket_order ticket_order ON ticket_order.id = runtime.order_id\
    \ JOIN social_event event ON event.id = runtime.event_id\
    \ LEFT JOIN event_ticket_billing_identity billing ON billing.order_id = runtime.order_id\
    \ WHERE note.id = ?::uuid AND note.lease_token = ?::uuid AND note.kind = 'credit_note'"
    [PersistText cdId, PersistText cdLease]
    :: SqlPersistT IO
      [( Single Int64, Single Text, Single Text, Single (Maybe Text), Single (Maybe Day)
       , Single Int64, Single Text, Single Text, Single Text, Single Text, Single Int64
       , Single (Maybe Day), Single Text, Single (Maybe Text), Single (Maybe Text)
       , Single (Maybe Text), Single (Maybe Text), Single Int64
       )]) pool
  case rows of
    [( Single sequential, Single establishment, Single emissionPoint, Single storedKey
     , Single storedDay, Single amountMinor, Single orderId, Single invoiceStatus
     , Single invoiceEstablishment, Single invoiceEmissionPoint, Single invoiceSequential
     , Single invoiceDay, Single eventTitle, Single buyerEmail, Single idType, Single idNumber
     , Single legalName, Single ticketCount
     )]
      | invoiceStatus `elem` ["pending", "submitted"] ->
          pure (Left (Deferred "Waiting for the modified invoice to be authorized"))
      | invoiceStatus /= "authorized" ->
          pure (Left (Refused "The modified invoice is not authorized; reconcile it first"))
      | otherwise -> case invoiceDay of
          Nothing -> pure (Left (Refused "The modified invoice has no issue date"))
          Just modifiedDay -> do
            let issuedOn = fromMaybe (ecuadorDay now) storedDay
                keyResult = maybe (accessKey AccessKeyInput
                  { akiIssuedOn = issuedOn, akiDocumentType = "04", akiRuc = dcRuc config
                  , akiEnvironment = dcEnvironment config, akiEstablishment = establishment
                  , akiEmissionPoint = emissionPoint, akiSequential = sequential
                  , akiNumericCode = numericCodeFromSeed cdId }) Right storedKey
                tickets = max 1 ticketCount
            case (keyResult, buyerIdentityFrom idType idNumber legalName) of
              (Left problem, _) -> pure (Left (Refused problem))
              (_, Left problem) -> pure (Left (Refused problem))
              (Right key, Right buyer) -> do
                when (storedKey /= Just key || storedDay /= Just issuedOn) $
                  void $ runSqlPool (rawExecute
                    "UPDATE commerce_tax_document SET access_key = ?, issued_on = ?\
                    \ WHERE id = ?::uuid AND lease_token = ?::uuid"
                    [PersistText key, PersistDay issuedOn, PersistText cdId, PersistText cdLease]) pool
                pure $ either (Left . Refused) Right $ creditNotePayload config InvoiceInput
                  { iiSequential = sequential, iiAccessKey = key
                  , iiIssuedAt = T.pack (show issuedOn) <> "T12:00:00.000-05:00"
                  , iiEstablishment = establishment, iiEmissionPoint = emissionPoint
                  , iiBuyer = buyer, iiBuyerEmail = buyerEmail
                  , iiLines = [InvoiceLine
                      { ilCode = "DEVOLUCION"
                      , ilDescription = T.take 300 ("Devolución de " <> T.pack (show tickets)
                          <> " entrada(s) - " <> eventTitle)
                      , ilQuantity = 1, ilUnitMinor = amountMinor, ilDiscountMinor = 0 }]
                  , iiTotalMinor = amountMinor, iiPaymentMedium = "otros"
                  , iiOrderReference = "TDF-" <> orderId }
                  ModifiedInvoice
                    { miNumber = invoiceEstablishment <> "-" <> invoiceEmissionPoint <> "-"
                        <> T.justifyRight 9 '0' (T.pack (show invoiceSequential))
                    , miIssuedOn = T.pack (show modifiedDay) <> "T12:00:00.000-05:00"
                    , miReason = "Devolución de entradas" }
    _ -> pure (Left (Refused "Credit note source data is missing"))

buyerIdentityFrom :: Maybe Text -> Maybe Text -> Maybe Text -> Either Text BuyerIdentity
buyerIdentityFrom (Just "cedula") (Just number) (Just name) = Right (Identified "05" number name)
buyerIdentityFrom (Just "ruc") (Just number) (Just name) = Right (Identified "04" number name)
buyerIdentityFrom (Just "pasaporte") (Just number) (Just name) = Right (Identified "06" number name)
buyerIdentityFrom (Just "consumidor_final") _ _ = Right ConsumidorFinal
buyerIdentityFrom Nothing _ _ = Right ConsumidorFinal
buyerIdentityFrom _ _ _ = Left "Buyer billing identity is incomplete"

startTaxInvoiceWorker :: Env -> IO ()
startTaxInvoiceWorker env = do
  enabled <- (== Just "true") <$> lookupEnv "TAX_INVOICE_WORKER_ENABLED"
  when enabled $ do
    configured <- loadDatilConfig
    case configured of
      Left problem -> hPutStrLn stderr ("[TaxInvoice] Worker disabled: " <> T.unpack problem)
      Right config -> void . forkIO . forever $ do
        result <- tryAny (processNextTaxDocument env config)
        delay <- case result of
          Right True -> pure 1000000
          Right False -> pure 15000000
          Left _ -> do
            void $ tryAny $ hPutStrLn stderr "[TaxInvoice] Worker iteration failed; documents retained"
            pure 30000000
        threadDelay delay
