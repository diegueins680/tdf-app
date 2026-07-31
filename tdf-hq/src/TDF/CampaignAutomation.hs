{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.CampaignAutomation
  ( CampaignTickStats(..)
  , applyWhatsAppCampaignOptOut
  , campaignAutomationTemplatesDTO
  , enrollCampaignParties
  , installCampaignAutomation
  , listCampaignAutomations
  , listCampaignEnrollments
  , previewCampaignAutomation
  , renderCampaignMessage
  , startCampaignAutomationJob
  , updateCampaignAutomationStatus
  , updateCampaignEnrollmentStatus
  , isWhatsAppCampaignOptOutMessage
  , validateCampaignAutomationActivation
  , validateCampaignAutomationDailyLimit
  , validateCampaignAutomationStatus
  ) where

import           Control.Applicative     ((<|>))
import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Exception       (SomeException, finally, try)
import           Control.Monad           (foldM, forever, unless, void, when)
import           Data.Char               (isAlphaNum)
import           Data.Int                (Int64)
import           Data.List               (find, nub)
import           Data.Maybe              (catMaybes, fromMaybe, isJust)
import           Data.Pool               (withResource)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time               (UTCTime(..), addUTCTime, getCurrentTime, utctDay)
import           Database.Persist
  ( Entity(..)
  , SelectOpt(..)
  , count
  , get
  , getBy
  , getJust
  , getJustEntity
  , insert
  , insertUnique
  , selectFirst
  , selectList
  , upsert
  , update
  , (=.)
  , (==.)
  , (<=.)
  , (>.)
  , (>=.)
  , (<-.)
  )
import           Database.Persist.Sql
  ( Single(..)
  , SqlPersistT
  , fromSqlKey
  , rawSql
  , runSqlConn
  , runSqlPool
  , toSqlKey
  )
import           System.IO               (hPutStrLn, stderr)

import           TDF.Config              (AppConfig, resolveConfiguredAppBase)
import           TDF.DB                  (ConnectionPool, Env(..))
import           TDF.DTO
import qualified TDF.LogBuffer           as LogBuf
import qualified TDF.Models              as M
import qualified TDF.ModelsExtra         as ME
import           TDF.WhatsApp.Client     (SendTextResult(..))
import           TDF.WhatsApp.History
  ( OutgoingWhatsAppRecord(..)
  , normalizeWhatsAppPhone
  , phoneLookupAliases
  , recordOutgoingWhatsAppMessage
  )
import           TDF.WhatsApp.Transport
  ( WhatsAppEnv
  , loadWhatsAppEnv
  , sendWhatsAppTemplateIO
  )

data CampaignStepTemplate = CampaignStepTemplate
  { cstPosition             :: Int
  , cstDelayDays            :: Int
  , cstProviderTemplateName :: Text
  , cstBody                 :: Text
  } deriving (Show, Eq)

data CampaignTemplate = CampaignTemplate
  { ctKey         :: Text
  , ctName        :: Text
  , ctObjective   :: Text
  , ctAudience    :: Text
  , ctOffer       :: Text
  , ctLandingPath :: Text
  , ctSteps       :: [CampaignStepTemplate]
  } deriving (Show, Eq)

data CampaignTickStats = CampaignTickStats
  { ctsSent    :: Int
  , ctsFailed  :: Int
  , ctsStopped :: Int
  } deriving (Show, Eq)

instance Semigroup CampaignTickStats where
  left <> right =
    CampaignTickStats
      { ctsSent = ctsSent left + ctsSent right
      , ctsFailed = ctsFailed left + ctsFailed right
      , ctsStopped = ctsStopped left + ctsStopped right
      }

instance Monoid CampaignTickStats where
  mempty = CampaignTickStats 0 0 0

campaignTemplates :: [CampaignTemplate]
campaignTemplates =
  [ CampaignTemplate
      { ctKey = "music-services"
      , ctName = "TDF · Single listo para lanzar"
      , ctObjective = "Vender paquetes de mezcla y mastering a artistas con material grabado."
      , ctAudience = "Artistas y bandas con relación previa o consentimiento vigente."
      , ctOffer = "Mezcla + mastering, una revisión y entrega en siete días por USD 249."
      , ctLandingPath =
          "/reservar?utm_source=whatsapp&utm_medium=automation&utm_campaign=music_services"
      , ctSteps =
          [ CampaignStepTemplate
              1
              0
              "tdf_music_services_intro_v1"
              ( T.unlines
                  [ "Hola {{name}}, abrimos tres cupos TDF para artistas que ya tengan una canción grabada."
                  , "El paquete incluye mezcla, mastering, una revisión y entrega en siete días por USD 249."
                  , "Puedes revisar o solicitar tu cupo aquí: {{url}}"
                  , "Si prefieres no recibir este seguimiento, responde SALIR."
                  ]
              )
          , CampaignStepTemplate
              2
              2
              "tdf_music_services_fit_v1"
              ( T.unlines
                  [ "Hola {{name}}, para confirmar si tu canción está lista solo necesitamos escuchar un bounce y revisar los stems."
                  , "Te diremos con honestidad si conviene mezclar ahora o corregir algo primero: {{url}}"
                  , "Si ya resolviste tu lanzamiento, responde LISTO y detenemos el seguimiento."
                  ]
              )
          , CampaignStepTemplate
              3
              3
              "tdf_music_services_close_v1"
              ( T.unlines
                  [ "Hola {{name}}, cerramos esta ronda de cupos de mezcla y mastering."
                  , "Si quieres reservar uno de los espacios disponibles, envía tu material desde {{url}}"
                  , "Si no es el momento, no hace falta responder; este es el último mensaje de la secuencia."
                  ]
              )
          ]
      }
  , CampaignTemplate
      { ctKey = "domo-bookings"
      , ctName = "Domo del Pululahua · Reservas piloto"
      , ctObjective = "Generar visitas y cotizaciones calificadas para usos de baja complejidad operativa."
      , ctAudience = "Fotógrafos, productoras, facilitadores, planners y empresas con consentimiento."
      , ctOffer = "Sesiones audiovisuales, talleres diurnos y experiencias privadas cotizadas a medida."
      , ctLandingPath =
          "/domo-del-pululahua?utm_source=whatsapp&utm_medium=automation&utm_campaign=domo_bookings"
      , ctSteps =
          [ CampaignStepTemplate
              1
              0
              "tdf_domo_bookings_intro_v1"
              ( T.unlines
                  [ "Hola {{name}}, Domo del Pululahua está recibiendo solicitudes para sesiones audiovisuales, talleres diurnos y experiencias privadas."
                  , "Puedes conocer el espacio y pedir una cotización aquí: {{url}}"
                  , "Toda reserva se confirma según disponibilidad, permisos y condiciones operativas."
                  ]
              )
          , CampaignStepTemplate
              2
              4
              "tdf_domo_bookings_visit_v1"
              ( T.unlines
                  [ "Hola {{name}}, si estás planificando una producción o experiencia, podemos revisar aforo, horario, acceso y proveedores antes de cotizar."
                  , "Solicita una visita o comparte tu idea desde {{url}}"
                  , "Si no deseas más mensajes, responde SALIR."
                  ]
              )
          , CampaignStepTemplate
              3
              6
              "tdf_domo_bookings_close_v1"
              ( T.unlines
                  [ "Hola {{name}}, este es el último seguimiento sobre Domo del Pululahua."
                  , "Si tienes una fecha tentativa, puedes dejarla aquí para validar disponibilidad: {{url}}"
                  , "No se bloqueará ninguna fecha sin cotización, condiciones y depósito confirmados."
                  ]
              )
          ]
      }
  , CampaignTemplate
      { ctKey = "managed-operations"
      , ctName = "TDF Ops · Implementación para estudios y venues"
      , ctObjective = "Vender una implementación gestionada de CRM, reservas, pagos y seguimiento."
      , ctAudience = "Estudios, academias, salas y venues con un contacto autorizado."
      , ctOffer = "Diagnóstico breve y configuración de un flujo operativo adaptado al negocio."
      , ctLandingPath =
          "/tdf?utm_source=whatsapp&utm_medium=automation&utm_campaign=managed_operations"
      , ctSteps =
          [ CampaignStepTemplate
              1
              0
              "tdf_managed_ops_intro_v1"
              ( T.unlines
                  [ "Hola {{name}}, TDF está abriendo cupos para ayudar a estudios y venues a ordenar contactos, reservas, servicios y cobros en un solo flujo."
                  , "La primera conversación es un diagnóstico breve, no una venta de software genérico: {{url}}"
                  , "Si no corresponde a tu operación, responde SALIR."
                  ]
              )
          , CampaignStepTemplate
              2
              4
              "tdf_managed_ops_audit_v1"
              ( T.unlines
                  [ "Hola {{name}}, normalmente empezamos detectando dónde se pierden consultas, fechas o anticipos."
                  , "Después proponemos una implementación pequeña con alcance, responsable y métricas claras."
                  , "Puedes revisar el ecosistema TDF aquí: {{url}}"
                  ]
              )
          , CampaignStepTemplate
              3
              5
              "tdf_managed_ops_close_v1"
              ( T.unlines
                  [ "Hola {{name}}, cierro este seguimiento sobre la implementación operativa de TDF."
                  , "Si quieres que revisemos tu proceso actual, responde AUDITORÍA o visita {{url}}"
                  , "Este es el último mensaje automático de la secuencia."
                  ]
              )
          ]
      }
  , CampaignTemplate
      { ctKey = "marketplace-validation"
      , ctName = "TDF Marketplace · Validación de compradores y artistas"
      , ctObjective = "Validar demanda real antes de ampliar funciones o invertir en pauta."
      , ctAudience = "Fans, artistas y compradores con consentimiento vigente."
      , ctOffer = "Acceso a pruebas de productos, tickets, drops y experiencias de artistas TDF."
      , ctLandingPath =
          "/marketplace?utm_source=whatsapp&utm_medium=automation&utm_campaign=marketplace_validation"
      , ctSteps =
          [ CampaignStepTemplate
              1
              0
              "tdf_marketplace_validation_intro_v1"
              ( T.unlines
                  [ "Hola {{name}}, estamos validando qué debería vender primero el marketplace de TDF: merch, entradas, drops digitales o experiencias."
                  , "Puedes explorar lo disponible y decirnos qué comprarías de verdad aquí: {{url}}"
                  , "Tu respuesta se usará para priorizar, no para prometer productos inexistentes."
                  ]
              )
          , CampaignStepTemplate
              2
              3
              "tdf_marketplace_validation_value_v1"
              ( T.unlines
                  [ "Hola {{name}}, una respuesta corta nos ayuda mucho: ¿qué comprarías primero a un artista local?"
                  , "Puedes responder MERCH, TICKETS, DIGITAL o EXPERIENCIA y revisar el marketplace en {{url}}"
                  , "Si no deseas más mensajes, responde SALIR."
                  ]
              )
          , CampaignStepTemplate
              3
              4
              "tdf_marketplace_validation_close_v1"
              ( T.unlines
                  [ "Hola {{name}}, cerramos esta ronda de validación del marketplace."
                  , "Si quieres participar, esta es la última invitación: {{url}}"
                  , "Gracias por ayudar a construir únicamente lo que artistas y fans sí usarán."
                  ]
              )
          ]
      }
  ]

campaignAutomationTemplatesDTO :: [CampaignAutomationTemplateDTO]
campaignAutomationTemplatesDTO = map templateToDTO campaignTemplates

templateToDTO :: CampaignTemplate -> CampaignAutomationTemplateDTO
templateToDTO CampaignTemplate{..} =
  CampaignAutomationTemplateDTO
    { catKey = ctKey
    , catName = ctName
    , catObjective = ctObjective
    , catAudience = ctAudience
    , catOffer = ctOffer
    , catLandingPath = ctLandingPath
    , catSteps = map (stepTemplateToDTO ctLandingPath) ctSteps
    }

stepTemplateToDTO :: Text -> CampaignStepTemplate -> CampaignAutomationStepDTO
stepTemplateToDTO landingPath CampaignStepTemplate{..} =
  CampaignAutomationStepDTO
    { casPosition = cstPosition
    , casDelayDays = cstDelayDays
    , casChannel = "whatsapp"
    , casProviderTemplateName = cstProviderTemplateName
    , casLanguageCode = "es"
    , casBody = cstBody
    , casCtaPath = landingPath
    }

findCampaignTemplate :: Text -> Maybe CampaignTemplate
findCampaignTemplate rawKey =
  let key = T.toLower (T.strip rawKey)
  in find ((== key) . ctKey) campaignTemplates

validateCampaignAutomationDailyLimit :: Maybe Int -> Either Text Int
validateCampaignAutomationDailyLimit Nothing = Right 20
validateCampaignAutomationDailyLimit (Just value)
  | value >= 1 && value <= 100 = Right value
  | otherwise = Left "dailyLimit must be between 1 and 100"

validateCampaignAutomationStatus :: Text -> Either Text Text
validateCampaignAutomationStatus rawStatus =
  let status = T.toLower (T.strip rawStatus)
  in if status `elem` ["draft", "active", "paused", "completed"]
       then Right status
       else Left "status must be draft, active, paused, or completed"

validateCampaignAutomationActivation :: Text -> Maybe Bool -> Either Text Text
validateCampaignAutomationActivation rawStatus templatesApproved = do
  status <- validateCampaignAutomationStatus rawStatus
  if status == "active" && templatesApproved /= Just True
    then Left "templatesApproved must be true before activation"
    else Right status

installCampaignAutomation
  :: UTCTime
  -> CampaignAutomationInstall
  -> SqlPersistT IO (Either Text CampaignAutomationDTO)
installCampaignAutomation now CampaignAutomationInstall{..} =
  case findCampaignTemplate caiTemplateKey of
    Nothing -> pure (Left "Unknown campaign automation template")
    Just template ->
      case validateCampaignAutomationDailyLimit caiDailyLimit of
        Left err -> pure (Left err)
        Right dailyLimit -> do
          existing <- getBy (ME.UniqueCampaignAutomationTemplate (ctKey template))
          case existing of
            Just (Entity automationId _) ->
              Right <$> loadCampaignAutomationDTO automationId
            Nothing -> do
              campaignId <- insert ME.Campaign
                { ME.campaignName = ctName template
                , ME.campaignObjective = Just (ctObjective template)
                , ME.campaignPlatform = Just "whatsapp"
                , ME.campaignStatus = "draft"
                , ME.campaignBudgetCents = Just 0
                , ME.campaignStartDate = Nothing
                , ME.campaignEndDate = Nothing
                , ME.campaignCreatedAt = now
                , ME.campaignUpdatedAt = now
                }
              automationId <- insert ME.CampaignAutomation
                { ME.campaignAutomationCampaignId = campaignId
                , ME.campaignAutomationTemplateKey = ctKey template
                , ME.campaignAutomationStatus = "draft"
                , ME.campaignAutomationStartAt = fromMaybe now caiStartAt
                , ME.campaignAutomationDailyLimit = dailyLimit
                , ME.campaignAutomationLastRunAt = Nothing
                , ME.campaignAutomationCreatedAt = now
                , ME.campaignAutomationUpdatedAt = now
                }
              mapM_ (insertStep now automationId (ctLandingPath template)) (ctSteps template)
              Right <$> loadCampaignAutomationDTO automationId

insertStep
  :: UTCTime
  -> ME.CampaignAutomationId
  -> Text
  -> CampaignStepTemplate
  -> SqlPersistT IO ()
insertStep now automationId landingPath CampaignStepTemplate{..} =
  void $ insert ME.CampaignAutomationStep
    { ME.campaignAutomationStepAutomationId = automationId
    , ME.campaignAutomationStepPosition = cstPosition
    , ME.campaignAutomationStepDelayDays = cstDelayDays
    , ME.campaignAutomationStepChannel = "whatsapp"
    , ME.campaignAutomationStepProviderTemplateName = cstProviderTemplateName
    , ME.campaignAutomationStepLanguageCode = "es"
    , ME.campaignAutomationStepBody = cstBody
    , ME.campaignAutomationStepCtaPath = landingPath
    , ME.campaignAutomationStepActive = True
    , ME.campaignAutomationStepCreatedAt = now
    , ME.campaignAutomationStepUpdatedAt = now
    }

listCampaignAutomations :: SqlPersistT IO [CampaignAutomationDTO]
listCampaignAutomations = do
  rows <- selectList [] [Desc ME.CampaignAutomationUpdatedAt]
  mapM (loadCampaignAutomationDTO . entityKey) rows

loadCampaignAutomationDTO
  :: ME.CampaignAutomationId
  -> SqlPersistT IO CampaignAutomationDTO
loadCampaignAutomationDTO automationId = do
  automation <- getJust automationId
  campaign <- getJust (ME.campaignAutomationCampaignId automation)
  stepRows <-
    selectList
      [ ME.CampaignAutomationStepAutomationId ==. automationId
      , ME.CampaignAutomationStepActive ==. True
      ]
      [Asc ME.CampaignAutomationStepPosition]
  enrollmentCount <- count [ME.CampaignEnrollmentAutomationId ==. automationId]
  scheduledCount <-
    count
      [ ME.CampaignEnrollmentAutomationId ==. automationId
      , ME.CampaignEnrollmentStatus ==. "scheduled"
      ]
  stoppedCount <-
    count
      [ ME.CampaignEnrollmentAutomationId ==. automationId
      , ME.CampaignEnrollmentStatus <-. ["stopped", "replied"]
      ]
  convertedCount <-
    count
      [ ME.CampaignEnrollmentAutomationId ==. automationId
      , ME.CampaignEnrollmentStatus ==. "converted"
      ]
  sentCount <-
    count
      [ ME.CampaignDeliveryAutomationId ==. automationId
      , ME.CampaignDeliveryStatus ==. "sent"
      ]
  failedCount <-
    count
      [ ME.CampaignDeliveryAutomationId ==. automationId
      , ME.CampaignDeliveryStatus ==. "failed"
      ]
  pure CampaignAutomationDTO
    { caaId = fromSqlKey automationId
    , caaCampaignId = fromSqlKey (ME.campaignAutomationCampaignId automation)
    , caaTemplateKey = ME.campaignAutomationTemplateKey automation
    , caaName = ME.campaignName campaign
    , caaObjective = ME.campaignObjective campaign
    , caaStatus = ME.campaignAutomationStatus automation
    , caaStartAt = ME.campaignAutomationStartAt automation
    , caaDailyLimit = ME.campaignAutomationDailyLimit automation
    , caaLastRunAt = ME.campaignAutomationLastRunAt automation
    , caaEnrollmentCount = enrollmentCount
    , caaScheduledCount = scheduledCount
    , caaSentCount = sentCount
    , caaConvertedCount = convertedCount
    , caaStoppedCount = stoppedCount
    , caaFailedCount = failedCount
    , caaSteps = map persistedStepToDTO stepRows
    }

persistedStepToDTO :: Entity ME.CampaignAutomationStep -> CampaignAutomationStepDTO
persistedStepToDTO (Entity _ step) =
  CampaignAutomationStepDTO
    { casPosition = ME.campaignAutomationStepPosition step
    , casDelayDays = ME.campaignAutomationStepDelayDays step
    , casChannel = ME.campaignAutomationStepChannel step
    , casProviderTemplateName = ME.campaignAutomationStepProviderTemplateName step
    , casLanguageCode = ME.campaignAutomationStepLanguageCode step
    , casBody = ME.campaignAutomationStepBody step
    , casCtaPath = ME.campaignAutomationStepCtaPath step
    }

enrollCampaignParties
  :: UTCTime
  -> Int64
  -> CampaignAutomationEnroll
  -> SqlPersistT IO (Either Text CampaignAutomationEnrollResultDTO)
enrollCampaignParties now rawAutomationId CampaignAutomationEnroll{caePartyIds}
  | null partyIds =
      pure (Left "partyIds must include at least one contact")
  | length partyIds > 200 =
      pure (Left "partyIds must include 200 contacts or fewer")
  | otherwise = do
      let automationId = toSqlKey rawAutomationId
      mAutomation <- get automationId
      case mAutomation of
        Nothing -> pure (Left "Campaign automation not found")
        Just automation
          | ME.campaignAutomationStatus automation == "completed" ->
              pure (Left "Completed campaign automations cannot accept enrollments")
          | otherwise -> do
              results <- mapM (enrollOne now automationId automation) partyIds
              pure $
                Right CampaignAutomationEnrollResultDTO
                  { carAcceptedPartyIds =
                      [partyId | Right partyId <- results]
                  , carRejected =
                      [rejected | Left rejected <- results]
                  }
  where
    partyIds = nub caePartyIds

enrollOne
  :: UTCTime
  -> ME.CampaignAutomationId
  -> ME.CampaignAutomation
  -> Int64
  -> SqlPersistT IO (Either CampaignEnrollmentRejectedDTO Int64)
enrollOne now automationId automation rawPartyId
  | rawPartyId <= 0 =
      pure (reject "partyId must be a positive integer")
  | otherwise = do
      let partyId = toSqlKey rawPartyId
      mParty <- get partyId
      case mParty of
        Nothing -> pure (reject "Contact not found")
        Just party ->
          case partyWhatsAppPhone party of
            Nothing -> pure (reject "Contact does not have a valid WhatsApp phone")
            Just phone -> do
              consent <- getBy (ME.UniqueWhatsAppConsent phone)
              if not (activeConsent consent)
                then pure (reject "WhatsApp consent is missing or revoked")
                else do
                  existing <-
                    getBy (ME.UniqueCampaignEnrollment automationId partyId)
                  case existing of
                    Just _ -> pure (reject "Contact is already enrolled")
                    Nothing -> do
                      let firstRunAt =
                            max now (ME.campaignAutomationStartAt automation)
                      _ <- insert ME.CampaignEnrollment
                        { ME.campaignEnrollmentAutomationId = automationId
                        , ME.campaignEnrollmentPartyId = partyId
                        , ME.campaignEnrollmentStatus = "scheduled"
                        , ME.campaignEnrollmentNextStepPosition = 1
                        , ME.campaignEnrollmentNextRunAt = firstRunAt
                        , ME.campaignEnrollmentLastSentAt = Nothing
                        , ME.campaignEnrollmentStoppedAt = Nothing
                        , ME.campaignEnrollmentStopReason = Nothing
                        , ME.campaignEnrollmentCreatedAt = now
                        , ME.campaignEnrollmentUpdatedAt = now
                        }
                      pure (Right rawPartyId)
  where
    reject reason =
      Left CampaignEnrollmentRejectedDTO
        { cerPartyId = rawPartyId
        , cerReason = reason
        }

partyWhatsAppPhone :: M.Party -> Maybe Text
partyWhatsAppPhone party =
  (M.partyWhatsapp party <|> M.partyPrimaryPhone party)
    >>= normalizeWhatsAppPhone

activeConsent :: Maybe (Entity ME.WhatsAppConsent) -> Bool
activeConsent Nothing = False
activeConsent (Just (Entity _ consent)) =
  ME.whatsAppConsentConsent consent
    && isJust (ME.whatsAppConsentConsentedAt consent)
    && ME.whatsAppConsentRevokedAt consent == Nothing

isWhatsAppCampaignOptOutMessage :: Text -> Bool
isWhatsAppCampaignOptOutMessage rawMessage =
  normalized `elem` ["salir", "stop", "cancelar", "baja"]
  where
    normalized =
      T.toCaseFold
        (T.dropAround (not . isAlphaNum) (T.strip rawMessage))

applyWhatsAppCampaignOptOut
  :: UTCTime
  -> Text
  -> Maybe M.PartyId
  -> SqlPersistT IO ()
applyWhatsAppCampaignOptOut now rawPhone mInboundPartyId =
  case normalizeWhatsAppPhone rawPhone of
    Nothing -> pure ()
    Just phone -> do
      let consentRecord =
            ME.WhatsAppConsent
              { ME.whatsAppConsentPhoneE164 = phone
              , ME.whatsAppConsentDisplayName = Nothing
              , ME.whatsAppConsentConsent = False
              , ME.whatsAppConsentSource = Just "campaign_opt_out_keyword"
              , ME.whatsAppConsentNote = Just "Inbound WhatsApp opt-out keyword"
              , ME.whatsAppConsentConsentedAt = Nothing
              , ME.whatsAppConsentRevokedAt = Just now
              , ME.whatsAppConsentCreatedAt = now
              , ME.whatsAppConsentUpdatedAt = now
              }
      _ <-
        upsert consentRecord
          [ ME.WhatsAppConsentConsent =. False
          , ME.WhatsAppConsentSource =. Just "campaign_opt_out_keyword"
          , ME.WhatsAppConsentNote =. Just "Inbound WhatsApp opt-out keyword"
          , ME.WhatsAppConsentConsentedAt =. Nothing
          , ME.WhatsAppConsentRevokedAt =. Just now
          , ME.WhatsAppConsentUpdatedAt =. now
          ]
      phoneParties <-
        selectList
          [M.PartyWhatsapp <-. map Just (phoneLookupAliases phone)]
          []
      primaryPhoneParties <-
        selectList
          [M.PartyPrimaryPhone <-. map Just (phoneLookupAliases phone)]
          []
      let partyIds =
            nub
              ( catMaybes
                  [mInboundPartyId]
                  <> map entityKey phoneParties
                  <> map entityKey primaryPhoneParties
              )
      unless (null partyIds) $ do
        enrollments <-
          selectList
            [ ME.CampaignEnrollmentPartyId <-. partyIds
            , ME.CampaignEnrollmentStatus ==. "scheduled"
            ]
            []
        mapM_
          (\(Entity enrollmentId _) ->
            markEnrollmentStopped now enrollmentId "whatsapp_opt_out"
          )
          enrollments

listCampaignEnrollments
  :: Int64
  -> SqlPersistT IO (Either Text [CampaignEnrollmentDTO])
listCampaignEnrollments rawAutomationId = do
  let automationId = toSqlKey rawAutomationId
  mAutomation <- get automationId
  case mAutomation of
    Nothing -> pure (Left "Campaign automation not found")
    Just _ -> do
      rows <-
        selectList
          [ME.CampaignEnrollmentAutomationId ==. automationId]
          [Desc ME.CampaignEnrollmentUpdatedAt, LimitTo 500]
      Right <$> mapM enrollmentToDTO rows

enrollmentToDTO
  :: Entity ME.CampaignEnrollment
  -> SqlPersistT IO CampaignEnrollmentDTO
enrollmentToDTO (Entity enrollmentId enrollment) = do
  party <- getJust (ME.campaignEnrollmentPartyId enrollment)
  let phone = partyWhatsAppPhone party
  consent <-
    case phone of
      Nothing -> pure Nothing
      Just phoneValue -> getBy (ME.UniqueWhatsAppConsent phoneValue)
  pure CampaignEnrollmentDTO
    { cedId = fromSqlKey enrollmentId
    , cedPartyId = fromSqlKey (ME.campaignEnrollmentPartyId enrollment)
    , cedPartyName = M.partyDisplayName party
    , cedPhoneE164 = phone
    , cedConsentActive = activeConsent consent
    , cedStatus = ME.campaignEnrollmentStatus enrollment
    , cedNextStepPosition = ME.campaignEnrollmentNextStepPosition enrollment
    , cedNextRunAt = ME.campaignEnrollmentNextRunAt enrollment
    , cedLastSentAt = ME.campaignEnrollmentLastSentAt enrollment
    , cedStopReason = ME.campaignEnrollmentStopReason enrollment
    }

updateCampaignAutomationStatus
  :: UTCTime
  -> Int64
  -> CampaignAutomationStatusUpdate
  -> SqlPersistT IO (Either Text CampaignAutomationDTO)
updateCampaignAutomationStatus
  now
  rawAutomationId
  CampaignAutomationStatusUpdate{cauStatus, cauTemplatesApproved} =
  case validateCampaignAutomationActivation cauStatus cauTemplatesApproved of
    Left err -> pure (Left err)
    Right status -> do
      let automationId = toSqlKey rawAutomationId
      mAutomation <- get automationId
      case mAutomation of
        Nothing -> pure (Left "Campaign automation not found")
        Just automation -> do
          validation <- validateStatusTransition automationId automation status
          case validation of
            Left err -> pure (Left err)
            Right () -> do
              update automationId
                [ ME.CampaignAutomationStatus =. status
                , ME.CampaignAutomationUpdatedAt =. now
                ]
              update (ME.campaignAutomationCampaignId automation)
                [ ME.CampaignStatus =. status
                , ME.CampaignUpdatedAt =. now
                ]
              when (status == "completed") $
                stopScheduledEnrollments now automationId "campaign_completed"
              Right <$> loadCampaignAutomationDTO automationId

validateStatusTransition
  :: ME.CampaignAutomationId
  -> ME.CampaignAutomation
  -> Text
  -> SqlPersistT IO (Either Text ())
validateStatusTransition automationId automation newStatus
  | ME.campaignAutomationStatus automation == "completed"
      && newStatus /= "completed" =
      pure (Left "Completed campaign automations cannot be reactivated")
  | newStatus /= "active" =
      pure (Right ())
  | otherwise = do
      enrollmentCount <-
        count
          [ ME.CampaignEnrollmentAutomationId ==. automationId
          , ME.CampaignEnrollmentStatus ==. "scheduled"
          ]
      stepCount <-
        count
          [ ME.CampaignAutomationStepAutomationId ==. automationId
          , ME.CampaignAutomationStepActive ==. True
          ]
      pure $
        if enrollmentCount <= 0
          then Left "Enroll at least one consented contact before activation"
          else
            if stepCount <= 0
              then Left "Campaign automation has no active steps"
              else Right ()

stopScheduledEnrollments
  :: UTCTime
  -> ME.CampaignAutomationId
  -> Text
  -> SqlPersistT IO ()
stopScheduledEnrollments now automationId reason = do
  rows <-
    selectList
      [ ME.CampaignEnrollmentAutomationId ==. automationId
      , ME.CampaignEnrollmentStatus ==. "scheduled"
      ]
      []
  mapM_
    (\(Entity enrollmentId _) ->
      update enrollmentId
        [ ME.CampaignEnrollmentStatus =. "stopped"
        , ME.CampaignEnrollmentStoppedAt =. Just now
        , ME.CampaignEnrollmentStopReason =. Just reason
        , ME.CampaignEnrollmentUpdatedAt =. now
        ]
    )
    rows

updateCampaignEnrollmentStatus
  :: UTCTime
  -> Int64
  -> Int64
  -> CampaignEnrollmentStatusUpdate
  -> SqlPersistT IO (Either Text CampaignEnrollmentDTO)
updateCampaignEnrollmentStatus
  now
  rawAutomationId
  rawEnrollmentId
  CampaignEnrollmentStatusUpdate{cesStatus, cesReason} = do
    let automationId = toSqlKey rawAutomationId
        enrollmentId = toSqlKey rawEnrollmentId
        status = T.toLower (T.strip cesStatus)
    mEnrollment <- get enrollmentId
    case mEnrollment of
      Nothing -> pure (Left "Campaign enrollment not found")
      Just enrollment
        | ME.campaignEnrollmentAutomationId enrollment /= automationId ->
            pure (Left "Campaign enrollment does not belong to automation")
        | status `notElem` ["scheduled", "converted", "stopped"] ->
            pure (Left "Enrollment status must be scheduled, converted, or stopped")
        | otherwise -> do
            update enrollmentId
              [ ME.CampaignEnrollmentStatus =. status
              , ME.CampaignEnrollmentStoppedAt =.
                  if status == "scheduled" then Nothing else Just now
              , ME.CampaignEnrollmentStopReason =.
                  if status == "scheduled"
                    then Nothing
                    else Just (fromMaybe status (nonEmptyText cesReason))
              , ME.CampaignEnrollmentNextRunAt =.
                  if status == "scheduled"
                    then now
                    else ME.campaignEnrollmentNextRunAt enrollment
              , ME.CampaignEnrollmentUpdatedAt =. now
              ]
            Right <$> (getJustEntity enrollmentId >>= enrollmentToDTO)

previewCampaignAutomation
  :: AppConfig
  -> Int64
  -> SqlPersistT IO (Either Text [CampaignPreviewDTO])
previewCampaignAutomation cfg rawAutomationId = do
  let automationId = toSqlKey rawAutomationId
  mAutomation <- get automationId
  case mAutomation of
    Nothing -> pure (Left "Campaign automation not found")
    Just _ -> do
      enrollments <-
        selectList
          [ ME.CampaignEnrollmentAutomationId ==. automationId
          , ME.CampaignEnrollmentStatus ==. "scheduled"
          ]
          [Asc ME.CampaignEnrollmentNextRunAt, LimitTo 25]
      Right . catMaybes <$> mapM (previewEnrollment cfg) enrollments

previewEnrollment
  :: AppConfig
  -> Entity ME.CampaignEnrollment
  -> SqlPersistT IO (Maybe CampaignPreviewDTO)
previewEnrollment cfg (Entity _ enrollment) = do
  party <- getJust (ME.campaignEnrollmentPartyId enrollment)
  step <-
    getBy
      ( ME.UniqueCampaignAutomationStep
          (ME.campaignEnrollmentAutomationId enrollment)
          (ME.campaignEnrollmentNextStepPosition enrollment)
      )
  pure $ do
    Entity _ stepValue <- step
    let ctaUrl = absoluteCampaignUrl cfg (ME.campaignAutomationStepCtaPath stepValue)
    pure CampaignPreviewDTO
      { cpdPartyId = fromSqlKey (ME.campaignEnrollmentPartyId enrollment)
      , cpdPartyName = M.partyDisplayName party
      , cpdStepPosition = ME.campaignAutomationStepPosition stepValue
      , cpdProviderTemplateName =
          ME.campaignAutomationStepProviderTemplateName stepValue
      , cpdLanguageCode = ME.campaignAutomationStepLanguageCode stepValue
      , cpdRenderedBody =
          renderCampaignMessage
            (M.partyDisplayName party)
            ctaUrl
            (ME.campaignAutomationStepBody stepValue)
      , cpdCtaUrl = ctaUrl
      }

renderCampaignMessage :: Text -> Text -> Text -> Text
renderCampaignMessage partyName ctaUrl =
  T.replace "{{url}}" ctaUrl
    . T.replace "{{name}}" (fallbackName partyName)

fallbackName :: Text -> Text
fallbackName rawName =
  let name = T.strip rawName
  in if T.null name then "amigo de TDF" else name

absoluteCampaignUrl :: AppConfig -> Text -> Text
absoluteCampaignUrl cfg rawPath =
  let base = T.dropWhileEnd (== '/') (resolveConfiguredAppBase cfg)
      path = T.strip rawPath
  in if "/" `T.isPrefixOf` path
       then base <> path
       else base <> "/" <> path

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText Nothing = Nothing
nonEmptyText (Just rawValue) =
  let value = T.strip rawValue
  in if T.null value then Nothing else Just value

startCampaignAutomationJob :: Env -> IO ()
startCampaignAutomationJob env = do
  void (forkIO (campaignAutomationLoop env))
  LogBuf.addLog
    LogBuf.LogInfo
    "[Cron][CampaignAutomation] Scheduled consent-gated campaign checks every 60s."

campaignAutomationLoop :: Env -> IO ()
campaignAutomationLoop env = forever $ do
  result <- try (runCampaignAutomationWithLeaderLock env)
    :: IO (Either SomeException (Maybe CampaignTickStats))
  case result of
    Left err -> do
      let message =
            "[Cron][CampaignAutomation] Tick failed: " <> T.pack (show err)
      hPutStrLn stderr (T.unpack message)
      LogBuf.addLog LogBuf.LogError message
    Right Nothing -> pure ()
    Right (Just stats) ->
      when (stats /= mempty) $
        LogBuf.addLog
          LogBuf.LogInfo
          ( "[Cron][CampaignAutomation] Sent "
              <> T.pack (show (ctsSent stats))
              <> ", failed "
              <> T.pack (show (ctsFailed stats))
              <> ", stopped "
              <> T.pack (show (ctsStopped stats))
              <> "."
          )
  threadDelay (60 * 1000000)

runCampaignAutomationWithLeaderLock :: Env -> IO (Maybe CampaignTickStats)
runCampaignAutomationWithLeaderLock env@Env{envPool} =
  withCampaignAutomationLeaderLock envPool (runCampaignAutomationTick env)

withCampaignAutomationLeaderLock
  :: ConnectionPool
  -> IO a
  -> IO (Maybe a)
withCampaignAutomationLeaderLock pool action =
  withResource pool $ \backend -> do
    acquiredRows <-
      runSqlConn
        (rawSql "SELECT pg_try_advisory_lock(8401320260729)" [] ::
          SqlPersistT IO [Single Bool])
        backend
    case acquiredRows of
      [Single True] ->
        Just
          <$> ( action
                  `finally` void
                    ( runSqlConn
                        (rawSql "SELECT pg_advisory_unlock(8401320260729)" [] ::
                          SqlPersistT IO [Single Bool])
                        backend
                    )
              )
      _ -> pure Nothing

runCampaignAutomationTick :: Env -> IO CampaignTickStats
runCampaignAutomationTick env@Env{envPool} = do
  now <- getCurrentTime
  waEnv <- loadWhatsAppEnv
  automations <-
    runSqlPool
      ( selectList
          [ ME.CampaignAutomationStatus ==. "active"
          , ME.CampaignAutomationStartAt <=. now
          ]
          [Asc ME.CampaignAutomationCreatedAt]
      )
      envPool
  foldM (processAutomation env waEnv now) mempty automations

processAutomation
  :: Env
  -> WhatsAppEnv
  -> UTCTime
  -> CampaignTickStats
  -> Entity ME.CampaignAutomation
  -> IO CampaignTickStats
processAutomation env@Env{envPool} waEnv now aggregate (Entity automationId automation) = do
  attemptedToday <-
    runSqlPool
      ( count
          [ ME.CampaignDeliveryAutomationId ==. automationId
          , ME.CampaignDeliveryAttemptedAt >=. Just (UTCTime (utctDay now) 0)
          ]
      )
      envPool
  let remaining = max 0 (ME.campaignAutomationDailyLimit automation - attemptedToday)
  due <-
    if remaining <= 0
      then pure []
      else
        runSqlPool
          ( selectList
              [ ME.CampaignEnrollmentAutomationId ==. automationId
              , ME.CampaignEnrollmentStatus ==. "scheduled"
              , ME.CampaignEnrollmentNextRunAt <=. now
              ]
              [Asc ME.CampaignEnrollmentNextRunAt, LimitTo remaining]
          )
          envPool
  stats <- foldM (processEnrollment env waEnv now automationId) mempty due
  unless (null due) $
    runSqlPool
      ( update automationId
          [ ME.CampaignAutomationLastRunAt =. Just now
          , ME.CampaignAutomationUpdatedAt =. now
          ]
      )
      envPool
  pure (aggregate <> stats)

processEnrollment
  :: Env
  -> WhatsAppEnv
  -> UTCTime
  -> ME.CampaignAutomationId
  -> CampaignTickStats
  -> Entity ME.CampaignEnrollment
  -> IO CampaignTickStats
processEnrollment Env{envPool, envConfig} waEnv now automationId stats enrollmentEntity@(Entity enrollmentId enrollment) = do
  preparation <-
    runSqlPool
      (prepareEnrollmentSend now automationId enrollmentEntity envConfig)
      envPool
  case preparation of
    Left stopReason -> do
      runSqlPool (markEnrollmentStopped now enrollmentId stopReason) envPool
      pure (stats <> mempty { ctsStopped = 1 })
    Right Nothing ->
      pure stats
    Right (Just (stepEntity@(Entity stepId step), party, phone, ctaUrl, body)) -> do
      deliveryId <-
        runSqlPool
          (insertUnique ME.CampaignDelivery
            { ME.campaignDeliveryAutomationId = automationId
            , ME.campaignDeliveryEnrollmentId = enrollmentId
            , ME.campaignDeliveryStepId = stepId
            , ME.campaignDeliveryPartyId = ME.campaignEnrollmentPartyId enrollment
            , ME.campaignDeliveryChannel = "whatsapp"
            , ME.campaignDeliveryStatus = "pending"
            , ME.campaignDeliveryScheduledAt = ME.campaignEnrollmentNextRunAt enrollment
            , ME.campaignDeliveryAttemptedAt = Just now
            , ME.campaignDeliverySentAt = Nothing
            , ME.campaignDeliveryProviderMessageId = Nothing
            , ME.campaignDeliveryError = Nothing
            , ME.campaignDeliveryBodySnapshot = body
            , ME.campaignDeliveryCreatedAt = now
            , ME.campaignDeliveryUpdatedAt = now
            })
          envPool
      case deliveryId of
        Nothing -> do
          stoppedAmbiguousAttempt <-
            runSqlPool
              (stopAmbiguousPendingDelivery now enrollmentId stepId)
              envPool
          pure $
            if stoppedAmbiguousAttempt
              then stats <> mempty { ctsFailed = 1 }
              else stats
        Just deliveryKey -> do
          sendResult <-
            sendWhatsAppTemplateIO
              waEnv
              phone
              (ME.campaignAutomationStepProviderTemplateName step)
              (ME.campaignAutomationStepLanguageCode step)
              [fallbackName (M.partyDisplayName party), ctaUrl]
          _ <-
            runSqlPool
              (recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord
                { owrRecipientPhone = phone
                , owrRecipientPartyId = Just (ME.campaignEnrollmentPartyId enrollment)
                , owrRecipientName = Just (M.partyDisplayName party)
                , owrRecipientEmail = M.partyPrimaryEmail party
                , owrActorPartyId = Nothing
                , owrBody = body
                , owrSource = Just "campaign_automation"
                , owrReplyToMessageId = Nothing
                , owrReplyToExternalId = Nothing
                , owrResendOfMessageId = Nothing
                , owrMetadata =
                    Just
                      ( "automationId="
                          <> T.pack (show (fromSqlKey automationId))
                          <> ";step="
                          <> T.pack
                            (show (ME.campaignAutomationStepPosition step))
                      )
                }
                sendResult)
              envPool
          case sendResult of
            Left err -> do
              runSqlPool
                (do
                  update deliveryKey
                    [ ME.CampaignDeliveryStatus =. "failed"
                    , ME.CampaignDeliveryError =. Just (T.take 1000 err)
                    , ME.CampaignDeliveryUpdatedAt =. now
                    ]
                  update enrollmentId
                    [ ME.CampaignEnrollmentStatus =. "stopped"
                    , ME.CampaignEnrollmentStoppedAt =. Just now
                    , ME.CampaignEnrollmentStopReason =. Just "delivery_failed"
                    , ME.CampaignEnrollmentUpdatedAt =. now
                    ])
                envPool
              pure (stats <> mempty { ctsFailed = 1 })
            Right result -> do
              runSqlPool
                (completeSuccessfulDelivery now deliveryKey enrollmentId stepEntity result)
                envPool
              pure (stats <> mempty { ctsSent = 1 })

stopAmbiguousPendingDelivery
  :: UTCTime
  -> ME.CampaignEnrollmentId
  -> ME.CampaignAutomationStepId
  -> SqlPersistT IO Bool
stopAmbiguousPendingDelivery now enrollmentId stepId = do
  mDelivery <-
    getBy (ME.UniqueCampaignDelivery enrollmentId stepId)
  case mDelivery of
    Just (Entity deliveryId delivery)
      | ME.campaignDeliveryStatus delivery == "pending" -> do
          update deliveryId
            [ ME.CampaignDeliveryStatus =. "failed"
            , ME.CampaignDeliveryError =.
                Just "Interrupted attempt has unknown provider outcome; stopped to prevent a duplicate send"
            , ME.CampaignDeliveryUpdatedAt =. now
            ]
          update enrollmentId
            [ ME.CampaignEnrollmentStatus =. "stopped"
            , ME.CampaignEnrollmentStoppedAt =. Just now
            , ME.CampaignEnrollmentStopReason =. Just "delivery_outcome_unknown"
            , ME.CampaignEnrollmentUpdatedAt =. now
            ]
          pure True
    _ -> pure False

prepareEnrollmentSend
  :: UTCTime
  -> ME.CampaignAutomationId
  -> Entity ME.CampaignEnrollment
  -> AppConfig
  -> SqlPersistT
       IO
       (Either Text (Maybe (Entity ME.CampaignAutomationStep, M.Party, Text, Text, Text)))
prepareEnrollmentSend now automationId (Entity _ enrollment) cfg = do
  party <- getJust (ME.campaignEnrollmentPartyId enrollment)
  case partyWhatsAppPhone party of
    Nothing -> pure (Left "missing_whatsapp_phone")
    Just phone -> do
      consent <- getBy (ME.UniqueWhatsAppConsent phone)
      if not (activeConsent consent)
        then pure (Left "consent_revoked")
        else do
          replied <- hasInboundReply enrollment phone
          if replied
            then pure (Left "contact_replied")
            else do
              mStep <-
                getBy
                  ( ME.UniqueCampaignAutomationStep
                      automationId
                      (ME.campaignEnrollmentNextStepPosition enrollment)
                  )
              case mStep of
                Nothing -> pure (Left "sequence_completed")
                Just stepEntity@(Entity _ step)
                  | not (ME.campaignAutomationStepActive step) ->
                      pure (Left "sequence_step_disabled")
                  | ME.campaignEnrollmentNextRunAt enrollment > now ->
                      pure (Right Nothing)
                  | otherwise -> do
                      let ctaUrl =
                            absoluteCampaignUrl
                              cfg
                              (ME.campaignAutomationStepCtaPath step)
                          body =
                            renderCampaignMessage
                              (M.partyDisplayName party)
                              ctaUrl
                              (ME.campaignAutomationStepBody step)
                      pure (Right (Just (stepEntity, party, phone, ctaUrl, body)))

hasInboundReply :: ME.CampaignEnrollment -> Text -> SqlPersistT IO Bool
hasInboundReply enrollment phone =
  case ME.campaignEnrollmentLastSentAt enrollment of
    Nothing -> pure False
    Just lastSentAt -> do
      let partyId = ME.campaignEnrollmentPartyId enrollment
      byParty <-
        selectFirst
          [ ME.WhatsAppMessageDirection ==. "incoming"
          , ME.WhatsAppMessagePartyId ==. Just partyId
          , ME.WhatsAppMessageCreatedAt >. lastSentAt
          ]
          []
      case byParty of
        Just _ -> pure True
        Nothing -> do
          byPhone <-
            selectFirst
              [ ME.WhatsAppMessageDirection ==. "incoming"
              , ME.WhatsAppMessageSenderId <-. phoneLookupAliases phone
              , ME.WhatsAppMessageCreatedAt >. lastSentAt
              ]
              []
          pure (isJust byPhone)

markEnrollmentStopped
  :: UTCTime
  -> ME.CampaignEnrollmentId
  -> Text
  -> SqlPersistT IO ()
markEnrollmentStopped now enrollmentId reason =
  update enrollmentId
    [ ME.CampaignEnrollmentStatus =.
        if reason == "contact_replied" then "replied"
        else if reason == "sequence_completed" then "completed"
        else "stopped"
    , ME.CampaignEnrollmentStoppedAt =. Just now
    , ME.CampaignEnrollmentStopReason =. Just reason
    , ME.CampaignEnrollmentUpdatedAt =. now
    ]

completeSuccessfulDelivery
  :: UTCTime
  -> ME.CampaignDeliveryId
  -> ME.CampaignEnrollmentId
  -> Entity ME.CampaignAutomationStep
  -> SendTextResult
  -> SqlPersistT IO ()
completeSuccessfulDelivery now deliveryId enrollmentId (Entity _ step) sendResult = do
  update deliveryId
    [ ME.CampaignDeliveryStatus =. "sent"
    , ME.CampaignDeliverySentAt =. Just now
    , ME.CampaignDeliveryProviderMessageId =. sendTextMessageId sendResult
    , ME.CampaignDeliveryUpdatedAt =. now
    ]
  let nextPosition = ME.campaignAutomationStepPosition step + 1
  mNextStep <-
    getBy
      ( ME.UniqueCampaignAutomationStep
          (ME.campaignAutomationStepAutomationId step)
          nextPosition
      )
  case mNextStep of
    Nothing ->
      update enrollmentId
        [ ME.CampaignEnrollmentStatus =. "completed"
        , ME.CampaignEnrollmentNextStepPosition =. nextPosition
        , ME.CampaignEnrollmentLastSentAt =. Just now
        , ME.CampaignEnrollmentStoppedAt =. Just now
        , ME.CampaignEnrollmentStopReason =. Just "sequence_completed"
        , ME.CampaignEnrollmentUpdatedAt =. now
        ]
    Just (Entity _ nextStep) ->
      let delaySeconds =
            fromIntegral (ME.campaignAutomationStepDelayDays nextStep * 86400)
      in update enrollmentId
          [ ME.CampaignEnrollmentNextStepPosition =. nextPosition
          , ME.CampaignEnrollmentNextRunAt =. addUTCTime delaySeconds now
          , ME.CampaignEnrollmentLastSentAt =. Just now
          , ME.CampaignEnrollmentUpdatedAt =. now
          ]
