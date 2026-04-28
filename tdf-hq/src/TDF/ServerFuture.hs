{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerFuture where

import           Control.Monad.Except (MonadError)
import           Data.Char
  ( GeneralCategory(Format, LineSeparator, ParagraphSeparator)
  , generalCategory
  , isControl
  )
import           Data.List            (nub)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Servant

import           TDF.API.Future
import           TDF.Auth
  ( AuthedUser
  , ModuleAccess(ModuleAdmin)
  , hasModuleAccess
  , hasStrictAdminAccess
  )

-- | Shared helper to quickly craft stub responses.
stub
  :: MonadError ServerError m
  => Text
  -> Text
  -> m StubResponse
stub rawArea rawEndpoint = do
  (area, endpoint) <-
    either throwError pure (validateFutureStubMetadata rawArea rawEndpoint)
  either throwError pure $
    validateFutureStubResponse $
      StubResponse
        { stubArea        = area
        , stubEndpoint    = endpoint
        , stubStatus      = "planned"
        , stubImplemented = False
        }

futureServer
  :: MonadError ServerError m
  => AuthedUser
  -> ServerT FutureAPI m
futureServer user = accessStubs
          :<|> crmStubs
          :<|> schedulingStubs
          :<|> packagesStubs
          :<|> invoicingStubs
          :<|> inventoryStubs
          :<|> adminStubs
          :<|> crossCuttingStubs
  where
    adminStub area endpoint =
      requireFutureAdminAccess user *> stub area endpoint

    accessStubs =    adminStub "access" "login-options"
                :<|> adminStub "access" "module-behaviour"
                :<|> adminStub "access" "session-policy"

    crmStubs =       adminStub "crm" "parties/list-columns"
                :<|> adminStub "crm" "parties/filters"
                :<|> adminStub "crm" "parties/detail-tabs"

    schedulingStubs =    adminStub "scheduling" "bookings/views"
                    :<|> adminStub "scheduling" "sessions/creation"
                    :<|> adminStub "scheduling" "rooms/features"

    packagesStubs =  adminStub "packages" "catalog"
                :<|> adminStub "packages" "purchase-flow"

    invoicingStubs = adminStub "invoicing" "composer"
                :<|> adminStub "invoicing" "status-flow"

    inventoryStubs = adminStub "inventory" "assets/metadata"
                :<|> adminStub "inventory" "assets/workflow"
                :<|> adminStub "inventory" "stock"

    adminStubs =     adminStub "admin" "seed"
                :<|> adminConsole

    adminConsole = do
      requireFutureAdminAccess user
      either throwError pure $
        validateFutureAdminConsoleView $
        AdminConsoleView
          { status = "preview"
          , cards = adminConsoleCards
          }

    crossCuttingStubs = adminStub "experience" "navigation"
                    :<|> adminStub "experience" "feedback"
                    :<|> adminStub "experience" "offline"
                    :<|> adminStub "experience" "design"
                    :<|> adminStub "experience" "auditing"

adminConsoleCards :: [AdminConsoleCard]
adminConsoleCards =
  [ AdminConsoleCard
      { cardId = "user-management"
      , title  = "Gestión de usuarios"
      , body   =
          [ "La asignación de roles se administra desde la pantalla de Parties."
          , "Próximamente aquí se podrá crear usuarios de servicio y tokens API."
          ]
      }
  , AdminConsoleCard
      { cardId = "api-tokens"
      , title  = "Tokens API"
      , body   =
          [ "Los tokens de servicio deben administrarse desde un flujo dedicado."
          , "El acceso quedará separado de usuarios humanos para integraciones internas."
          ]
      }
  ]

validateFutureAdminAccess :: AuthedUser -> Either ServerError ()
validateFutureAdminAccess user
  | not (hasStrictAdminAccess user) = Left err403 { errBody = "Admin role required" }
  | hasModuleAccess ModuleAdmin user = Right ()
  | otherwise = Left err403 { errBody = "Admin module access required" }

requireFutureAdminAccess :: MonadError ServerError m => AuthedUser -> m ()
requireFutureAdminAccess user =
  either throwError pure (validateFutureAdminAccess user)

validateFutureStubMetadata :: Text -> Text -> Either ServerError (Text, Text)
validateFutureStubMetadata rawArea rawEndpoint = do
  catalog <- validateFutureStubCatalog allowedFutureStubMetadata
  validateFutureStubMetadataIn catalog rawArea rawEndpoint

validateFutureStubMetadataIn
  :: [(Text, Text)]
  -> Text
  -> Text
  -> Either ServerError (Text, Text)
validateFutureStubMetadataIn catalog rawArea rawEndpoint = do
  area <- validateFutureStubArea rawArea
  endpoint <- validateFutureStubEndpoint rawEndpoint
  if (area, endpoint) `elem` catalog
    then pure (area, endpoint)
    else invalidFutureStubMetadata

validateFutureStubCatalog :: [(Text, Text)] -> Either ServerError [(Text, Text)]
validateFutureStubCatalog catalog = do
  normalized <-
    either (const invalidFutureStubCatalog) Right $
      traverse validateFutureStubCatalogEntry catalog
  if normalized /= allowedFutureStubMetadata || length normalized /= length (nub normalized)
    then invalidFutureStubCatalog
    else Right normalized

validateFutureStubCatalogEntry :: (Text, Text) -> Either ServerError (Text, Text)
validateFutureStubCatalogEntry (area, endpoint) = do
  areaClean <- validateFutureStubArea area
  endpointClean <- validateFutureStubEndpoint endpoint
  pure (areaClean, endpointClean)

validateFutureStubResponse :: StubResponse -> Either ServerError StubResponse
validateFutureStubResponse response =
  case validateFutureStubMetadata (stubArea response) (stubEndpoint response) of
    Left _ -> invalidFutureStubResponse
    Right (area, endpoint)
      | stubStatus response /= "planned" -> invalidFutureStubResponse
      | stubImplemented response -> invalidFutureStubResponse
      | otherwise ->
          Right response
            { stubArea = area
            , stubEndpoint = endpoint
            }

validateFutureAdminConsoleCard :: AdminConsoleCard -> Either ServerError AdminConsoleCard
validateFutureAdminConsoleCard card
  | not (validFutureStubSlug (cardId card)) = invalidFutureAdminConsoleMetadata
  | cardId card `notElem` allowedFutureAdminConsoleCardIds =
      invalidFutureAdminConsoleMetadata
  | title card /= expectedFutureAdminConsoleTitle (cardId card) =
      invalidFutureAdminConsoleMetadata
  | body card /= expectedFutureAdminConsoleBody (cardId card) =
      invalidFutureAdminConsoleMetadata
  | invalidCardText 120 (title card) = invalidFutureAdminConsoleMetadata
  | null (body card) || length (body card) > 8 = invalidFutureAdminConsoleMetadata
  | any (invalidCardText 240) (body card) = invalidFutureAdminConsoleMetadata
  | hasDuplicateFutureAdminConsoleBodyLines card = invalidFutureAdminConsoleMetadata
  | otherwise = Right card

expectedFutureAdminConsoleTitle :: Text -> Text
expectedFutureAdminConsoleTitle cardIdValue
  | cardIdValue == "user-management" = "Gestión de usuarios"
  | cardIdValue == "api-tokens" = "Tokens API"
  | otherwise = ""

expectedFutureAdminConsoleBody :: Text -> [Text]
expectedFutureAdminConsoleBody cardIdValue
  | cardIdValue == "user-management" =
      [ "La asignación de roles se administra desde la pantalla de Parties."
      , "Próximamente aquí se podrá crear usuarios de servicio y tokens API."
      ]
  | cardIdValue == "api-tokens" =
      [ "Los tokens de servicio deben administrarse desde un flujo dedicado."
      , "El acceso quedará separado de usuarios humanos para integraciones internas."
      ]
  | otherwise = []

allowedFutureAdminConsoleCardIds :: [Text]
allowedFutureAdminConsoleCardIds =
  [ "user-management"
  , "api-tokens"
  ]

validateFutureAdminConsoleView :: AdminConsoleView -> Either ServerError AdminConsoleView
validateFutureAdminConsoleView view
  | status view /= "preview" = invalidFutureAdminConsoleMetadata
  | otherwise = do
      validatedCards <- traverse validateFutureAdminConsoleCard (cards view)
      if map cardId validatedCards /= allowedFutureAdminConsoleCardIds
           || hasDuplicateFutureAdminConsoleTitles validatedCards
        then invalidFutureAdminConsoleMetadata
        else Right view { cards = validatedCards }

hasDuplicateFutureAdminConsoleTitles :: [AdminConsoleCard] -> Bool
hasDuplicateFutureAdminConsoleTitles cardsValue =
  let normalizedTitles = map (T.toCaseFold . title) cardsValue
  in length normalizedTitles /= length (nub normalizedTitles)

hasDuplicateFutureAdminConsoleBodyLines :: AdminConsoleCard -> Bool
hasDuplicateFutureAdminConsoleBodyLines card =
  let normalizedBodyLines = map T.toCaseFold (body card)
  in length normalizedBodyLines /= length (nub normalizedBodyLines)

invalidCardText :: Int -> Text -> Bool
invalidCardText maxLength value =
  T.null stripped
    || value /= stripped
    || T.length value > maxLength
    || T.any invalidCardChar value
  where
    stripped = T.strip value
    invalidCardChar ch =
      isControl ch
        || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

allowedFutureStubMetadata :: [(Text, Text)]
allowedFutureStubMetadata =
  [ ("access", "login-options")
  , ("access", "module-behaviour")
  , ("access", "session-policy")
  , ("crm", "parties/list-columns")
  , ("crm", "parties/filters")
  , ("crm", "parties/detail-tabs")
  , ("scheduling", "bookings/views")
  , ("scheduling", "sessions/creation")
  , ("scheduling", "rooms/features")
  , ("packages", "catalog")
  , ("packages", "purchase-flow")
  , ("invoicing", "composer")
  , ("invoicing", "status-flow")
  , ("inventory", "assets/metadata")
  , ("inventory", "assets/workflow")
  , ("inventory", "stock")
  , ("admin", "seed")
  , ("experience", "navigation")
  , ("experience", "feedback")
  , ("experience", "offline")
  , ("experience", "design")
  , ("experience", "auditing")
  ]

validateFutureStubArea :: Text -> Either ServerError Text
validateFutureStubArea rawArea
  | rawArea /= area = invalidFutureStubMetadata
  | validFutureStubSlug area = Right area
  | otherwise = invalidFutureStubMetadata
  where
    area = T.strip rawArea

validateFutureStubEndpoint :: Text -> Either ServerError Text
validateFutureStubEndpoint rawEndpoint
  | rawEndpoint /= endpoint = invalidFutureStubMetadata
  | validFutureStubPath endpoint = Right endpoint
  | otherwise = invalidFutureStubMetadata
  where
    endpoint = T.strip rawEndpoint

validFutureStubPath :: Text -> Bool
validFutureStubPath endpoint =
  not (T.null endpoint)
    && T.length endpoint <= 128
    && all validFutureStubSlug (T.splitOn "/" endpoint)

validFutureStubSlug :: Text -> Bool
validFutureStubSlug slug =
  not (T.null slug)
    && T.length slug <= 64
    && T.head slug /= '-'
    && T.last slug /= '-'
    && not ("--" `T.isInfixOf` slug)
    && T.all validFutureStubSlugChar slug

validFutureStubSlugChar :: Char -> Bool
validFutureStubSlugChar ch =
  (ch >= 'a' && ch <= 'z')
    || (ch >= '0' && ch <= '9')
    || ch == '-'

invalidFutureStubMetadata :: Either ServerError a
invalidFutureStubMetadata =
  Left err500 { errBody = "Invalid future stub metadata" }

invalidFutureStubCatalog :: Either ServerError a
invalidFutureStubCatalog =
  Left err500 { errBody = "Invalid future stub catalog" }

invalidFutureAdminConsoleMetadata :: Either ServerError a
invalidFutureAdminConsoleMetadata =
  Left err500 { errBody = "Invalid future admin console metadata" }

invalidFutureStubResponse :: Either ServerError a
invalidFutureStubResponse =
  Left err500 { errBody = "Invalid future stub response" }
