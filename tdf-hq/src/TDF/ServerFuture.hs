{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerFuture where

import           Control.Monad.Except (MonadError)
import qualified Data.ByteString.Lazy as BL
import           Data.Char
  ( GeneralCategory
      ( EnclosingMark
      , Format
      , LineSeparator
      , NonSpacingMark
      , ParagraphSeparator
      , Space
      , SpacingCombiningMark
      )
  , generalCategory
  , isControl
  )
import           Data.List            (group, isPrefixOf, nub)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Database.Persist.Sql (fromSqlKey)
import           Servant

import           TDF.API.Future
import           TDF.Auth
  ( AuthedUser(..)
  , ModuleAccess(ModuleAdmin)
  , moduleName
  , modulesForRoles
  )
import           TDF.Models (RoleEnum(Admin, Customer, Fan), roleToText)

-- | Shared helper to quickly craft stub responses.
stub
  :: MonadError ServerError m
  => Text
  -> Text
  -> m StubResponse
stub rawArea rawEndpoint =
  either throwError pure $
    futureStubResponseForWithConsole futureAdminConsoleView rawArea rawEndpoint

futureServer
  :: MonadError ServerError m
  => AuthedUser
  -> ServerT FutureAPI m
futureServer user = futureCatalog
          :<|> accessStubs
          :<|> crmStubs
          :<|> schedulingStubs
          :<|> packagesStubs
          :<|> invoicingStubs
          :<|> inventoryStubs
          :<|> adminStubs
          :<|> crossCuttingStubs
  where
    futureCatalog =
      requireFutureAdminAccess user *> futureStubCatalogResponse

    adminStubEntry (area, endpoint) =
      requireFutureAdminAccess user *> stub area endpoint

    accessStubs =    adminStubEntry accessLoginOptionsStub
                :<|> adminStubEntry accessModuleBehaviourStub
                :<|> adminStubEntry accessSessionPolicyStub

    crmStubs =       adminStubEntry crmPartiesListColumnsStub
                :<|> adminStubEntry crmPartiesFiltersStub
                :<|> adminStubEntry crmPartiesDetailTabsStub

    schedulingStubs =    adminStubEntry schedulingBookingsViewsStub
                    :<|> adminStubEntry schedulingSessionsCreationStub
                    :<|> adminStubEntry schedulingRoomsFeaturesStub

    packagesStubs =  adminStubEntry packagesCatalogStub
                :<|> adminStubEntry packagesPurchaseFlowStub

    invoicingStubs = adminStubEntry invoicingComposerStub
                :<|> adminStubEntry invoicingStatusFlowStub

    inventoryStubs = adminStubEntry inventoryAssetsMetadataStub
                :<|> adminStubEntry inventoryAssetsWorkflowStub
                :<|> adminStubEntry inventoryStockStub

    adminStubs =     adminStubEntry adminSeedPolicyStub
                :<|> adminConsole

    adminConsole = do
      requireFutureAdminAccess user
      either throwError pure $
        validateFutureAdminConsoleViewWithCatalog
          allowedFutureStubMetadata
          futureAdminConsoleView

    crossCuttingStubs = adminStubEntry experienceNavigationStub
                    :<|> adminStubEntry experienceFeedbackStub
                    :<|> adminStubEntry experienceOfflineStub
                    :<|> adminStubEntry experienceDesignStub
                    :<|> adminStubEntry experienceAuditingStub

futureAdminConsoleView :: AdminConsoleView
futureAdminConsoleView =
  AdminConsoleView
    { viewArea = "admin"
    , viewEndpoint = "console"
    , viewId = futureStubId "admin" "console"
    , viewPath = futureStubPath "admin" "console"
    , viewMethod = futureStubMethod
    , viewStatus = "preview"
    , viewRequiredRole = futureStubRequiredRole
    , viewRequiredModule = futureStubRequiredModule
    , viewImplemented = False
    , cards = adminConsoleCards
    }

adminConsoleCards :: [AdminConsoleCard]
adminConsoleCards =
  [ AdminConsoleCard
      { cardId = "user-management"
      , title  = "Gestión de usuarios"
      , body   =
          [ "La asignación de roles se administra desde la pantalla de Parties."
          , "Próximamente aquí se podrá crear usuarios de servicio y tokens API."
          ]
      , implemented = False
      }
  , AdminConsoleCard
      { cardId = "api-tokens"
      , title  = "Tokens API"
      , body   =
          [ "Los tokens de servicio deben administrarse desde un flujo dedicado."
          , "El acceso quedará separado de usuarios humanos para integraciones internas."
          ]
      , implemented = False
      }
  ]

validateFutureAdminAccess :: AuthedUser -> Either ServerError ()
validateFutureAdminAccess user
  | Admin `notElem` auRoles user = Left err403 { errBody = "Admin role required" }
  | fromSqlKey (auPartyId user) <= 0 =
      Left err403 { errBody = "Valid admin party required" }
  | length (auRoles user) /= length (nub (auRoles user)) =
      Left err403 { errBody = "Admin role grants must be unique" }
  | any (not . isFutureAdminRoleScope) (auRoles user) =
      Left err403
        { errBody = "Admin fallback discovery cannot be combined with non-baseline roles" }
  | not (null missingBaselineRoles) =
      Left err403
        { errBody =
            textBody $
              "Admin fallback discovery requires baseline roles; missing: "
                <> T.intercalate ", " (map roleToText missingBaselineRoles)
        }
  | not (ModuleAdmin `Set.member` auModules user) =
      Left err403 { errBody = "Admin module access required" }
  | auModules user /= modulesForRoles (auRoles user) =
      Left err403 { errBody = "Admin module grants must match roles" }
  | otherwise = Right ()
  where
    missingBaselineRoles =
      filter (`notElem` auRoles user) requiredFutureAdminBaselineRoles

textBody :: Text -> BL.ByteString
textBody =
  BL.fromStrict . TE.encodeUtf8

isFutureAdminRoleScope :: RoleEnum -> Bool
isFutureAdminRoleScope role =
  role `elem` [Admin, Fan, Customer]

requiredFutureAdminBaselineRoles :: [RoleEnum]
requiredFutureAdminBaselineRoles =
  [ Fan
  , Customer
  ]

requireFutureAdminAccess :: MonadError ServerError m => AuthedUser -> m ()
requireFutureAdminAccess user =
  either throwError pure (validateFutureAdminAccess user)

validateFutureStubMetadata :: Text -> Text -> Either ServerError (Text, Text)
validateFutureStubMetadata =
  validateFutureStubMetadataIn allowedFutureStubMetadata

validateFutureStubMetadataIn
  :: [(Text, Text)]
  -> Text
  -> Text
  -> Either ServerError (Text, Text)
validateFutureStubMetadataIn catalog rawArea rawEndpoint = do
  validatedCatalog <- validateFutureStubCatalog catalog
  area <- validateFutureStubArea rawArea
  endpoint <- validateFutureStubEndpoint rawEndpoint
  if (area, endpoint) `elem` validatedCatalog
    then pure (area, endpoint)
    else invalidFutureStubMetadata

validateFutureStubCatalog :: [(Text, Text)] -> Either ServerError [(Text, Text)]
validateFutureStubCatalog catalog = do
  reservedRoutes <- validateReservedFutureStubRoutes reservedFutureStubRoutes
  normalized <-
    either (const invalidFutureStubCatalog) Right $
      traverse validateFutureStubCatalogEntry catalog
  _ <- validateFutureStubCatalogRouteBoundaries reservedRoutes normalized
  _ <- validateFutureStubCatalogAreaOrder normalized
  if normalized /= allowedFutureStubMetadata || length normalized /= length (nub normalized)
    then invalidFutureStubCatalog
    else Right normalized

validateReservedFutureStubRoutes :: [(Text, Text)] -> Either ServerError [(Text, Text)]
validateReservedFutureStubRoutes routes = do
  normalized <-
    either (const invalidFutureStubCatalog) Right $
      traverse validateReservedFutureStubRoute routes
  if null normalized
       || normalized /= requiredReservedFutureStubRoutes
       || length normalized /= length (nub normalized)
       || any (`elem` allowedFutureStubMetadata) normalized
    then invalidFutureStubCatalog
    else Right normalized

validateReservedFutureStubRoute :: (Text, Text) -> Either ServerError (Text, Text)
validateReservedFutureStubRoute (area, endpoint) = do
  areaClean <- validateFutureStubArea area
  endpointClean <- validateFutureStubEndpoint endpoint
  pure (areaClean, endpointClean)

validateFutureStubCatalogRouteBoundaries
  :: [(Text, Text)]
  -> [(Text, Text)]
  -> Either ServerError [(Text, Text)]
validateFutureStubCatalogRouteBoundaries reservedRoutes catalog = do
  validatedReservedRoutes <- validateReservedFutureStubRoutes reservedRoutes
  validatedCatalog <-
    either (const invalidFutureStubCatalog) Right $
      traverse validateFutureStubBoundaryRoute catalog
  if length validatedReservedRoutes /= length (nub validatedReservedRoutes)
       || length validatedCatalog /= length (nub validatedCatalog)
       || any routesOverlap
            [ (reservedRoute, catalogRoute)
            | reservedRoute <- validatedReservedRoutes
            , catalogRoute <- validatedCatalog
            ]
    then invalidFutureStubCatalog
    else Right validatedCatalog
  where
    validateFutureStubBoundaryRoute (area, endpoint) = do
      areaClean <- validateFutureStubArea area
      endpointClean <- validateFutureStubEndpoint endpoint
      pure (areaClean, endpointClean)

    routesOverlap ((reservedArea, reservedEndpoint), (area, endpoint)) =
      area == reservedArea
        && ( reservedSegments `isPrefixOf` endpointSegments
             || endpointSegments `isPrefixOf` reservedSegments
           )
      where
        reservedSegments = T.splitOn "/" reservedEndpoint
        endpointSegments = T.splitOn "/" endpoint

validateFutureStubCatalogEntry :: (Text, Text) -> Either ServerError (Text, Text)
validateFutureStubCatalogEntry (area, endpoint) = do
  areaClean <- validateFutureStubArea area
  endpointClean <- validateFutureStubEndpoint endpoint
  if (areaClean, endpointClean) `elem` reservedFutureStubRoutes
    then invalidFutureStubMetadata
    else if (areaClean, endpointClean) `notElem` allowedFutureStubMetadata
      then invalidFutureStubMetadata
    else pure (areaClean, endpointClean)

validateFutureStubCatalogAreaOrder :: [(Text, Text)] -> Either ServerError [Text]
validateFutureStubCatalogAreaOrder catalog = do
  validatedCatalog <-
    either (const invalidFutureStubCatalog) Right $
      traverse validateFutureStubCatalogEntry catalog
  let areaGroups = group (map fst validatedCatalog)
      expectedAreaGroups = group (map fst allowedFutureStubMetadata)
      areaRuns = map head areaGroups
      entriesFor area = filter ((== area) . fst) validatedCatalog
      expectedEntriesFor area = filter ((== area) . fst) allowedFutureStubMetadata
      areaEntriesMatch =
        all
          (\area -> entriesFor area == expectedEntriesFor area)
          mountedFutureStubAreas
  if length validatedCatalog /= length (nub validatedCatalog)
       || areaRuns /= mountedFutureStubAreas
       || map length areaGroups /= map length expectedAreaGroups
       || length areaRuns /= length (nub areaRuns)
       || not areaEntriesMatch
    then invalidFutureStubCatalog
    else Right areaRuns

reservedFutureStubRoutes :: [(Text, Text)]
reservedFutureStubRoutes =
  requiredReservedFutureStubRoutes

requiredReservedFutureStubRoutes :: [(Text, Text)]
requiredReservedFutureStubRoutes =
  [ ("admin", "console")
  , ("admin", "seed")
  ]

validateFutureStubResponse :: StubResponse -> Either ServerError StubResponse
validateFutureStubResponse response =
  case validateFutureStubMetadata (stubArea response) (stubEndpoint response) of
    Left _ -> invalidFutureStubResponse
    Right (area, endpoint)
      | stubMethod response /= futureStubMethod -> invalidFutureStubResponse
      | stubStatus response /= "planned" -> invalidFutureStubResponse
      | stubRequiredRole response /= futureStubRequiredRole -> invalidFutureStubResponse
      | stubRequiredModule response /= futureStubRequiredModule -> invalidFutureStubResponse
      | stubImplemented response -> invalidFutureStubResponse
      | otherwise -> do
          responseId <- validateFutureStubPublishedId area endpoint (stubId response)
          path <- validateFutureStubPublishedPath area endpoint (stubPath response)
          Right response
            { stubArea = area
            , stubEndpoint = endpoint
            , stubId = responseId
            , stubPath = path
            , stubMethod = futureStubMethod
            , stubRequiredRole = futureStubRequiredRole
            , stubRequiredModule = futureStubRequiredModule
            }

validateFutureStubPublishedId
  :: Text
  -> Text
  -> Text
  -> Either ServerError Text
validateFutureStubPublishedId rawArea rawEndpoint rawId =
  case validateFutureStubMetadata rawArea rawEndpoint of
    Left _ -> invalidFutureStubResponse
    Right (area, endpoint)
      | isFuturePublishedIdShape area endpoint rawId -> Right rawId
      | otherwise -> invalidFutureStubResponse

validateFutureAdminConsolePublishedId :: Text -> Either ServerError Text
validateFutureAdminConsolePublishedId rawId = do
  (area, endpoint) <- validateFutureAdminConsoleRoute
  if isFuturePublishedIdShape area endpoint rawId
    then Right rawId
    else invalidFutureAdminConsoleMetadata

isFuturePublishedIdShape :: Text -> Text -> Text -> Bool
isFuturePublishedIdShape area endpoint rawId =
  rawId == expectedId
    && idSegments == expectedSegments
    && all validFutureStubSlug idSegments
  where
    endpointSegments = T.splitOn "/" endpoint
    expectedSegments = area : endpointSegments
    idSegments = T.splitOn "." rawId
    expectedId = T.intercalate "." expectedSegments

validateFutureStubPublishedPath
  :: Text
  -> Text
  -> Text
  -> Either ServerError Text
validateFutureStubPublishedPath rawArea rawEndpoint rawPath =
  case validateFutureStubMetadata rawArea rawEndpoint of
    Left _ -> invalidFutureStubResponse
    Right (area, endpoint)
      | isFuturePublishedPathShape area endpoint rawPath -> Right rawPath
      | otherwise -> invalidFutureStubResponse

validateFutureAdminConsolePublishedPath :: Text -> Either ServerError Text
validateFutureAdminConsolePublishedPath rawPath = do
  (area, endpoint) <- validateFutureAdminConsoleRoute
  if isFuturePublishedPathShape area endpoint rawPath
    then Right rawPath
    else invalidFutureAdminConsoleMetadata

validateFutureAdminConsoleRoute :: Either ServerError (Text, Text)
validateFutureAdminConsoleRoute =
  validateFutureAdminConsoleRouteIn reservedFutureStubRoutes

validateFutureAdminConsoleRouteIn
  :: [(Text, Text)]
  -> Either ServerError (Text, Text)
validateFutureAdminConsoleRouteIn routes =
  case validateReservedFutureStubRoutes routes of
    Left _ -> invalidFutureAdminConsoleMetadata
    Right reservedRoutes ->
      if route `elem` reservedRoutes
        then Right route
        else invalidFutureAdminConsoleMetadata
  where
    route = ("admin", "console")

isFuturePublishedPathShape :: Text -> Text -> Text -> Bool
isFuturePublishedPathShape area endpoint rawPath =
  rawPath == futureStubPath area endpoint
    && pathSegments == area : endpointSegments
    && all validFutureStubSlug pathSegments
  where
    endpointSegments = T.splitOn "/" endpoint
    pathSegments =
      case T.stripPrefix "/stubs/" rawPath of
        Nothing -> []
        Just suffix -> T.splitOn "/" suffix

futureStubCatalogResponse
  :: MonadError ServerError m
  => m [StubResponse]
futureStubCatalogResponse =
  either throwError pure validateFutureStubCatalogResponse

validateFutureStubCatalogResponse :: Either ServerError [StubResponse]
validateFutureStubCatalogResponse =
  validateFutureStubCatalogResponseWithConsole futureAdminConsoleView

validateFutureStubCatalogResponseWithConsole
  :: AdminConsoleView
  -> Either ServerError [StubResponse]
validateFutureStubCatalogResponseWithConsole consoleView = do
  catalog <- validateFutureStubCatalog allowedFutureStubMetadata
  _ <- validateFutureAdminConsoleViewWithCatalog catalog consoleView
  responses <- traverse (uncurry futureStubResponseFor) catalog
  validateFutureStubCatalogResponses responses

validateFutureStubCatalogResponses :: [StubResponse] -> Either ServerError [StubResponse]
validateFutureStubCatalogResponses responses = do
  validatedResponses <-
    either (const invalidFutureStubCatalog) Right $
      traverse validateFutureStubResponse responses
  let metadata = map (\response -> (stubArea response, stubEndpoint response)) validatedResponses
      ids = map stubId validatedResponses
      paths = map stubPath validatedResponses
  if metadata /= allowedFutureStubMetadata
       || length metadata /= length (nub metadata)
       || length ids /= length (nub ids)
       || length paths /= length (nub paths)
    then invalidFutureStubCatalog
    else Right validatedResponses

futureStubResponseFor :: Text -> Text -> Either ServerError StubResponse
futureStubResponseFor rawArea rawEndpoint = do
  (area, endpoint) <- validateFutureStubMetadata rawArea rawEndpoint
  validateFutureStubResponse $
    StubResponse
      { stubArea = area
      , stubEndpoint = endpoint
      , stubId = futureStubId area endpoint
      , stubPath = futureStubPath area endpoint
      , stubMethod = futureStubMethod
      , stubStatus = "planned"
      , stubRequiredRole = futureStubRequiredRole
      , stubRequiredModule = futureStubRequiredModule
      , stubImplemented = False
      }

futureStubResponseForWithConsole
  :: AdminConsoleView
  -> Text
  -> Text
  -> Either ServerError StubResponse
futureStubResponseForWithConsole consoleView rawArea rawEndpoint = do
  response <- futureStubResponseFor rawArea rawEndpoint
  _ <- validateFutureAdminConsoleViewWithCatalog allowedFutureStubMetadata consoleView
  Right response

futureStubPath :: Text -> Text -> Text
futureStubPath area endpoint =
  "/stubs/" <> area <> "/" <> endpoint

futureStubId :: Text -> Text -> Text
futureStubId area endpoint =
  area <> "." <> T.replace "/" "." endpoint

futureStubMethod :: Text
futureStubMethod = "GET"

futureStubRequiredRole :: Text
futureStubRequiredRole = roleToText Admin

futureStubRequiredModule :: Text
futureStubRequiredModule = moduleName ModuleAdmin

validateFutureAdminConsoleCard :: AdminConsoleCard -> Either ServerError AdminConsoleCard
validateFutureAdminConsoleCard card
  | not (validFutureStubSlug (cardId card)) = invalidFutureAdminConsoleMetadata
  | cardId card `notElem` allowedFutureAdminConsoleCardIds =
      invalidFutureAdminConsoleMetadata
  | implemented card = invalidFutureAdminConsoleMetadata
  | invalidCardText 120 (title card) = invalidFutureAdminConsoleMetadata
  | null (body card) || length (body card) > 8 = invalidFutureAdminConsoleMetadata
  | any (invalidCardText 240) (body card) = invalidFutureAdminConsoleMetadata
  | hasDuplicateFutureAdminConsoleBodyLines card = invalidFutureAdminConsoleMetadata
  | title card /= expectedFutureAdminConsoleTitle (cardId card) =
      invalidFutureAdminConsoleMetadata
  | body card /= expectedFutureAdminConsoleBody (cardId card) =
      invalidFutureAdminConsoleMetadata
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
  | viewArea view /= "admin" = invalidFutureAdminConsoleMetadata
  | viewEndpoint view /= "console" = invalidFutureAdminConsoleMetadata
  | viewMethod view /= futureStubMethod = invalidFutureAdminConsoleMetadata
  | viewStatus view /= "preview" = invalidFutureAdminConsoleMetadata
  | viewRequiredRole view /= futureStubRequiredRole = invalidFutureAdminConsoleMetadata
  | viewRequiredModule view /= futureStubRequiredModule = invalidFutureAdminConsoleMetadata
  | viewImplemented view = invalidFutureAdminConsoleMetadata
  | otherwise = do
      viewIdVal <- validateFutureAdminConsolePublishedId (viewId view)
      path <- validateFutureAdminConsolePublishedPath (viewPath view)
      validatedCards <- traverse validateFutureAdminConsoleCard (cards view)
      if map cardId validatedCards /= allowedFutureAdminConsoleCardIds
           || hasDuplicateFutureAdminConsoleTitles validatedCards
           || hasDuplicateFutureAdminConsoleBodyLinesAcrossCards validatedCards
        then invalidFutureAdminConsoleMetadata
        else Right view
          { viewArea = "admin"
          , viewEndpoint = "console"
          , viewId = viewIdVal
          , viewPath = path
          , viewMethod = futureStubMethod
          , viewStatus = "preview"
          , viewRequiredRole = futureStubRequiredRole
          , viewRequiredModule = futureStubRequiredModule
          , viewImplemented = False
          , cards = validatedCards
          }

validateFutureAdminConsoleViewWithCatalog
  :: [(Text, Text)]
  -> AdminConsoleView
  -> Either ServerError AdminConsoleView
validateFutureAdminConsoleViewWithCatalog catalog view = do
  _ <- validateFutureStubCatalog catalog
  validateFutureAdminConsoleView view

hasDuplicateFutureAdminConsoleTitles :: [AdminConsoleCard] -> Bool
hasDuplicateFutureAdminConsoleTitles cardsValue =
  let normalizedTitles = map (T.toCaseFold . title) cardsValue
  in length normalizedTitles /= length (nub normalizedTitles)

hasDuplicateFutureAdminConsoleBodyLines :: AdminConsoleCard -> Bool
hasDuplicateFutureAdminConsoleBodyLines card =
  let normalizedBodyLines = map T.toCaseFold (body card)
  in length normalizedBodyLines /= length (nub normalizedBodyLines)

hasDuplicateFutureAdminConsoleBodyLinesAcrossCards :: [AdminConsoleCard] -> Bool
hasDuplicateFutureAdminConsoleBodyLinesAcrossCards cardsValue =
  let normalizedBodyLines = concatMap (map T.toCaseFold . body) cardsValue
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
        || generalCategory ch
             `elem` [ EnclosingMark
                    , Format
                    , LineSeparator
                    , NonSpacingMark
                    , ParagraphSeparator
                    , SpacingCombiningMark
                    ]
        || (generalCategory ch == Space && ch /= ' ')

accessLoginOptionsStub, accessModuleBehaviourStub, accessSessionPolicyStub
  :: (Text, Text)
accessLoginOptionsStub = ("access", "login-options")
accessModuleBehaviourStub = ("access", "module-behaviour")
accessSessionPolicyStub = ("access", "session-policy")

crmPartiesListColumnsStub, crmPartiesFiltersStub, crmPartiesDetailTabsStub
  :: (Text, Text)
crmPartiesListColumnsStub = ("crm", "parties/list-columns")
crmPartiesFiltersStub = ("crm", "parties/filters")
crmPartiesDetailTabsStub = ("crm", "parties/detail-tabs")

schedulingBookingsViewsStub, schedulingSessionsCreationStub, schedulingRoomsFeaturesStub
  :: (Text, Text)
schedulingBookingsViewsStub = ("scheduling", "bookings/views")
schedulingSessionsCreationStub = ("scheduling", "sessions/creation")
schedulingRoomsFeaturesStub = ("scheduling", "rooms/features")

packagesCatalogStub, packagesPurchaseFlowStub :: (Text, Text)
packagesCatalogStub = ("packages", "catalog")
packagesPurchaseFlowStub = ("packages", "purchase-flow")

invoicingComposerStub, invoicingStatusFlowStub :: (Text, Text)
invoicingComposerStub = ("invoicing", "composer")
invoicingStatusFlowStub = ("invoicing", "status-flow")

inventoryAssetsMetadataStub, inventoryAssetsWorkflowStub, inventoryStockStub
  :: (Text, Text)
inventoryAssetsMetadataStub = ("inventory", "assets/metadata")
inventoryAssetsWorkflowStub = ("inventory", "assets/workflow")
inventoryStockStub = ("inventory", "stock")

adminSeedPolicyStub :: (Text, Text)
adminSeedPolicyStub = ("admin", "seed-policy")

experienceNavigationStub, experienceFeedbackStub, experienceOfflineStub
  :: (Text, Text)
experienceNavigationStub = ("experience", "navigation")
experienceFeedbackStub = ("experience", "feedback")
experienceOfflineStub = ("experience", "offline")

experienceDesignStub, experienceAuditingStub :: (Text, Text)
experienceDesignStub = ("experience", "design")
experienceAuditingStub = ("experience", "auditing")

allowedFutureStubMetadata :: [(Text, Text)]
allowedFutureStubMetadata =
  [ accessLoginOptionsStub
  , accessModuleBehaviourStub
  , accessSessionPolicyStub
  , crmPartiesListColumnsStub
  , crmPartiesFiltersStub
  , crmPartiesDetailTabsStub
  , schedulingBookingsViewsStub
  , schedulingSessionsCreationStub
  , schedulingRoomsFeaturesStub
  , packagesCatalogStub
  , packagesPurchaseFlowStub
  , invoicingComposerStub
  , invoicingStatusFlowStub
  , inventoryAssetsMetadataStub
  , inventoryAssetsWorkflowStub
  , inventoryStockStub
  , adminSeedPolicyStub
  , experienceNavigationStub
  , experienceFeedbackStub
  , experienceOfflineStub
  , experienceDesignStub
  , experienceAuditingStub
  ]

mountedFutureStubAreas :: [Text]
mountedFutureStubAreas =
  [ "access"
  , "crm"
  , "scheduling"
  , "packages"
  , "invoicing"
  , "inventory"
  , "admin"
  , "experience"
  ]

allowedFutureStubAreas :: [Text]
allowedFutureStubAreas = deriveFutureStubAreas allowedFutureStubMetadata

deriveFutureStubAreas :: [(Text, Text)] -> [Text]
deriveFutureStubAreas =
  foldr collectAreaRuns []
  where
    collectAreaRuns (area, _) [] = [area]
    collectAreaRuns (area, _) areas@(nextArea : _)
      | area == nextArea = areas
      | otherwise = area : areas

validateFutureStubArea :: Text -> Either ServerError Text
validateFutureStubArea rawArea
  | rawArea /= area = invalidFutureStubMetadata
  | area `notElem` mountedFutureStubAreas = invalidFutureStubMetadata
  | area `notElem` allowedFutureStubAreas = invalidFutureStubMetadata
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
    && length segments <= maxFutureStubEndpointSegments
    && all validFutureStubSlug segments
  where
    segments = T.splitOn "/" endpoint

maxFutureStubEndpointSegments :: Int
maxFutureStubEndpointSegments = 2

validFutureStubSlug :: Text -> Bool
validFutureStubSlug slug =
  not (T.null slug)
    && T.length slug <= 64
    && validFutureStubSlugStart (T.head slug)
    && T.last slug /= '-'
    && not ("--" `T.isInfixOf` slug)
    && T.all validFutureStubSlugChar slug

validFutureStubSlugStart :: Char -> Bool
validFutureStubSlugStart ch =
  ch >= 'a' && ch <= 'z'

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
