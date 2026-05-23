{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerFuture where

import           Control.Monad.Except (MonadError)
import qualified Data.ByteString.Lazy as BL
import           Data.Char
  ( GeneralCategory
      ( CurrencySymbol
      , EnclosingMark
      , Format
      , LineSeparator
      , MathSymbol
      , ModifierSymbol
      , NonSpacingMark
      , NotAssigned
      , OtherSymbol
      , ParagraphSeparator
      , PrivateUse
      , Space
      , SpacingCombiningMark
      , Surrogate
      )
  , generalCategory
  , isControl
  , ord
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
    , viewStatus = futureAdminConsoleStatus
    , viewRequiredRole = futureStubRequiredRole
    , viewRequiredRoles = futureStubRequiredRoles
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
validateFutureAdminAccess =
  validateFutureAdminAccessWithBaselineRoles requiredFutureAdminBaselineRoles

validateFutureAdminAccessWithBaselineRoles
  :: [RoleEnum]
  -> AuthedUser
  -> Either ServerError ()
validateFutureAdminAccessWithBaselineRoles baselineRoles user
  | Admin `notElem` auRoles user = Left err403 { errBody = "Admin role required" }
  | otherwise =
      case validateFutureAdminBaselineRoles baselineRoles of
        Left serverErr -> Left serverErr
        Right requiredBaselineRoles
          | fromSqlKey (auPartyId user) <= 0 ->
              Left err403 { errBody = "Valid admin party required" }
          | length (auRoles user) /= length (nub (auRoles user)) ->
              Left err403 { errBody = "Admin role grants must be unique" }
          | any (not . isFutureAdminRoleScope) (auRoles user) ->
              Left err403
                { errBody = "Admin fallback discovery cannot be combined with non-baseline roles" }
          | not (null (missingBaselineRoles requiredBaselineRoles)) ->
              Left err403
                { errBody =
                    textBody $
                      "Admin fallback discovery requires baseline roles; missing: "
                        <> T.intercalate ", " (map roleToText (missingBaselineRoles requiredBaselineRoles))
                }
          | not (ModuleAdmin `Set.member` auModules user) ->
              Left err403 { errBody = "Admin module access required" }
          | auModules user /= modulesForRoles (auRoles user) ->
              Left err403 { errBody = "Admin module grants must match roles" }
          | otherwise -> Right ()
  where
    missingBaselineRoles requiredBaselineRoles =
      filter (`notElem` auRoles user) requiredBaselineRoles

validateFutureAdminBaselineRoles :: [RoleEnum] -> Either ServerError [RoleEnum]
validateFutureAdminBaselineRoles baselineRoles
  | baselineRoles == canonicalFutureAdminBaselineRoles = Right baselineRoles
  | otherwise = invalidFutureAdminAccessPolicy

textBody :: Text -> BL.ByteString
textBody =
  BL.fromStrict . TE.encodeUtf8

isFutureAdminRoleScope :: RoleEnum -> Bool
isFutureAdminRoleScope role =
  role `elem` [Admin, Fan, Customer]

requiredFutureAdminBaselineRoles :: [RoleEnum]
requiredFutureAdminBaselineRoles =
  canonicalFutureAdminBaselineRoles

canonicalFutureAdminBaselineRoles :: [RoleEnum]
canonicalFutureAdminBaselineRoles =
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
  canonicalCatalog <- validateAllowedFutureStubMetadata allowedFutureStubMetadata
  reservedRoutes <- validateReservedFutureStubRoutes reservedFutureStubRoutes
  normalized <-
    either (const invalidFutureStubCatalog) Right $
      traverse validateFutureStubCatalogEntry catalog
  _ <- validateFutureStubCatalogRouteBoundaries reservedRoutes normalized
  _ <- validateFutureStubCatalogTopLevelBoundaries normalized
  _ <- validateFutureStubCatalogAreaOrder normalized
  _ <- validateFutureStubCatalogEndpointLeaves normalized
  if normalized /= canonicalCatalog || length normalized /= length (nub normalized)
    then invalidFutureStubCatalog
    else Right normalized

validateAllowedFutureStubMetadata
  :: [(Text, Text)]
  -> Either ServerError [(Text, Text)]
validateAllowedFutureStubMetadata metadata
  | metadata == canonicalFutureStubMetadata = Right metadata
  | otherwise = invalidFutureStubCatalog

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
       || any reservedSiblingSegmentCollision
            [ (reservedRoute, catalogRoute)
            | reservedRoute <- validatedReservedRoutes
            , catalogRoute <- validatedCatalog
            ]
       || any routesOverlap (routePairs validatedCatalog)
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

    reservedSiblingSegmentCollision
      ((reservedArea, reservedEndpoint), catalogRoute@(area, endpoint)) =
      area == reservedArea
        && catalogRoute `notElem` allowedFutureStubReservedSiblingRoutes
        && any segmentsOverlap
             [ (reservedSegment, endpointSegment)
             | reservedSegment <- T.splitOn "/" reservedEndpoint
             , endpointSegment <- T.splitOn "/" endpoint
             ]

    segmentsOverlap (reservedSegment, endpointSegment) =
      reservedSegment /= endpointSegment
        && ( reservedSegment `T.isPrefixOf` endpointSegment
             || endpointSegment `T.isPrefixOf` reservedSegment
           )

    routePairs [] = []
    routePairs (catalogRoute:remaining) =
      map ((,) catalogRoute) remaining <> routePairs remaining

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
  mountedAreas <- validateFutureStubAreaRegistry mountedFutureStubAreas
  validatedCatalog <-
    either (const invalidFutureStubCatalog) Right $
      traverse validateFutureStubCatalogEntry catalog
  _ <- validateFutureStubCatalogTopLevelBoundaries validatedCatalog
  let areaGroups = group (map fst validatedCatalog)
      expectedAreaGroups = group (map fst allowedFutureStubMetadata)
      areaRuns = map head areaGroups
      entriesFor area = filter ((== area) . fst) validatedCatalog
      expectedEntriesFor area = filter ((== area) . fst) allowedFutureStubMetadata
      areaEntriesMatch =
        all
          (\area -> entriesFor area == expectedEntriesFor area)
          mountedAreas
  if length validatedCatalog /= length (nub validatedCatalog)
       || areaRuns /= mountedAreas
       || map length areaGroups /= map length expectedAreaGroups
       || length areaRuns /= length (nub areaRuns)
       || not areaEntriesMatch
    then invalidFutureStubCatalog
    else Right areaRuns

validateFutureStubCatalogEndpointLeaves
  :: [(Text, Text)]
  -> Either ServerError [(Text, Text)]
validateFutureStubCatalogEndpointLeaves =
  validateFutureStubCatalogEndpointLeavesWithCardIds allowedFutureAdminConsoleCardIds

validateFutureStubCatalogEndpointLeavesWithCardIds
  :: [Text]
  -> [(Text, Text)]
  -> Either ServerError [(Text, Text)]
validateFutureStubCatalogEndpointLeavesWithCardIds rawCardIds catalog = do
  mountedAreas <- validateFutureStubAreaRegistry mountedFutureStubAreas
  reservedTopLevelAreas <-
    validateReservedFutureStubTopLevelAreas reservedFutureStubTopLevelAreas
  reservedRoutes <- validateReservedFutureStubRoutes reservedFutureStubRoutes
  cardIds <- validateFutureAdminConsoleCardIds rawCardIds
  validatedCatalog <-
    either (const invalidFutureStubCatalog) Right $
      traverse validateFutureStubLeafRoute catalog
  if any (hasDuplicateLeaves validatedCatalog) mountedAreas
       || any (hasLeafBranchCollision validatedCatalog) mountedAreas
       || any (hasLeafPrefixCollision validatedCatalog) mountedAreas
       || any (hasSegmentPrefixCollision validatedCatalog) mountedAreas
       || hasMountedAreaSegmentCollision mountedAreas validatedCatalog
       || hasReservedTopLevelAreaSegmentCollision reservedTopLevelAreas validatedCatalog
       || any (hasReservedLeafCollision reservedRoutes validatedCatalog) mountedAreas
       || hasReservedLeafLabelCollision reservedRoutes validatedCatalog
       || hasReservedSegmentLabelCollision reservedRoutes validatedCatalog
       || hasAdminConsoleCardSegmentCollision cardIds validatedCatalog
    then invalidFutureStubCatalog
    else Right validatedCatalog
  where
    validateFutureStubLeafRoute (area, endpoint) = do
      areaClean <- validateFutureStubArea area
      endpointClean <- validateFutureStubEndpoint endpoint
      pure (areaClean, endpointClean)

    hasDuplicateLeaves catalogForAreas area =
      let leaves =
            [ endpointLeaf endpoint
            | (entryArea, endpoint) <- catalogForAreas
            , entryArea == area
            ]
      in length leaves /= length (nub leaves)

    hasLeafBranchCollision catalogForAreas area =
      let areaEndpoints =
            [ endpoint
            | (entryArea, endpoint) <- catalogForAreas
            , entryArea == area
            ]
          leaves = Set.fromList (map endpointLeaf areaEndpoints)
          branchSegments = concatMap endpointBranchSegments areaEndpoints
      in any (`Set.member` leaves) branchSegments

    hasLeafPrefixCollision catalogForAreas area =
      any isAmbiguousLabelPair (labelPairs leaves)
      where
        leaves =
          [ endpointLeaf endpoint
          | (entryArea, endpoint) <- catalogForAreas
          , entryArea == area
          ]

    hasSegmentPrefixCollision catalogForAreas area =
      any isAmbiguousLabelPair (labelPairs segments)
      where
        segments =
          nub
            [ segment
            | (entryArea, endpoint) <- catalogForAreas
            , entryArea == area
            , segment <- T.splitOn "/" endpoint
            ]

    isAmbiguousLabelPair (leftLabel, rightLabel) =
      leftLabel /= rightLabel
        && (leftLabel `T.isPrefixOf` rightLabel || rightLabel `T.isPrefixOf` leftLabel)

    hasMountedAreaSegmentCollision mountedAreasValue catalogForAreas =
      any
        (\(_area, endpoint) ->
           any
             (\segment -> any (segmentsOverlap segment) mountedAreasValue)
             (T.splitOn "/" endpoint)
        )
        catalogForAreas

    hasReservedTopLevelAreaSegmentCollision reservedTopLevelAreasValue catalogForAreas =
      any
        (\route@(_area, endpoint) ->
           route `notElem` allowedFutureStubReservedTopLevelEndpointRoutes
             && any
                  (\segment ->
                     any (segmentsOverlap segment) reservedTopLevelAreasValue
                  )
                  (T.splitOn "/" endpoint)
        )
        catalogForAreas

    hasReservedLeafCollision reservedRoutes catalogForAreas area =
      let reservedLeaves =
            Set.fromList
              [ endpointLeaf endpoint
              | (reservedArea, endpoint) <- reservedRoutes
              , reservedArea == area
              ]
          catalogLeaves =
            [ endpointLeaf endpoint
            | (entryArea, endpoint) <- catalogForAreas
            , entryArea == area
            ]
      in any (`Set.member` reservedLeaves) catalogLeaves

    hasReservedLeafLabelCollision reservedRoutesForAreas catalogForAreas =
      let reservedLeaves = Set.fromList (map (endpointLeaf . snd) reservedRoutesForAreas)
      in any
          ((`Set.member` reservedLeaves) . endpointLeaf . snd)
          catalogForAreas

    hasReservedSegmentLabelCollision reservedRoutesForAreas catalogForAreas =
      let reservedSegments =
            Set.toList $
              Set.fromList (concatMap (T.splitOn "/" . snd) reservedRoutesForAreas)
      in any (routeHasReservedSegmentLabel reservedSegments) catalogForAreas

    hasAdminConsoleCardSegmentCollision cardIds catalogForAreas =
      any
          (any (hasAdminConsoleCardLabelOverlap cardIds) . T.splitOn "/" . snd)
          catalogForAreas

    hasAdminConsoleCardLabelOverlap cardIds segment =
      any (segmentsOverlap segment) cardIds

    routeHasReservedSegmentLabel reservedSegments route@(_area, endpoint)
      | route `elem` allowedFutureStubReservedSiblingRoutes = False
      | otherwise =
          any
            (\segment -> any (segmentsOverlap segment) reservedSegments)
            (T.splitOn "/" endpoint)

    segmentsOverlap segment reservedSegment =
      segment == reservedSegment
        || reservedSegment `T.isPrefixOf` segment
        || segment `T.isPrefixOf` reservedSegment

    labelPairs [] = []
    labelPairs (label:remaining) =
      map ((,) label) remaining <> labelPairs remaining

    endpointLeaf endpoint =
      case reverse (T.splitOn "/" endpoint) of
        leaf:_ -> leaf
        [] -> ""

    endpointBranchSegments endpoint =
      case T.splitOn "/" endpoint of
        [] -> []
        [_] -> []
        segments -> init segments

validateFutureStubCatalogTopLevelBoundaries
  :: [(Text, Text)]
  -> Either ServerError [(Text, Text)]
validateFutureStubCatalogTopLevelBoundaries catalog = do
  reservedTopLevelAreas <-
    validateReservedFutureStubTopLevelAreas reservedFutureStubTopLevelAreas
  validatedCatalog <-
    either (const invalidFutureStubCatalog) Right $
      traverse validateFutureStubTopLevelBoundaryRoute catalog
  if any ((`elem` reservedTopLevelAreas) . fst) validatedCatalog
    then invalidFutureStubCatalog
    else Right validatedCatalog
  where
    validateFutureStubTopLevelBoundaryRoute (area, endpoint) = do
      areaClean <- validateFutureStubArea area
      endpointClean <- validateFutureStubEndpoint endpoint
      pure (areaClean, endpointClean)

reservedFutureStubRoutes :: [(Text, Text)]
reservedFutureStubRoutes =
  requiredReservedFutureStubRoutes

requiredReservedFutureStubRoutes :: [(Text, Text)]
requiredReservedFutureStubRoutes =
  [ ("admin", "console")
  , ("admin", "seed")
  ]

reservedFutureStubTopLevelAreas :: [Text]
reservedFutureStubTopLevelAreas =
  requiredReservedFutureStubTopLevelAreas

requiredReservedFutureStubTopLevelAreas :: [Text]
requiredReservedFutureStubTopLevelAreas =
  -- /stubs/catalog is the discovery index, not a generic stub area.
  [ "catalog"
  ]

allowedFutureStubReservedTopLevelEndpointRoutes :: [(Text, Text)]
allowedFutureStubReservedTopLevelEndpointRoutes =
  -- /stubs/packages/catalog is a real package-catalog placeholder; only the
  -- top-level /stubs/catalog route is reserved for discovery.
  [ packagesCatalogStub
  ]

allowedFutureStubReservedSiblingRoutes :: [(Text, Text)]
allowedFutureStubReservedSiblingRoutes =
  -- /stubs/admin/seed is a reserved mutating route. The read-only policy
  -- placeholder intentionally documents that surface without mounting it.
  [ adminSeedPolicyStub
  ]

validateReservedFutureStubTopLevelAreas :: [Text] -> Either ServerError [Text]
validateReservedFutureStubTopLevelAreas areas
  | null areas = invalidFutureStubCatalog
  | length areas /= length (nub areas) = invalidFutureStubCatalog
  | not (all validFutureStubSlug areas) = invalidFutureStubCatalog
  | any (`elem` mountedFutureStubAreas) areas = invalidFutureStubCatalog
  | areas /= requiredReservedFutureStubTopLevelAreas = invalidFutureStubCatalog
  | otherwise = Right areas

validateFutureStubResponse :: StubResponse -> Either ServerError StubResponse
validateFutureStubResponse response =
  case validateFutureStubMetadata (stubArea response) (stubEndpoint response) of
    Left _ -> invalidFutureStubResponse
    Right (area, endpoint)
      | stubImplemented response -> invalidFutureStubResponse
      | otherwise -> do
          status <- validateFutureStubStatus (stubStatus response)
          method <-
            validateFutureStubMethod (stubMethod response)
          requiredModule <-
            validateFutureStubRequiredModule (stubRequiredModule response)
          (requiredRole, requiredRoles) <-
            validateFutureStubAuthMetadata
              (stubRequiredRole response)
              (stubRequiredRoles response)
          responseId <- validateFutureStubPublishedId area endpoint (stubId response)
          path <- validateFutureStubPublishedPath area endpoint (stubPath response)
          Right response
            { stubArea = area
            , stubEndpoint = endpoint
            , stubId = responseId
            , stubPath = path
            , stubMethod = method
            , stubStatus = status
            , stubRequiredRole = requiredRole
            , stubRequiredRoles = requiredRoles
            , stubRequiredModule = requiredModule
            , stubImplemented = False
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
      if adminConsoleRoute `elem` reservedRoutes
        then Right adminConsoleRoute
        else invalidFutureAdminConsoleMetadata
  where
    adminConsoleRoute = ("admin", "console")

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
validateFutureStubCatalogResponses responses
  | length responses /= length allowedFutureStubMetadata = invalidFutureStubCatalog
  | any isReservedFutureStubCatalogResponseRoute responses = invalidFutureStubCatalog
  | otherwise = do
      validatedResponses <- traverse validateFutureStubResponse responses
      let metadata =
            map
              (\response -> (stubArea response, stubEndpoint response))
              validatedResponses
          ids = map stubId validatedResponses
          paths = map stubPath validatedResponses
      if metadata /= allowedFutureStubMetadata
           || length metadata /= length (nub metadata)
           || length ids /= length (nub ids)
           || length paths /= length (nub paths)
        then invalidFutureStubCatalog
        else Right validatedResponses

isReservedFutureStubCatalogResponseRoute :: StubResponse -> Bool
isReservedFutureStubCatalogResponseRoute response =
  (stubArea response, stubEndpoint response) `elem` reservedFutureStubRoutes
    || stubArea response `elem` reservedFutureStubTopLevelAreas
    || stubId response `elem` reservedFutureStubCatalogResponseIds
    || stubPath response `elem` reservedFutureStubCatalogResponsePaths

reservedFutureStubCatalogResponseIds :: [Text]
reservedFutureStubCatalogResponseIds =
  map (uncurry futureStubId) reservedFutureStubRoutes
    <> reservedFutureStubTopLevelAreas

reservedFutureStubCatalogResponsePaths :: [Text]
reservedFutureStubCatalogResponsePaths =
  map (uncurry futureStubPath) reservedFutureStubRoutes
    <> map ("/stubs/" <>) reservedFutureStubTopLevelAreas

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
      , stubStatus = futureStubStatus
      , stubRequiredRole = futureStubRequiredRole
      , stubRequiredRoles = futureStubRequiredRoles
      , stubRequiredModule = futureStubRequiredModule
      , stubImplemented = False
      }

futureStubResponseForWithConsole
  :: AdminConsoleView
  -> Text
  -> Text
  -> Either ServerError StubResponse
futureStubResponseForWithConsole consoleView rawArea rawEndpoint = do
  _ <- validateFutureAdminConsoleViewWithCatalog allowedFutureStubMetadata consoleView
  futureStubResponseFor rawArea rawEndpoint

futureStubPath :: Text -> Text -> Text
futureStubPath area endpoint =
  "/stubs/" <> area <> "/" <> endpoint

futureStubId :: Text -> Text -> Text
futureStubId area endpoint =
  area <> "." <> T.replace "/" "." endpoint

futureStubMethod :: Text
futureStubMethod = "GET"

canonicalFutureStubMethod :: Text
canonicalFutureStubMethod = "GET"

futureStubStatus :: Text
futureStubStatus = "planned"

canonicalFutureStubStatus :: Text
canonicalFutureStubStatus = "planned"

futureAdminConsoleStatus :: Text
futureAdminConsoleStatus = "preview"

canonicalFutureAdminConsoleStatus :: Text
canonicalFutureAdminConsoleStatus = "preview"

futureStubRequiredRole :: Text
futureStubRequiredRole = roleToText Admin

futureStubRequiredRoles :: [Text]
futureStubRequiredRoles =
  map roleToText (Admin : requiredFutureAdminBaselineRoles)

futureStubRequiredModule :: Text
futureStubRequiredModule = moduleName ModuleAdmin

canonicalFutureStubRequiredRole :: Text
canonicalFutureStubRequiredRole = "Admin"

canonicalFutureStubRequiredModule :: Text
canonicalFutureStubRequiredModule = "Admin"

canonicalFutureStubRequiredRoles :: [Text]
canonicalFutureStubRequiredRoles =
  [ "Admin"
  , "Fan"
  , "Customer"
  ]

validateFutureStubAuthMetadata
  :: Text
  -> [Text]
  -> Either ServerError (Text, [Text])
validateFutureStubAuthMetadata =
  validateFutureAuthMetadataWith invalidFutureStubResponse

validateFutureAdminConsoleAuthMetadata
  :: Text
  -> [Text]
  -> Either ServerError (Text, [Text])
validateFutureAdminConsoleAuthMetadata =
  validateFutureAuthMetadataWith invalidFutureAdminConsoleMetadata

validateFutureStubStatus :: Text -> Either ServerError Text
validateFutureStubStatus =
  validateFutureStatusMetadataWith
    invalidFutureStubResponse
    futureStubStatus
    canonicalFutureStubStatus

validateFutureAdminConsoleStatus :: Text -> Either ServerError Text
validateFutureAdminConsoleStatus =
  validateFutureStatusMetadataWith
    invalidFutureAdminConsoleMetadata
    futureAdminConsoleStatus
    canonicalFutureAdminConsoleStatus

validateFutureStubMethod :: Text -> Either ServerError Text
validateFutureStubMethod =
  validateFutureMethodMetadataWith invalidFutureStubResponse futureStubMethod

validateFutureAdminConsoleMethod :: Text -> Either ServerError Text
validateFutureAdminConsoleMethod =
  validateFutureMethodMetadataWith invalidFutureAdminConsoleMetadata futureStubMethod

validateFutureMethodMetadataWith
  :: Either ServerError Text
  -> Text
  -> Text
  -> Either ServerError Text
validateFutureMethodMetadataWith invalid configuredMethod publishedMethod
  | configuredMethod /= canonicalFutureStubMethod = invalid
  | publishedMethod /= canonicalFutureStubMethod = invalid
  | otherwise = Right publishedMethod

validateFutureStatusMetadataWith
  :: Either ServerError Text
  -> Text
  -> Text
  -> Text
  -> Either ServerError Text
validateFutureStatusMetadataWith invalid configuredStatus canonicalStatus publishedStatus
  | configuredStatus /= canonicalStatus = invalid
  | publishedStatus /= canonicalStatus = invalid
  | otherwise = Right publishedStatus

validateFutureStubRequiredModule :: Text -> Either ServerError Text
validateFutureStubRequiredModule =
  validateFutureRequiredModuleWith invalidFutureStubResponse

validateFutureAdminConsoleRequiredModule :: Text -> Either ServerError Text
validateFutureAdminConsoleRequiredModule =
  validateFutureRequiredModuleWith invalidFutureAdminConsoleMetadata

validateFutureRequiredModuleWith
  :: Either ServerError Text
  -> Text
  -> Either ServerError Text
validateFutureRequiredModuleWith invalid requiredModule
  | futureStubRequiredModule /= canonicalFutureStubRequiredModule = invalid
  | requiredModule /= canonicalFutureStubRequiredModule = invalid
  | otherwise = Right requiredModule

validateFutureAuthMetadataWith
  :: Either ServerError (Text, [Text])
  -> Text
  -> [Text]
  -> Either ServerError (Text, [Text])
validateFutureAuthMetadataWith invalid requiredRole requiredRoles
  | futureStubRequiredRole /= canonicalFutureStubRequiredRole = invalid
  | futureStubRequiredRoles /= canonicalFutureStubRequiredRoles = invalid
  | requiredRole /= canonicalFutureStubRequiredRole = invalid
  | requiredRoles /= canonicalFutureStubRequiredRoles = invalid
  | requiredRole `notElem` requiredRoles = invalid
  | length requiredRoles /= length (nub requiredRoles) = invalid
  | otherwise = Right (requiredRole, requiredRoles)

validateFutureAdminConsoleCard :: AdminConsoleCard -> Either ServerError AdminConsoleCard
validateFutureAdminConsoleCard =
  validateFutureAdminConsoleCardWithIds allowedFutureAdminConsoleCardIds

validateFutureAdminConsoleCardWithIds
  :: [Text]
  -> AdminConsoleCard
  -> Either ServerError AdminConsoleCard
validateFutureAdminConsoleCardWithIds rawAllowedCardIds card = do
  allowedCardIds <- validateFutureAdminConsoleCardIds rawAllowedCardIds
  validateCard allowedCardIds
  where
    validateCard allowedCardIds
      | not (validFutureStubSlug (cardId card)) = invalidFutureAdminConsoleMetadata
      | cardId card `notElem` allowedCardIds =
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
  canonicalFutureAdminConsoleCardIds

canonicalFutureAdminConsoleCardIds :: [Text]
canonicalFutureAdminConsoleCardIds =
  [ "user-management"
  , "api-tokens"
  ]

validateFutureAdminConsoleView :: AdminConsoleView -> Either ServerError AdminConsoleView
validateFutureAdminConsoleView view
  | viewArea view /= "admin" = invalidFutureAdminConsoleMetadata
  | viewEndpoint view /= "console" = invalidFutureAdminConsoleMetadata
  | viewImplemented view = invalidFutureAdminConsoleMetadata
  | otherwise = do
      status <- validateFutureAdminConsoleStatus (viewStatus view)
      method <-
        validateFutureAdminConsoleMethod (viewMethod view)
      requiredModule <-
        validateFutureAdminConsoleRequiredModule (viewRequiredModule view)
      (requiredRole, requiredRoles) <-
        validateFutureAdminConsoleAuthMetadata
          (viewRequiredRole view)
          (viewRequiredRoles view)
      allowedCardIds <- validateFutureAdminConsoleCardIds allowedFutureAdminConsoleCardIds
      viewIdVal <- validateFutureAdminConsolePublishedId (viewId view)
      path <- validateFutureAdminConsolePublishedPath (viewPath view)
      validatedCards <- traverse validateFutureAdminConsoleCard (cards view)
      if map cardId validatedCards /= allowedCardIds
           || hasDuplicateFutureAdminConsoleTitles validatedCards
           || hasDuplicateFutureAdminConsoleBodyLinesAcrossCards validatedCards
        then invalidFutureAdminConsoleMetadata
        else Right view
          { viewArea = "admin"
          , viewEndpoint = "console"
          , viewId = viewIdVal
          , viewPath = path
          , viewMethod = method
          , viewStatus = status
          , viewRequiredRole = requiredRole
          , viewRequiredRoles = requiredRoles
          , viewRequiredModule = requiredModule
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

validateFutureAdminConsoleCardIds :: [Text] -> Either ServerError [Text]
validateFutureAdminConsoleCardIds cardIds
  | null cardIds = invalidFutureAdminConsoleMetadata
  | length cardIds /= length (nub cardIds) = invalidFutureAdminConsoleMetadata
  | not (all validFutureStubSlug cardIds) = invalidFutureAdminConsoleMetadata
  | any (invalidCardText 120 . expectedFutureAdminConsoleTitle) cardIds =
      invalidFutureAdminConsoleMetadata
  | any (any (invalidCardText 240) . expectedFutureAdminConsoleBody) cardIds =
      invalidFutureAdminConsoleMetadata
  | cardIds /= canonicalFutureAdminConsoleCardIds = invalidFutureAdminConsoleMetadata
  | otherwise = Right cardIds

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
        || ord ch > 0xFF
        || generalCategory ch
             `elem` [ CurrencySymbol
                    , EnclosingMark
                    , Format
                    , LineSeparator
                    , MathSymbol
                    , ModifierSymbol
                    , NonSpacingMark
                    , NotAssigned
                    , OtherSymbol
                    , ParagraphSeparator
                    , PrivateUse
                    , SpacingCombiningMark
                    , Surrogate
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
  canonicalFutureStubMetadata

canonicalFutureStubMetadata :: [(Text, Text)]
canonicalFutureStubMetadata =
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

validateFutureStubAreaRegistry :: [Text] -> Either ServerError [Text]
validateFutureStubAreaRegistry areas = do
  reservedTopLevelAreas <-
    validateReservedFutureStubTopLevelAreas reservedFutureStubTopLevelAreas
  validateAreas reservedTopLevelAreas
  where
    validateAreas reservedTopLevelAreas
      | null areas = invalidFutureStubCatalog
      | length areas /= length (nub areas) = invalidFutureStubCatalog
      | any (`elem` reservedTopLevelAreas) areas = invalidFutureStubCatalog
      | not (all validFutureStubSlug areas) = invalidFutureStubCatalog
      | areas /= allowedFutureStubAreas = invalidFutureStubCatalog
      | otherwise = Right areas

validateFutureStubArea :: Text -> Either ServerError Text
validateFutureStubArea rawArea = do
  mountedAreas <- validateFutureStubAreaRegistry mountedFutureStubAreas
  let area = T.strip rawArea
  validateArea mountedAreas area
  where
    validateArea mountedAreas area
      | rawArea /= area = invalidFutureStubMetadata
      | area `elem` reservedFutureStubTopLevelAreas = invalidFutureStubMetadata
      | area `notElem` mountedAreas = invalidFutureStubMetadata
      | area `notElem` allowedFutureStubAreas = invalidFutureStubMetadata
      | validFutureStubSlug area = Right area
      | otherwise = invalidFutureStubMetadata

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
    && not (hasAmbiguousFutureStubPathSegments segments)
  where
    segments = T.splitOn "/" endpoint

maxFutureStubEndpointSegments :: Int
maxFutureStubEndpointSegments = 2

hasAmbiguousFutureStubPathSegments :: [Text] -> Bool
hasAmbiguousFutureStubPathSegments segments =
  any ambiguousSegmentPair (segmentPairs segments)
  where
    ambiguousSegmentPair (leftSegment, rightSegment) =
      leftSegment == rightSegment
        || leftSegment `T.isPrefixOf` rightSegment
        || rightSegment `T.isPrefixOf` leftSegment

    segmentPairs [] = []
    segmentPairs (segment:remaining) =
      map ((,) segment) remaining <> segmentPairs remaining

validFutureStubSlug :: Text -> Bool
validFutureStubSlug slug =
  not (T.null slug)
    && T.length slug <= 64
    && slug `notElem` reservedFutureStubSlugLabels
    && validFutureStubSlugStart (T.head slug)
    && T.last slug /= '-'
    && not ("--" `T.isInfixOf` slug)
    && T.all validFutureStubSlugChar slug

reservedFutureStubSlugLabels :: [Text]
reservedFutureStubSlugLabels =
  [ "constructor"
  , "index"
  , "null"
  , "prototype"
  , "undefined"
  ]

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

invalidFutureAdminAccessPolicy :: Either ServerError a
invalidFutureAdminAccessPolicy =
  Left err500 { errBody = "Invalid future admin access policy" }

invalidFutureStubResponse :: Either ServerError a
invalidFutureStubResponse =
  Left err500 { errBody = "Invalid future stub response" }
