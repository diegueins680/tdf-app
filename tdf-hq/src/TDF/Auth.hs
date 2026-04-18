{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module TDF.Auth
  ( AuthedUser(..)
  , ModuleAccess(..)
  , authContext
  , hasAiToolingAccess
  , hasStrictAdminAccess
  , hasOperationsAccess
  , hasSocialSyncAccess
  , hasSocialInboxAccess
  , hasModuleAccess
  , moduleName
  , modulesForRoles
  , loadAuthedUser
  , lookupUsernameFromToken
  , resolveUsernameFromLabel
  , extractToken
  , extractTokenFromHeaders
  , sessionCookieHeader
  , clearSessionCookieHeader
  ) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (forM_, guard)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy       as BL
import           Data.List                  (foldl')
import           Data.Maybe                 (listToMaybe, maybeToList)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Database.Persist           (Entity(..), getBy, selectList, upsert, (==.), (=.))
import           Database.Persist.Sql       (SqlPersistT, runSqlPool)
import           Network.Wai                (Request, requestHeaders)
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler, AuthServerData)

import           TDF.DB                     (Env(..))
import           TDF.Config                 (AppConfig(..))
import           TDF.Models

-- | Enumeration of major application modules.
data ModuleAccess
  = ModuleCRM
  | ModuleScheduling
  | ModulePackages
  | ModuleInvoicing
  | ModuleAdmin
  | ModuleInternships
  | ModuleOps
  deriving (Eq, Ord, Show, Enum, Bounded)

moduleName :: ModuleAccess -> Text
moduleName ModuleCRM        = "CRM"
moduleName ModuleScheduling = "Scheduling"
moduleName ModulePackages   = "Packages"
moduleName ModuleInvoicing  = "Invoicing"
moduleName ModuleAdmin      = "Admin"
moduleName ModuleInternships = "Internships"
moduleName ModuleOps        = "Ops"

-- | Authenticated user with associated module access.
data AuthedUser = AuthedUser
  { auPartyId :: PartyId
  , auRoles   :: [RoleEnum]
  , auModules :: Set ModuleAccess
  } deriving (Show, Eq)

-- | Create the Servant auth context using the database environment.
authContext :: Env -> Context '[AuthHandler Request AuthedUser]
authContext env = mkAuthHandler (authWithToken env) :. EmptyContext

-- | Check whether the user can access the given module.
hasModuleAccess :: ModuleAccess -> AuthedUser -> Bool
hasModuleAccess moduleTag AuthedUser{..} = moduleTag `Set.member` auModules

hasStrictAdminAccess :: AuthedUser -> Bool
hasStrictAdminAccess AuthedUser{..} = Admin `elem` auRoles

hasOperationsAccess :: AuthedUser -> Bool
hasOperationsAccess user@AuthedUser{..} =
  hasModuleAccess ModuleAdmin user || any (`elem` auRoles) [Manager, Maintenance]

hasAiToolingAccess :: AuthedUser -> Bool
hasAiToolingAccess = hasOperationsAccess

hasSocialSyncAccess :: AuthedUser -> Bool
hasSocialSyncAccess = hasStrictAdminAccess

hasSocialInboxAccess :: AuthedUser -> Bool
hasSocialInboxAccess user@AuthedUser{..} =
  hasModuleAccess ModuleCRM user
    && any (`elem` auRoles)
      [ Admin
      , Manager
      , StudioManager
      , Reception
      , LiveSessionsProducer
      , Producer
      , AandR
      , Webmaster
      ]

-- Internal -----------------------------------------------------------------

authWithToken :: Env -> Request -> Handler AuthedUser
authWithToken env req = do
  let cfg = envConfig env
  token <- either throw401 pure (extractToken cfg req)
  mUser <- liftIO . flip runSqlPool (envPool env) $ loadAuthedUser token
  case mUser of
    Nothing   -> throw401 "Invalid or inactive token"
    Just user -> pure user
  where
    throw401 :: Text -> Handler a
    throw401 msg = throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

loadAuthedUser :: Text -> SqlPersistT IO (Maybe AuthedUser)
loadAuthedUser token = do
  mToken <- getBy (UniqueApiToken token)
  case mToken of
    Nothing -> pure Nothing
    Just (Entity _ tok)
      | not (apiTokenActive tok) -> pure Nothing
      | otherwise -> do
          roles <- selectList [PartyRolePartyId ==. apiTokenPartyId tok, PartyRoleActive ==. True] []
          roleList <- ensureDefaultRoles (apiTokenPartyId tok) (map (partyRoleRole . entityVal) roles)
          let modules  = modulesForRoles roleList
          pure $ Just AuthedUser
            { auPartyId = apiTokenPartyId tok
            , auRoles   = roleList
            , auModules = modules
            }

lookupUsernameFromToken :: Text -> SqlPersistT IO (Maybe Text)
lookupUsernameFromToken token = do
  mToken <- getBy (UniqueApiToken token)
  case mToken of
    Just (Entity _ tok)
      | apiTokenActive tok -> do
          case apiTokenLabel tok >>= resolveUsernameFromLabel of
            Just username -> pure (Just username)
            Nothing -> do
              creds <- selectList [UserCredentialPartyId ==. apiTokenPartyId tok, UserCredentialActive ==. True] []
              pure (userCredentialUsername . entityVal <$> listToMaybe creds)
      | otherwise -> pure Nothing
    Nothing -> pure Nothing

resolveUsernameFromLabel :: Text -> Maybe Text
resolveUsernameFromLabel rawLabel =
  let trimmed = T.strip rawLabel
      attempt prefix = T.strip <$> T.stripPrefix prefix trimmed
      resolved = attempt "password-login:" <|> attempt "password-reset:"
      nonEmpty txt =
        let stripped = T.strip txt
        in if T.null stripped then Nothing else Just stripped
  in resolved >>= nonEmpty

modulesForRoles :: [RoleEnum] -> Set ModuleAccess
modulesForRoles = foldl' (flip (Set.union . modulesForRole)) Set.empty

modulesForRole :: RoleEnum -> Set ModuleAccess
modulesForRole Admin      = Set.fromList [ModuleCRM, ModuleScheduling, ModulePackages, ModuleInvoicing, ModuleAdmin, ModuleInternships, ModuleOps]
modulesForRole Manager    = Set.fromList [ModuleCRM, ModuleScheduling, ModulePackages, ModuleInvoicing, ModuleInternships, ModuleOps]
modulesForRole StudioManager = Set.fromList [ModuleCRM, ModuleScheduling, ModulePackages, ModuleInvoicing, ModuleAdmin, ModuleInternships, ModuleOps]
modulesForRole Reception  = Set.fromList [ModuleCRM, ModuleScheduling]
modulesForRole Accounting = Set.singleton ModuleInvoicing
modulesForRole Engineer   = Set.singleton ModuleScheduling
modulesForRole Teacher    = Set.singleton ModuleScheduling
modulesForRole LiveSessionsProducer = Set.fromList [ModuleCRM, ModuleScheduling]
modulesForRole Intern     = Set.singleton ModuleInternships
modulesForRole Artist     = Set.fromList [ModuleScheduling, ModulePackages]
modulesForRole Artista    = Set.fromList [ModuleScheduling, ModulePackages]
modulesForRole Webmaster  = Set.fromList [ModuleAdmin, ModuleCRM]
modulesForRole Promotor   = Set.empty
modulesForRole Promoter   = Set.empty
modulesForRole Producer   = Set.fromList [ModuleCRM, ModuleScheduling]
modulesForRole Songwriter = Set.empty
modulesForRole DJ         = Set.empty
modulesForRole Publicist  = Set.empty
modulesForRole TourManager = Set.empty
modulesForRole LabelRep    = Set.empty
modulesForRole StageManager = Set.empty
modulesForRole RoadCrew    = Set.empty
modulesForRole Photographer = Set.empty
modulesForRole AandR      = Set.fromList [ModuleCRM, ModuleScheduling]
modulesForRole Student    = Set.singleton ModuleScheduling
modulesForRole Vendor     = Set.singleton ModulePackages
modulesForRole Customer   = Set.singleton ModulePackages
modulesForRole ReadOnly   = Set.singleton ModuleCRM
modulesForRole Fan        = Set.empty
modulesForRole Maintenance = Set.fromList [ModulePackages, ModuleScheduling, ModuleOps]

-- Ensure every authenticated user has baseline Fan and Customer roles active.
defaultRoles :: [RoleEnum]
defaultRoles = [Fan, Customer]

ensureDefaultRoles :: PartyId -> [RoleEnum] -> SqlPersistT IO [RoleEnum]
ensureDefaultRoles pid roles = do
  let existing = Set.fromList roles
      missing  = filter (`Set.notMember` existing) defaultRoles
  forM_ missing $ \r ->
    upsert (PartyRole pid r True) [PartyRoleActive =. True]
  pure (roles ++ missing)

extractToken :: AppConfig -> Request -> Either Text Text
extractToken cfg req =
  case authorizationHeaders of
    [] ->
      extractCookieToken cfg req
    [rawHeader] ->
      case TE.decodeUtf8' rawHeader of
        Left _ -> Left "Invalid Authorization header"
        Right txt -> extractTokenFromHeaders cfg (Just txt) Nothing
    _ ->
      Left "Multiple Authorization headers found"
  where
    authorizationHeaders =
      [ rawHeader
      | (headerName, rawHeader) <- requestHeaders req
      , headerName == "Authorization"
      ]

    extractCookieToken AppConfig{sessionCookieName} request =
      case cookieHeaders request of
        [] -> Left "Missing or invalid auth token"
        [rawHeader] ->
          case TE.decodeUtf8' rawHeader of
            Left _ -> Left "Missing or invalid auth token"
            Right header -> lookupCookie sessionCookieName header
        _ -> Left "Multiple Cookie headers found"

    cookieHeaders request =
      [ rawHeader
      | (headerName, rawHeader) <- requestHeaders request
      , headerName == "Cookie"
      ]

extractTokenFromHeaders :: AppConfig -> Maybe Text -> Maybe Text -> Either Text Text
extractTokenFromHeaders AppConfig{sessionCookieName} mAuthorizationHeader mCookieHeader =
  case mAuthorizationHeader of
    Just rawHeader ->
      case T.words rawHeader of
        [scheme, value]
          | T.toLower scheme == "bearer" -> Right value
        _ -> Left "Invalid Authorization header"
    Nothing ->
      maybe
        (Left "Missing or invalid auth token")
        (lookupCookie sessionCookieName)
        mCookieHeader

lookupCookie :: Text -> Text -> Either Text Text
lookupCookie cookieName rawHeader =
  let pairs = map (breakOnEquals . T.strip) (T.splitOn ";" rawHeader)
      matchingValues = do
        (namePart, valuePart) <- pairs
        let name = T.strip namePart
            value = T.strip valuePart
        guard (name == cookieName)
        pure value
  in case matchingValues of
       [] -> Left "Missing or invalid auth token"
       [value] ->
         maybe
           (Left "Missing or invalid auth token")
           Right
           (nonEmptyText value)
       _ -> Left "Multiple session cookies found"
  where
    breakOnEquals chunk =
      let (name, rest) = T.breakOn "=" chunk
      in (name, T.drop 1 rest)

    nonEmptyText txt =
      let trimmed = T.strip txt
      in if T.null trimmed then Nothing else Just trimmed

sessionCookieHeader :: AppConfig -> Text -> Text
sessionCookieHeader cfg token =
  cookieHeaderWithValue cfg token

clearSessionCookieHeader :: AppConfig -> Text
clearSessionCookieHeader AppConfig{..} =
  let segments =
        [ sessionCookieName <> "="
        , "Path=" <> sessionCookiePath
        , "HttpOnly"
        , "SameSite=" <> sessionCookieSameSite
        , "Max-Age=0"
        , "Expires=Thu, 01 Jan 1970 00:00:00 GMT"
        ]
        <> maybe [] (\domainVal -> ["Domain=" <> domainVal]) sessionCookieDomain
  in T.intercalate "; " segments

cookieHeaderWithValue :: AppConfig -> Text -> Text
cookieHeaderWithValue AppConfig{..} rawValue =
  let segments =
        [ sessionCookieName <> "=" <> rawValue
        , "Path=" <> sessionCookiePath
        , "HttpOnly"
        , "SameSite=" <> sessionCookieSameSite
        ]
        <> maybe [] (\domainVal -> ["Domain=" <> domainVal]) sessionCookieDomain
        <> ["Max-Age=" <> T.pack (show maxAge) | maxAge <- maybeToList sessionCookieMaxAgeSeconds]
        <> ["Secure" | sessionCookieSecure]
  in T.intercalate "; " segments
type instance AuthServerData (AuthProtect "bearer-token") = AuthedUser
