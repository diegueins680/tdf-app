{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module TDF.Auth
  ( AuthedUser(..)
  , ModuleAccess(..)
  , authContext
  , hasModuleAccess
  , moduleName
  , modulesForRoles
  , loadAuthedUser
  , lookupUsernameFromToken
  , resolveUsernameFromLabel
  ) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (guard)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy       as BL
import           Data.List                  (foldl')
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Database.Persist           (Entity(..), getBy, selectList, (==.))
import           Database.Persist.Sql       (SqlPersistT, runSqlPool)
import           Network.Wai                (Request, requestHeaders)
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler, AuthServerData)
import           Servant.API.Experimental.Auth    (AuthProtect)

import           TDF.DB                     (Env(..))
import           TDF.Models

-- | Enumeration of major application modules.
data ModuleAccess
  = ModuleCRM
  | ModuleScheduling
  | ModulePackages
  | ModuleInvoicing
  | ModuleAdmin
  deriving (Eq, Ord, Show, Enum, Bounded)

moduleName :: ModuleAccess -> Text
moduleName ModuleCRM        = "CRM"
moduleName ModuleScheduling = "Scheduling"
moduleName ModulePackages   = "Packages"
moduleName ModuleInvoicing  = "Invoicing"
moduleName ModuleAdmin      = "Admin"

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

-- Internal -----------------------------------------------------------------

authWithToken :: Env -> Request -> Handler AuthedUser
authWithToken env req = do
  token <- either throw401 pure (extractToken req)
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
          let roleList = map (partyRoleRole . entityVal) roles
              modules  = modulesForRoles roleList
          pure $ Just AuthedUser
            { auPartyId = apiTokenPartyId tok
            , auRoles   = roleList
            , auModules = modules
            }

lookupUsernameFromToken :: Text -> SqlPersistT IO (Maybe Text)
lookupUsernameFromToken token = do
  mToken <- getBy (UniqueApiToken token)
  pure $ do
    Entity _ tok <- mToken
    guard (apiTokenActive tok)
    label <- apiTokenLabel tok
    resolveUsernameFromLabel label

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
modulesForRole Admin      = Set.fromList [ModuleCRM, ModuleScheduling, ModulePackages, ModuleInvoicing, ModuleAdmin]
modulesForRole Manager    = Set.fromList [ModuleCRM, ModuleScheduling, ModulePackages, ModuleInvoicing]
modulesForRole StudioManager = Set.fromList [ModuleCRM, ModuleScheduling, ModulePackages, ModuleInvoicing]
modulesForRole Reception  = Set.fromList [ModuleCRM, ModuleScheduling]
modulesForRole Accounting = Set.singleton ModuleInvoicing
modulesForRole Engineer   = Set.singleton ModuleScheduling
modulesForRole Teacher    = Set.singleton ModuleScheduling
modulesForRole LiveSessionsProducer = Set.fromList [ModuleCRM, ModuleScheduling]
modulesForRole Artist     = Set.fromList [ModuleScheduling, ModulePackages]
modulesForRole Artista    = Set.fromList [ModuleScheduling, ModulePackages]
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

extractToken :: Request -> Either Text Text
extractToken req =
  case lookup "Authorization" (requestHeaders req) of
    Nothing   -> Left "Missing Authorization header"
    Just hdr  -> parseHeader (TE.decodeUtf8' hdr)
  where
    parseHeader (Left _) = Left "Invalid Authorization header encoding"
    parseHeader (Right txt) =
      case T.words txt of
        [scheme, value]
          | T.toLower scheme == "bearer" -> Right value
          | otherwise                    -> Left "Unsupported authorization scheme"
        _ -> Left "Malformed Authorization header"
type instance AuthServerData (AuthProtect "bearer-token") = AuthedUser
