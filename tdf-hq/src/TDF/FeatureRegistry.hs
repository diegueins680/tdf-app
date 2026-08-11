{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TDF.FeatureRegistry
  ( FeatureRule(..)
  , RegistryFeature(..)
  , allRegistryFeatures
  , findRegistryFeature
  , registryFeatureAllows
  , registryFeatureRequestable
  , registryReviewerCanDecide
  , supportedFeatureActions
  ) where

import           Data.Aeson               (FromJSON(..), Value(..), eitherDecodeStrict', withObject,
                                           (.:), (.:?), (.!=))
import qualified Data.Aeson.Key           as Key
import qualified Data.Aeson.KeyMap        as KeyMap
import           Data.Aeson.Types         (parseEither)
import qualified Data.ByteString          as BS
import           Data.FileEmbed           (embedFile)
import           Data.List                (find)
import           Data.Maybe               (fromMaybe)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as T

import           TDF.Auth                 (AuthedUser(..), moduleName)
import           TDF.Models               (RoleEnum(Admin, Manager, StudioManager), roleToText)

data FeatureRule = FeatureRule
  { featureRuleRolesAny   :: [Text]
  , featureRuleRolesAll   :: [Text]
  , featureRuleModulesAny :: [Text]
  , featureRuleModulesAll :: [Text]
  , featureRuleStrictAdmin :: Bool
  } deriving (Eq, Show)

instance FromJSON FeatureRule where
  parseJSON = withObject "FeatureRule" $ \obj ->
    FeatureRule
      <$> obj .:? "rolesAny" .!= []
      <*> obj .:? "rolesAll" .!= []
      <*> obj .:? "modulesAny" .!= []
      <*> obj .:? "modulesAll" .!= []
      <*> obj .:? "strictAdmin" .!= False

data RegistryFeature = RegistryFeature
  { registryFeatureId                    :: Text
  , registryFeatureRequiredRoles         :: [Text]
  , registryFeatureRequiredModules       :: [Text]
  , registryFeaturePermissions           :: [(Text, FeatureRule)]
  , registryFeatureAccessRequestEligible :: Bool
  , registryFeatureTechnical             :: Bool
  , registryFeatureMaturity              :: Text
  , registryFeatureSafeLockedDisclosure  :: Bool
  , registryFeatureNavigationGroup       :: Maybe Text
  , registryFeatureFavoriteEligible      :: Bool
  , registryFeaturePinEligible           :: Bool
  , registryFeatureRecentBehavior        :: Text
  } deriving (Eq, Show)

instance FromJSON RegistryFeature where
  parseJSON = withObject "RegistryFeature" $ \obj -> do
    permissionsValue <- obj .:? "permissions" .!= Object mempty
    permissions <- case permissionsValue of
      Object values -> traverse parsePermission (KeyMap.toList values)
      _ -> fail "permissions must be an object"
    RegistryFeature
      <$> obj .: "id"
      <*> obj .:? "requiredRoles" .!= []
      <*> obj .:? "requiredModules" .!= []
      <*> pure permissions
      <*> obj .:? "accessRequestEligible" .!= True
      <*> obj .:? "technical" .!= False
      <*> obj .:? "maturity" .!= "stable"
      <*> obj .:? "safeLockedDisclosure" .!= True
      <*> obj .:? "navigationGroup"
      <*> obj .:? "favoriteEligible" .!= True
      <*> obj .:? "pinEligible" .!= True
      <*> obj .:? "recentBehavior" .!= "feature"
    where
      parsePermission (key, value) = do
        rule <- parseJSON value
        pure (T.toLower (Key.toText key), rule)

data RawRegistry = RawRegistry
  { rawRegistryDefaults :: Value
  , rawRegistryFeatures :: [Value]
  }

instance FromJSON RawRegistry where
  parseJSON = withObject "FeatureRegistry" $ \obj ->
    RawRegistry <$> obj .: "defaults" <*> obj .: "features"

registryBytes :: BS.ByteString
registryBytes = $(embedFile "assets/feature-registry.json")

allRegistryFeatures :: [RegistryFeature]
allRegistryFeatures =
  case eitherDecodeStrict' registryBytes of
    Left message -> error ("Invalid embedded feature registry: " <> message)
    Right RawRegistry{..} -> map (decodeMerged rawRegistryDefaults) rawRegistryFeatures
  where
    decodeMerged defaultsValue featureValue =
      case mergeRegistryDefaults defaultsValue featureValue >>= parseRegistryFeature of
        Left message -> error ("Invalid embedded feature registry entry: " <> message)
        Right feature -> feature

mergeRegistryDefaults :: Value -> Value -> Either String Value
mergeRegistryDefaults (Object defaults) (Object feature) =
  Right (Object (feature `KeyMap.union` defaults))
mergeRegistryDefaults _ _ = Left "defaults and features must be objects"

parseRegistryFeature :: Value -> Either String RegistryFeature
parseRegistryFeature = parseEither parseJSON

findRegistryFeature :: Text -> Maybe RegistryFeature
findRegistryFeature featureId =
  find ((== normalizeToken featureId) . normalizeToken . registryFeatureId) allRegistryFeatures

supportedFeatureActions :: Set Text
supportedFeatureActions = Set.fromList
  [ "discover", "view", "create", "edit", "delete", "archive", "deactivate"
  , "import", "export", "submit", "validate", "approve", "reject", "assign"
  , "publish", "report", "administer"
  ]

registryFeatureRequestable :: RegistryFeature -> Text -> Bool
registryFeatureRequestable feature rawAction =
  let action = normalizeToken rawAction
  in action `Set.member` supportedFeatureActions
      && registryFeatureAccessRequestEligible feature
      && registryFeatureSafeLockedDisclosure feature
      && not (registryFeatureTechnical feature)
      && registryFeatureMaturity feature `notElem` ["broken", "incomplete"]
      && (action == "view" || action == "discover" || action `elem` map fst (registryFeaturePermissions feature))

registryFeatureAllows :: [RoleEnum] -> [Text] -> RegistryFeature -> Text -> Bool
registryFeatureAllows roles modules RegistryFeature{..} rawAction =
  let action = normalizeToken rawAction
      requestedRule = lookup action registryFeaturePermissions
      actionRule = fromMaybe emptyRule requestedRule
      baseRolesAny = map normalizeToken registryFeatureRequiredRoles
      baseModulesAll = map normalizeToken registryFeatureRequiredModules
      rolesAny = if null (featureRuleRolesAny actionRule)
        then baseRolesAny
        else map normalizeToken (featureRuleRolesAny actionRule)
      rolesAll = map normalizeToken (featureRuleRolesAll actionRule)
      modulesAny = map normalizeToken (featureRuleModulesAny actionRule)
      modulesAll = baseModulesAll <> map normalizeToken (featureRuleModulesAll actionRule)
      roleSet = Set.fromList (map (normalizeToken . roleToText) roles)
      moduleSet = Set.fromList (map normalizeToken modules)
      strictAdminRoles = Set.fromList ["admin", "fan", "customer"]
      strictAdminAllowed = not (featureRuleStrictAdmin actionRule)
        || ("admin" `Set.member` roleSet && roleSet `Set.isSubsetOf` strictAdminRoles)
  in requestedRule /= Nothing
      && action `Set.member` supportedFeatureActions
      && strictAdminAllowed
      && all (`Set.member` roleSet) rolesAll
      && (null rolesAny || any (`Set.member` roleSet) rolesAny)
      && all (`Set.member` moduleSet) modulesAll
      && (null modulesAny || any (`Set.member` moduleSet) modulesAny)

registryReviewerCanDecide :: AuthedUser -> RegistryFeature -> Text -> Bool
registryReviewerCanDecide AuthedUser{..} feature action =
  let reviewerRole = any (`elem` auRoles) [Admin, Manager, StudioManager]
      modules = map moduleName (Set.toList auModules)
  in reviewerRole && registryFeatureAllows auRoles modules feature action

emptyRule :: FeatureRule
emptyRule = FeatureRule [] [] [] [] False

normalizeToken :: Text -> Text
normalizeToken = T.filter (`notElem` [' ', '-', '_', '&', '/']) . T.toLower . T.strip
