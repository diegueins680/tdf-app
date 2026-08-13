{-# LANGUAGE OverloadedStrings #-}

module TDF.Catalog.SecuritySpec (spec) where

import Data.Aeson (eitherDecodeStrict')
import qualified Data.ByteString.Char8 as BS
import Data.Either (isLeft)
import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Hspec

import TDF.API.Catalog
  ( PartyRoleGrantDraftRequest (..)
  , SecurityGrantReviewRequest (..)
  , SelfFanRoleRequest (..)
  )
import TDF.Catalog.Security
  ( SecurityRegistrySnapshot (..)
  , expectedSecurityActionCodes
  , expectedSecurityModuleCodes
  , expectedSecurityPermissionBindings
  , expectedSecurityRoleAssignmentPolicyBindings
  , expectedSecurityRoleCodes
  , validateSecurityRegistrySnapshot
  )

spec :: Spec
spec = do
  describe "security registry startup validation" $ do
    it "accepts the complete code-recognized registry" $
      validateSecurityRegistrySnapshot validSnapshot `shouldBe` Right ()

    it "fails closed when a recognized module, action, role, or permission is missing" $ do
      expectFailure "missing code-recognized modules" $
        validSnapshot {srsModuleCodes = Set.delete "catalog" expectedSecurityModuleCodes}
      expectFailure "missing code-recognized actions" $
        validSnapshot {srsActionCodes = Set.delete "read" expectedSecurityActionCodes}
      expectFailure "missing code-recognized roles" $
        validSnapshot {srsRoleCodes = Set.delete "admin" expectedSecurityRoleCodes}
      expectFailure "missing code-recognized permissions" $
        validSnapshot
          { srsPermissionBindings =
              Set.delete ("catalog.read", "catalog", "read", "catalog") expectedSecurityPermissionBindings
          }
      expectFailure "missing code-recognized permissions" $
        validSnapshot
          { srsPermissionBindings =
              Set.delete ("pipeline.update", "scheduling", "update", "pipeline") expectedSecurityPermissionBindings
          }
      expectFailure "missing automatic assignment policies" $
        validSnapshot
          { srsAutomaticPolicyBindings =
              Set.delete
                ("account.signup.customer", "account-signup", "customer", False)
                expectedSecurityRoleAssignmentPolicyBindings
          }

    it "rejects unknown active permissions before they can grant access" $
      expectFailure "unknown active permissions" $
        validSnapshot
          { srsPermissionBindings =
              Set.insert
                ("catalog.unknown-startup-test", "catalog", "read", "catalog")
                expectedSecurityPermissionBindings
          }

    it "rejects known permission codes bound to a different capability" $
      expectFailure "permissions bound to the wrong module, action, or scope" $
        validSnapshot
          { srsPermissionBindings =
              Set.insert
                ("catalog.read", "catalog", "read", "wrong-scope")
                (Set.delete ("catalog.read", "catalog", "read", "catalog") expectedSecurityPermissionBindings)
          }

    it "rejects unknown or altered automatic role assignment policies" $
      expectFailure "unknown or altered automatic assignment policies" $
        validSnapshot
          { srsAutomaticPolicyBindings =
              Set.insert
                ("account.signup.customer", "account-signup", "admin", False)
                (Set.delete
                  ("account.signup.customer", "account-signup", "customer", False)
                  expectedSecurityRoleAssignmentPolicyBindings)
          }

    it "rejects unknown granted permissions and assigned roles" $ do
      expectFailure "unknown permission codes with active grants" $
        validSnapshot
          { srsGrantedPermissionCodes = Set.insert "catalog.unknown-grant" expectedPermissionCodes
          }
      expectFailure "unknown role codes with active assignments" $
        validSnapshot
          { srsAssignedRoleCodes = Set.insert "unknown-role" expectedSecurityRoleCodes
          }

  describe "security grant JSON contracts" $ do
    it "accepts only canonical role IDs and the complete typed draft shape" $
      eitherDecodeStrict'
        (BS.pack "{\"partyId\":42,\"roleId\":\"f683a8fc-39aa-4635-a56d-b1e43e603a9f\",\"desiredActive\":true,\"expectedVersion\":0,\"reason\":\"Reviewed access request\",\"sourcePlatform\":\"web\",\"correlationId\":\"security-test-001\"}")
        `shouldBe` Right
          ( PartyRoleGrantDraftRequest
              42
              "f683a8fc-39aa-4635-a56d-b1e43e603a9f"
              True
              0
              "Reviewed access request"
              "web"
              "security-test-001"
          )

    it "rejects legacy role strings and unknown compatibility fields" $
      ( eitherDecodeStrict'
          (BS.pack "{\"partyId\":42,\"roleId\":\"f683a8fc-39aa-4635-a56d-b1e43e603a9f\",\"role\":\"Admin\",\"desiredActive\":true,\"expectedVersion\":0,\"reason\":\"Reviewed access request\",\"sourcePlatform\":\"web\",\"correlationId\":\"security-test-002\"}")
          :: Either String PartyRoleGrantDraftRequest
      ) `shouldSatisfy` isLeft

    it "rejects the former boolean emergency override contract" $
      ( eitherDecodeStrict'
          (BS.pack "{\"notes\":\"Emergency review\",\"emergencyOverride\":true}")
          :: Either String SecurityGrantReviewRequest
      ) `shouldSatisfy` isLeft

    it "accepts a self-service Fan request without a caller-selected role identifier" $
      eitherDecodeStrict'
        (BS.pack "{\"reason\":\"I want to follow artists\",\"sourcePlatform\":\"web\",\"correlationId\":\"fan-request-001\"}")
        `shouldBe` Right
          (SelfFanRoleRequest "I want to follow artists" "web" "fan-request-001")

    it "rejects role ids or role strings injected into self-service Fan requests" $ do
      let injectedRoleId = eitherDecodeStrict'
            (BS.pack "{\"roleId\":\"f683a8fc-39aa-4635-a56d-b1e43e603a9f\",\"reason\":\"I want to follow artists\",\"sourcePlatform\":\"web\",\"correlationId\":\"fan-request-002\"}")
            :: Either String SelfFanRoleRequest
          injectedRoleCode = eitherDecodeStrict'
            (BS.pack "{\"role\":\"Admin\",\"reason\":\"I want to follow artists\",\"sourcePlatform\":\"web\",\"correlationId\":\"fan-request-003\"}")
            :: Either String SelfFanRoleRequest
      injectedRoleId `shouldSatisfy` isLeft
      injectedRoleCode `shouldSatisfy` isLeft

validSnapshot :: SecurityRegistrySnapshot
validSnapshot =
  SecurityRegistrySnapshot
    { srsModuleCodes = expectedSecurityModuleCodes
    , srsActionCodes = expectedSecurityActionCodes
    , srsRoleCodes = expectedSecurityRoleCodes
    , srsPermissionBindings = expectedSecurityPermissionBindings
    , srsAutomaticPolicyBindings = expectedSecurityRoleAssignmentPolicyBindings
    , srsGrantedPermissionCodes = expectedPermissionCodes
    , srsAssignedRoleCodes = expectedSecurityRoleCodes
    }

expectedPermissionCodes :: Set.Set T.Text
expectedPermissionCodes =
  Set.map (\(code, _, _, _) -> code) expectedSecurityPermissionBindings

expectFailure :: T.Text -> SecurityRegistrySnapshot -> Expectation
expectFailure expectedMessage snapshot =
  case validateSecurityRegistrySnapshot snapshot of
    Left message -> message `shouldSatisfy` T.isInfixOf expectedMessage
    Right () -> expectationFailure "Expected security registry validation to fail"
