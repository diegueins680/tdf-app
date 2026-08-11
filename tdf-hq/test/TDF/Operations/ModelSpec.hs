{-# LANGUAGE OverloadedStrings #-}

module TDF.Operations.ModelSpec (spec) where

import Data.Either (isLeft, isRight)
import Test.Hspec
import Test.QuickCheck (Gen, elements, forAll, property)

import TDF.Models (RoleEnum(..))
import TDF.Operations.Model
import TDF.Operations.Types (WorkPriority(..), WorkStatus(..))

spec :: Spec
spec = do
  describe "operations lifecycle model" $ do
    it "accepts every declared edge when its guards are satisfied" $
      property $ forAll genStatus $ \current ->
        all (isRight . validateTransition . validContext current) (allowedTargets current)

    it "rejects every undeclared edge" $
      property $ forAll genStatus $ \current ->
        all (isLeft . validateTransition . validContext current)
          (filter (`notElem` allowedTargets current) allStatuses)

    it "requires an explicit external-dependency classification for Waiting" $ do
      validateTransition (validContext WorkInProgress WorkWaiting)
        { waitingExternalDependency = Nothing }
        `shouldBe` Left WaitingDependencyClassificationRequired

    it "requires a human reason for resolving, waiting, and archiving" $ do
      validateTransition (validContext WorkInProgress WorkResolved) { reason = Nothing }
        `shouldBe` Left TransitionReasonRequired
      validateTransition (validContext WorkInProgress WorkWaiting) { reason = Just "  " }
        `shouldBe` Left TransitionReasonRequired
      validateTransition (validContext WorkResolved WorkArchived) { reason = Nothing }
        `shouldBe` Left TransitionReasonRequired

    it "keeps reopening explicit and retains archived as a reversible lifecycle state" $ do
      allowedTargets WorkResolved `shouldContain` [WorkInProgress]
      allowedTargets WorkArchived `shouldBe` [WorkInProgress]

  describe "operations authorization and approval model" $ do
    it "never exposes security incidents to non-admin roles, even with a known id" $ do
      canViewEntityType [Manager] False "security_incident" `shouldBe` False
      canViewEntityType [Admin] False "security_incident" `shouldBe` True

    it "limits teachers and engineers to assigned minimum-necessary domains" $ do
      canViewEntityType [Teacher] False "course_registration" `shouldBe` False
      canViewEntityType [Teacher] True "course_registration" `shouldBe` True
      canViewEntityType [Engineer] True "invoice" `shouldBe` False

    it "classifies every consequential financial action for dual approval" $ do
      map (\action -> requiresTwoPersonApproval action Nothing 0)
        [ "refund", "payment_reversal", "payment_void", "chargeback_resolution"
        , "cancel_paid_reservation", "cancel_near_term_reservation"
        , "credit_note", "debit_note", "modify_issued_document", "privacy_erasure"
        ] `shouldSatisfy` and
      requiresTwoPersonApproval "custom_financial_action" (Just 10000) 10000 `shouldBe` True
      requiresTwoPersonApproval "ordinary_note" (Just 10000) 9999 `shouldBe` False

  describe "priority and SLA policy" $ do
    it "makes security/outage and multi-customer failures continuous urgent work" $ do
      recommendedPriority "security.unauthorized_access" False False `shouldBe` Urgent
      recommendedPriority "payment.failed" False True `shouldBe` Urgent
      prioritySlaMinutes Urgent `shouldBe` (15, 60, 240)

    it "preserves configured default target minutes" $ do
      prioritySlaMinutes High `shouldBe` (60, 60, 480)
      prioritySlaMinutes Normal `shouldBe` (240, 240, 1440)
      prioritySlaMinutes Low `shouldBe` (480, 480, 2400)

allStatuses :: [WorkStatus]
allStatuses = [minBound .. maxBound]

genStatus :: Gen WorkStatus
genStatus = elements allStatuses

validContext :: WorkStatus -> WorkStatus -> TransitionContext
validContext current target = TransitionContext
  { currentStatus = current
  , targetStatus = target
  , actorRoles = [Admin]
  , hasAssignee = True
  , reason = Just "validated operational reason"
  , waitingExternalDependency = if target == WorkWaiting then Just True else Nothing
  , resumeAtPresent = target == WorkWaiting
  }
