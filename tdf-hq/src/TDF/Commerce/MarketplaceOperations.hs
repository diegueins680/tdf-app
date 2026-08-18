{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.MarketplaceOperations
  ( MarketplaceCustomerRequestKind(..)
  , MarketplaceCustomerRequestStatus(..)
  , MarketplaceCustomerReviewAction(..)
  , MarketplaceDepositSettlementMethod(..)
  , parseMarketplaceCustomerRequestKind
  , marketplaceCustomerRequestKindText
  , parseMarketplaceCustomerRequestStatus
  , marketplaceCustomerRequestStatusText
  , parseMarketplaceCustomerReviewAction
  , marketplaceCustomerReviewStatus
  , parseMarketplaceDepositSettlementMethod
  , marketplaceDepositSettlementMethodText
  , validateMarketplaceCustomerRequest
  , validateMarketplaceCustomerReview
  , validateMarketplaceDepositSettlement
  , validateIndependentDepositReviewer
  ) where

import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day)

data MarketplaceCustomerRequestKind
  = SaleCancellationRequest
  | SaleReturnRequest
  | RentalCancellationRequest
  | RentalExtensionRequest
  | RentalDisputeRequest
  deriving (Eq, Show)

data MarketplaceCustomerRequestStatus
  = CustomerRequestSubmitted
  | CustomerRequestNeedsQuote
  | CustomerRequestApproved
  | CustomerRequestRejected
  deriving (Eq, Show)

data MarketplaceCustomerReviewAction
  = CustomerRequestApprove
  | CustomerRequestReject
  | CustomerRequestNeedsQuoteAction
  | CustomerRequestRequireReconciliation
  deriving (Eq, Show)

data MarketplaceDepositSettlementMethod
  = DepositBankTransfer
  | DepositCash
  | DepositPos
  | DepositForfeiture
  deriving (Eq, Show)

parseMarketplaceCustomerRequestKind :: Text -> Either Text MarketplaceCustomerRequestKind
parseMarketplaceCustomerRequestKind rawKind =
  case T.toLower (T.strip rawKind) of
    "sale_cancellation" -> Right SaleCancellationRequest
    "sale_return" -> Right SaleReturnRequest
    "rental_cancellation" -> Right RentalCancellationRequest
    "rental_extension" -> Right RentalExtensionRequest
    "rental_dispute" -> Right RentalDisputeRequest
    _ -> Left "Unknown marketplace customer request type"

marketplaceCustomerRequestKindText :: MarketplaceCustomerRequestKind -> Text
marketplaceCustomerRequestKindText requestKind = case requestKind of
  SaleCancellationRequest -> "sale_cancellation"
  SaleReturnRequest -> "sale_return"
  RentalCancellationRequest -> "rental_cancellation"
  RentalExtensionRequest -> "rental_extension"
  RentalDisputeRequest -> "rental_dispute"

parseMarketplaceCustomerRequestStatus :: Text -> Either Text MarketplaceCustomerRequestStatus
parseMarketplaceCustomerRequestStatus rawStatus =
  case T.toLower (T.strip rawStatus) of
    "submitted" -> Right CustomerRequestSubmitted
    "needs_quote" -> Right CustomerRequestNeedsQuote
    "approved" -> Right CustomerRequestApproved
    "rejected" -> Right CustomerRequestRejected
    _ -> Left "Unknown marketplace customer request status"

marketplaceCustomerRequestStatusText :: MarketplaceCustomerRequestStatus -> Text
marketplaceCustomerRequestStatusText status = case status of
  CustomerRequestSubmitted -> "submitted"
  CustomerRequestNeedsQuote -> "needs_quote"
  CustomerRequestApproved -> "approved"
  CustomerRequestRejected -> "rejected"

parseMarketplaceCustomerReviewAction :: Text -> Either Text MarketplaceCustomerReviewAction
parseMarketplaceCustomerReviewAction rawAction =
  case T.toLower (T.strip rawAction) of
    "approve" -> Right CustomerRequestApprove
    "reject" -> Right CustomerRequestReject
    "needs_quote" -> Right CustomerRequestNeedsQuoteAction
    "requires_reconciliation" -> Right CustomerRequestRequireReconciliation
    _ -> Left "Review action must be approve, reject, needs_quote, or requires_reconciliation"

marketplaceCustomerReviewStatus
  :: MarketplaceCustomerReviewAction
  -> Either Text MarketplaceCustomerRequestStatus
marketplaceCustomerReviewStatus action = case action of
  CustomerRequestApprove -> Right CustomerRequestApproved
  CustomerRequestReject -> Right CustomerRequestRejected
  CustomerRequestNeedsQuoteAction -> Right CustomerRequestNeedsQuote
  CustomerRequestRequireReconciliation ->
    Left "requires_reconciliation is only valid for deposit settlement review"

parseMarketplaceDepositSettlementMethod
  :: Text
  -> Either Text MarketplaceDepositSettlementMethod
parseMarketplaceDepositSettlementMethod rawMethod =
  case T.toLower (T.strip rawMethod) of
    "bank_transfer" -> Right DepositBankTransfer
    "cash" -> Right DepositCash
    "pos" -> Right DepositPos
    "forfeiture" -> Right DepositForfeiture
    _ -> Left "Deposit settlement method must be bank_transfer, cash, pos, or forfeiture"

marketplaceDepositSettlementMethodText :: MarketplaceDepositSettlementMethod -> Text
marketplaceDepositSettlementMethodText method = case method of
  DepositBankTransfer -> "bank_transfer"
  DepositCash -> "cash"
  DepositPos -> "pos"
  DepositForfeiture -> "forfeiture"

validateMarketplaceCustomerRequest
  :: MarketplaceCustomerRequestKind
  -> Text
  -> Text
  -> Maybe Day
  -> Maybe Day
  -> Either Text ()
validateMarketplaceCustomerRequest requestKind rawOrderKind rawDomainStatus currentEnd requestedEnd =
  case requestKind of
    SaleCancellationRequest
      | orderKind /= "sale" -> wrongDomain
      | domainStatus `elem` ["ready_to_fulfill", "picking", "ready_for_pickup"] -> Right ()
      | otherwise -> Left "Sale cancellation can only be requested before shipment or delivery"
    SaleReturnRequest
      | orderKind /= "sale" -> wrongDomain
      | domainStatus == "delivered" -> Right ()
      | otherwise -> Left "Sale return can only be requested after verified delivery"
    RentalCancellationRequest
      | orderKind /= "rental" -> wrongDomain
      | domainStatus `elem` ["confirmed", "ready_for_handoff"] -> Right ()
      | otherwise -> Left "Rental cancellation can only be requested before handoff"
    RentalExtensionRequest
      | orderKind /= "rental" -> wrongDomain
      | domainStatus `notElem` ["confirmed", "ready_for_handoff", "checked_out", "return_due"] ->
          Left "Rental extension is unavailable in the current rental state"
      | Just oldEnd <- currentEnd
      , Just newEnd <- requestedEnd
      , newEnd > oldEnd -> Right ()
      | otherwise -> Left "Rental extension date must be later than the current return date"
    RentalDisputeRequest
      | orderKind /= "rental" -> wrongDomain
      | domainStatus `elem`
          [ "checked_out", "return_due", "returned_pending_inspection"
          , "damage_review", "deposit_refund_due", "lost"
          ] -> Right ()
      | otherwise -> Left "Rental dispute is unavailable in the current rental state"
  where
    orderKind = T.toLower (T.strip rawOrderKind)
    domainStatus = T.toLower (T.strip rawDomainStatus)
    wrongDomain = Left "Customer request type does not match the marketplace order kind"

validateMarketplaceCustomerReview
  :: MarketplaceCustomerRequestKind
  -> MarketplaceCustomerRequestStatus
  -> MarketplaceCustomerReviewAction
  -> Either Text MarketplaceCustomerRequestStatus
validateMarketplaceCustomerReview requestKind currentStatus action = do
  nextStatus <- marketplaceCustomerReviewStatus action
  if requestKind == RentalExtensionRequest && nextStatus == CustomerRequestApproved
    then Left "Rental extensions require a versioned quote, atomic availability check, and payable change order"
    else if transitionAllowed currentStatus nextStatus
      then Right nextStatus
      else Left "Marketplace customer request review transition is not allowed"
  where
    transitionAllowed CustomerRequestSubmitted target =
      target `elem` [CustomerRequestNeedsQuote, CustomerRequestApproved, CustomerRequestRejected]
    transitionAllowed CustomerRequestNeedsQuote target = target == CustomerRequestRejected
    transitionAllowed _ _ = False

validateMarketplaceDepositSettlement
  :: MarketplaceDepositSettlementMethod
  -> Int64
  -> Int64
  -> Int64
  -> Either Text ()
validateMarketplaceDepositSettlement method depositAmount deductionAmount refundAmount
  | depositAmount <= 0 = Left "Rental deposit must be positive before settlement"
  | deductionAmount < 0 || deductionAmount > depositAmount =
      Left "Rental deposit deduction is outside the collected deposit"
  | refundAmount /= depositAmount - deductionAmount =
      Left "Rental deposit refund does not match deposit minus deduction"
  | method == DepositForfeiture && refundAmount /= 0 =
      Left "Deposit forfeiture requires a zero refund amount"
  | method /= DepositForfeiture && refundAmount <= 0 =
      Left "Manual deposit refund requires a positive refund amount"
  | otherwise = Right ()

validateIndependentDepositReviewer :: Int64 -> Int64 -> Either Text ()
validateIndependentDepositReviewer submittedBy reviewedBy
  | submittedBy == reviewedBy = Left "Deposit settlement requires an independent reviewer"
  | otherwise = Right ()
