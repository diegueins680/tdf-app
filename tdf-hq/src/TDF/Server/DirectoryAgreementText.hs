{-# LANGUAGE OverloadedStrings #-}

-- | Pure text of the marketplace agreement handlers (ADR 0135, ADR 0136): the
-- SQL projections and fragments shared by the agreement queries and the
-- notification copy for each agreement state. Kept apart from
-- "TDF.Server.Directory" so that module stays within its size limit; nothing
-- here runs a query or decides anything.
module TDF.Server.DirectoryAgreementText
  ( agreementPricingSql
  , agreementProjectionSql
  , agreementPaymentProjectionSql
  , agreementFromSql
  , liveAttemptStatusesSql
  , payoutAccountProjectionSql
  , agreementNotificationType
  , agreementNotificationTitle
  , agreementNotificationBody
  ) where

import Data.Text (Text)

import TDF.Directory.Policy (AgreementStatus(..))

-- Pricing rule that applies to a classified: its category's active rule, or
-- the default one. Expects the alias "classified" in scope.
agreementPricingSql :: Text
agreementPricingSql =
  "JOIN LATERAL (SELECT rule.commission_bps,rule.deposit_bps,rule.deposit_due_hours,rule.cancellation_policy FROM marketplace_agreement_pricing rule WHERE rule.active AND (rule.category_id=classified.category_id OR rule.category_id IS NULL) ORDER BY (rule.category_id IS NULL),rule.created_at DESC LIMIT 1) pricing ON TRUE"

-- Typed agreement projection. Both parties see the same frozen figures: the
-- total, the deposit/balance split, the TDF commission and the provider's
-- net. The participant role is interpolated as a SQL literal or expression by
-- the caller. Expects the aliases agreement, classified, buyer and provider.
agreementProjectionSql :: Text -> Text
agreementProjectionSql roleSql =
  "jsonb_build_object('id',agreement.id,'applicationId',agreement.application_id,'classified',jsonb_build_object('id',classified.id,'title',classified.title,'slug',classified.slug),'buyerProfile',jsonb_build_object('id',buyer.id,'name',buyer.public_name,'slug',buyer.slug),'providerProfile',jsonb_build_object('id',provider.id,'name',provider.public_name,'slug',provider.slug),'amountMinor',agreement.amount_minor,'currencyId',agreement.currency_id,'commissionBps',agreement.commission_bps,'commissionMinor',agreement.commission_minor,'providerNetMinor',agreement.amount_minor-agreement.commission_minor,'depositBps',agreement.deposit_bps,'depositMinor',agreement.deposit_minor,'balanceMinor',agreement.balance_minor,'terms',agreement.terms,'cancellationPolicy',agreement.cancellation_policy,'settlementMode',agreement.settlement_mode,'status',agreement.status,'participantRole'," <> roleSql <> ",'startsAt',agreement.starts_at,'endsAt',agreement.ends_at,'startedAt',agreement.started_at,'completedAt',agreement.completed_at,'cancelledAt',agreement.cancelled_at,'cancellationReason',agreement.cancellation_reason,'interactionId',agreement.interaction_id,'createdAt',agreement.created_at,'updatedAt',agreement.updated_at,'version',agreement.version)||jsonb_build_object('depositDueAt',agreement.deposit_due_at,'payments',(SELECT coalesce(jsonb_agg(" <> agreementPaymentProjectionSql <> " ORDER BY payment.created_at,payment.id),'[]'::jsonb) FROM marketplace_agreement_payment payment WHERE payment.agreement_id=agreement.id AND payment.status<>'cancelled'),'payoutProviders',(SELECT coalesce(jsonb_agg(account.provider ORDER BY account.provider),'[]'::jsonb) FROM marketplace_payout_account account WHERE account.profile_id=agreement.provider_profile_id AND account.status='ready'))"

-- One charge of an agreement with its frozen split. The payout recipient is
-- never projected. Expects the alias payment.
agreementPaymentProjectionSql :: Text
agreementPaymentProjectionSql =
  "jsonb_build_object('id',payment.id,'kind',payment.kind,'provider',payment.provider,'status',payment.status,'amountMinor',payment.amount_minor,'currency',payment.currency,'platformFeeMinor',payment.platform_fee_minor,'processingFeeMinor',payment.processing_fee_minor,'processingFeePayer',payment.processing_fee_payer,'providerAmountMinor',payment.provider_amount_minor,'dueAt',payment.due_at,'paidAt',payment.paid_at,'createdAt',payment.created_at)"

agreementFromSql :: Text
agreementFromSql =
  " FROM marketplace_agreement agreement JOIN classified ON classified.id=agreement.classified_id JOIN directory_profile buyer ON buyer.id=agreement.buyer_profile_id JOIN directory_profile provider ON provider.id=agreement.provider_profile_id "

-- Payment attempt states that may still collect money. Trusted SQL literal.
liveAttemptStatusesSql :: Text
liveAttemptStatusesSql = "('created','requires_customer_action','processing','requires_review','succeeded')"

-- Payout account as its managers see it: the recipient is masked.
payoutAccountProjectionSql :: Text
payoutAccountProjectionSql =
  "jsonb_build_object('id',account.id,'profileId',account.profile_id,'profileName',profile.public_name,'provider',account.provider,'identifierType',account.identifier_type,'identifierMasked',repeat('*',greatest(length(account.identifier)-4,0))||right(account.identifier,4),'status',account.status,'updatedAt',account.updated_at,'version',account.version) FROM marketplace_payout_account account JOIN directory_profile profile ON profile.id=account.profile_id "

agreementNotificationType :: AgreementStatus -> Text
agreementNotificationType status = case status of
  AgreementInProgress -> "marketplace.agreement.started"
  AgreementCompleted -> "marketplace.agreement.completed"
  AgreementCancelled -> "marketplace.agreement.cancelled"
  AgreementDisputed -> "marketplace.agreement.disputed"
  AgreementBalanceDue -> "marketplace.agreement.balance_due"
  AgreementExpired -> "marketplace.agreement.expired"
  _ -> "marketplace.agreement.created"

agreementNotificationTitle :: AgreementStatus -> Text
agreementNotificationTitle status = case status of
  AgreementInProgress -> "Acuerdo en curso"
  AgreementCompleted -> "Acuerdo completado"
  AgreementCancelled -> "Acuerdo cancelado"
  AgreementDisputed -> "Acuerdo en disputa"
  AgreementBalanceDue -> "Saldo pendiente"
  AgreementExpired -> "Acuerdo vencido"
  _ -> "Acuerdo actualizado"

agreementNotificationBody :: AgreementStatus -> Text
agreementNotificationBody status = case status of
  AgreementInProgress -> "El proveedor marcó el trabajo como iniciado."
  AgreementCompleted -> "El acuerdo se completó. Ya pueden dejarse una reseña verificada."
  AgreementCancelled -> "La otra parte canceló el acuerdo."
  AgreementDisputed -> "La otra parte abrió una disputa sobre el acuerdo."
  AgreementBalanceDue -> "El comprador confirmó la entrega. El acuerdo se completa cuando se pague el saldo."
  AgreementExpired -> "El depósito no se pagó a tiempo y el acuerdo venció. El cupo del anuncio quedó libre."
  _ -> "El acuerdo cambió de estado."
