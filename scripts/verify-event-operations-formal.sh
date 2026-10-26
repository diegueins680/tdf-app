#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
MODEL_DIR="${REPO_DIR}/formal/event-operations"
JAVA_BIN="${JAVA_BIN:-java}"
TLA2TOOLS_JAR="${TLA2TOOLS_JAR:-}"
ALLOY_JAR="${ALLOY_JAR:-}"

if [[ -z "${TLA2TOOLS_JAR}" || -z "${ALLOY_JAR}" ]]; then
  echo "TLA2TOOLS_JAR and ALLOY_JAR must point to the pinned official JARs." >&2
  exit 2
fi

if [[ ! -f "${TLA2TOOLS_JAR}" || ! -f "${ALLOY_JAR}" ]]; then
  echo "Formal tool JAR not found." >&2
  exit 2
fi

sha256() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  else
    shasum -a 256 "$1" | awk '{print $1}'
  fi
}

expected_tla_sha="fa18543e44ed5974a85bd2c60c0dc16620ae117680ea8e693d2691999ed90b22"
expected_alloy_sha="6b8c1cb5bc93bedfc7c61435c4e1ab6e688a242dc702a394628d9a9801edb78d"
actual_tla_sha="$(sha256 "${TLA2TOOLS_JAR}")"
actual_alloy_sha="$(sha256 "${ALLOY_JAR}")"

if [[ "${actual_tla_sha}" != "${expected_tla_sha}" ]]; then
  echo "Unexpected TLA+ tools checksum: ${actual_tla_sha}" >&2
  exit 2
fi

if [[ "${actual_alloy_sha}" != "${expected_alloy_sha}" ]]; then
  echo "Unexpected Alloy checksum: ${actual_alloy_sha}" >&2
  exit 2
fi

run_root="$(mktemp -d "${TMPDIR:-/tmp}/tdf-event-ops-formal.XXXXXX")"
cleanup() {
  case "${run_root}" in
    */tdf-event-ops-formal.*) rm -rf -- "${run_root}" ;;
    *) echo "Refusing to remove unexpected temporary path: ${run_root}" >&2 ;;
  esac
}
trap cleanup EXIT

run_tlc() {
  local module="$1"
  local config="$2"
  local slug="$3"
  local status=0
  "${JAVA_BIN}" "-Djava.io.tmpdir=${run_root}" -XX:+UseParallelGC -jar "${TLA2TOOLS_JAR}" \
    -workers 1 \
    -metadir "${run_root}/tlc-${slug}" \
    -config "${config}" \
    "${module}" || status=$?
  if [[ "${status}" -eq 0 ]]; then
    printf 'TDF_TLC_RESULT positive %s %s\n' "${module}" "${config}"
  fi
  return "${status}"
}

cd "${MODEL_DIR}"

# Reused from PR #372: TLC checks generated TLA+, so first verify its source link.
export JAVA_BIN TLA2TOOLS_JAR
node "${SCRIPT_DIR}/verify-event-operations-pluscal.mjs"
node --test "${SCRIPT_DIR}/__tests__/event-operations-pluscal.test.mjs"

# TLC 1.7.2 materializes standard modules under java.io.tmpdir. Keep calls
# sequential within this run and isolate their temporary directory from other runs.
run_tlc NativeLanding.tla NativeLanding.cfg native-landing
run_tlc NativeArtistFollow.tla NativeArtistFollow.cfg native-artist-follow
run_tlc ExperimentAuthority.tla ExperimentAuthority.cfg experiment-authority
run_tlc ExperimentAuthority.tla ExperimentAuthorityPaused.cfg experiment-paused
run_tlc EventLifecycle.tla EventLifecycle.cfg event-lifecycle
run_tlc EventLifecycle.tla EventLifecycleBoundaries.cfg event-lifecycle-boundaries

# Negative controls must fail the named property, not merely fail to execute.
run_negative_tlc() {
  local config="$1" slug="$2" expected="$3" module="${4:-EventLifecycle.tla}" status=0
  run_tlc "${module}" "${config}" "${slug}" > "${run_root}/${slug}.log" 2>&1 || status=$?
  cat "${run_root}/${slug}.log"
  if [[ "${status}" -eq 0 ]] || ! grep -Fq "${expected}" "${run_root}/${slug}.log"; then
    echo "Negative control ${slug} did not detect ${expected}." >&2
    exit 1
  fi
  printf 'TDF_TLC_RESULT negative %s %s\n' "${module}" "${config}"
}
run_tlc OperationsCommandFence.tla OperationsCommandFence.cfg operationscommandfence
run_negative_tlc OperationsCommandFenceAllowStale.cfg operationscommandfenceallowstale 'Invariant NoStaleCommit is violated' OperationsCommandFence.tla
run_negative_tlc OperationsCommandFenceAllowRevoked.cfg operationscommandfenceallowrevoked 'Invariant NoRevokedCommit is violated' OperationsCommandFence.tla
run_tlc WhatsAppRedelivery.tla WhatsAppRedelivery.cfg whatsapp-redelivery
run_negative_tlc WhatsAppRedeliveryEveryDeliveryApplies.cfg whatsapp-redelivery-every-delivery-applies 'Invariant AppliedOnce is violated' WhatsAppRedelivery.tla
run_negative_tlc WhatsAppRedeliveryDuplicateStore.cfg whatsapp-redelivery-duplicate-store 'Invariant StoredOnce is violated' WhatsAppRedelivery.tla
run_negative_tlc WhatsAppRedeliveryEffectAfterStore.cfg whatsapp-redelivery-effect-after-store 'Invariant StoredStopApplied is violated' WhatsAppRedelivery.tla
run_negative_tlc WhatsAppRedeliveryEveryDeliveryReplies.cfg whatsapp-redelivery-every-delivery-replies 'Invariant RepliedOnce is violated' WhatsAppRedelivery.tla
run_negative_tlc OperationsCommandFenceLeakEffect.cfg operationscommandfenceleakeffect 'Invariant AtomicEvidence is violated' OperationsCommandFence.tla
run_tlc AccessGrant.tla AccessGrant.cfg accessgrant
run_negative_tlc AccessGrantSelfApproval.cfg accessgrantselfapproval 'Invariant NoSelfApproval is violated' AccessGrant.tla
run_negative_tlc AccessGrantNonAdmin.cfg accessgrantnonadmin 'Invariant AdminApprovesGrant is violated' AccessGrant.tla
run_negative_tlc AccessGrantExpired.cfg accessgrantexpired 'Invariant NoApprovalAfterExpiry is violated' AccessGrant.tla
run_negative_tlc AccessGrantRebind.cfg accessgrantrebind 'Invariant SingleRedeemer is violated' AccessGrant.tla
run_negative_tlc AccessGrantRevokedLink.cfg accessgrantrevokedlink 'Invariant RedeemedNeverRevoked is violated' AccessGrant.tla
run_negative_tlc AccessGrantReactivate.cfg accessgrantreactivate 'Invariant NoAutomaticReactivation is violated' AccessGrant.tla
run_negative_tlc AccessGrantAdminInvite.cfg accessgrantadmininvite 'Invariant InvitationNeverAdministrative is violated' AccessGrant.tla

run_tlc LedgerNamespace.tla LedgerNamespace.cfg ledger-namespace
run_negative_tlc LedgerNamespacePosting.cfg ledger-namespace-posting 'Invariant PostedBalanced is violated' LedgerNamespace.tla
run_negative_tlc LedgerNamespaceEntry.cfg ledger-namespace-entry 'Invariant PostedBalanced is violated' LedgerNamespace.tla

run_tlc MigrationLedger.tla MigrationLedger.cfg migration-ledger
run_negative_tlc MigrationLedgerRecordAfterCommit.cfg migration-ledger-record-after-commit 'Invariant EffectsRecorded is violated' MigrationLedger.tla
run_negative_tlc MigrationLedgerBatchUnchecked.cfg migration-ledger-batch-unchecked 'Invariant NoRunPastDrift is violated' MigrationLedger.tla
run_negative_tlc MigrationLedgerPrepareUnchecked.cfg migration-ledger-prepare-unchecked 'Invariant PreparationSound is violated' MigrationLedger.tla
run_negative_tlc MigrationLedgerUnknownAccepted.cfg migration-ledger-unknown-accepted 'Invariant PreparationSound is violated' MigrationLedger.tla

run_tlc StartupGates.tla StartupGates.cfg startup-gates
run_negative_tlc StartupGatesStagedAccepted.cfg startup-gates-staged-accepted 'The invariant of StartImpliesApproved is equal to FALSE' StartupGates.tla
run_negative_tlc StartupGatesMissingAccepted.cfg startup-gates-missing-accepted 'The invariant of StartImpliesApproved is equal to FALSE' StartupGates.tla
run_negative_tlc StartupGatesEitherRejected.cfg startup-gates-either-rejected 'The invariant of ApprovedStarts is equal to FALSE' StartupGates.tla
run_negative_tlc StartupGatesRequiredUnchecked.cfg startup-gates-required-unchecked 'The invariant of StartImpliesApproved is equal to FALSE' StartupGates.tla
# AUTH-OPS-READ-001: one state; every invariant quantifies over the whole finite policy, so TLC reports a constant-level failure.
run_tlc OperationsReadPolicy.tla OperationsReadPolicy.cfg operations-read-policy
run_negative_tlc OperationsReadPolicyWildcardSecurity.cfg operations-read-policy-wildcard-security 'The invariant of NoSecurityDisclosure is equal to FALSE' OperationsReadPolicy.tla
run_negative_tlc OperationsReadPolicySqlDrift.cfg operations-read-policy-sql-drift 'The invariant of ReadersAgree is equal to FALSE' OperationsReadPolicy.tla
run_negative_tlc OperationsReadPolicyUnassigned.cfg operations-read-policy-unassigned 'The invariant of SpecialistsAssigned is equal to FALSE' OperationsReadPolicy.tla
run_negative_tlc OperationsReadPolicyReadOnlyWrites.cfg operations-read-policy-readonly-writes 'The invariant of ReadOnlyNoWriteGain is equal to FALSE' OperationsReadPolicy.tla
run_negative_tlc OperationsReadPolicyUnknownGrant.cfg operations-read-policy-unknown-grant 'The invariant of UnknownNothing is equal to FALSE' OperationsReadPolicy.tla


run_tlc OperationsWorkerTick.tla OperationsWorkerTick.cfg operations-worker-tick
run_negative_tlc OperationsWorkerTickDeferredCheck.cfg operations-worker-tick-deferred-check 'Invariant PoisonIsolated is violated' OperationsWorkerTick.tla
run_negative_tlc OperationsWorkerTickNoAttempt.cfg operations-worker-tick-no-attempt 'Invariant AttemptsRecorded is violated' OperationsWorkerTick.tla
run_negative_tlc OperationsWorkerTickUnboundedRetry.cfg operations-worker-tick-unbounded-retry 'Invariant DeadLetterAtBound is violated' OperationsWorkerTick.tla
run_tlc InvoiceReceipt.tla InvoiceReceipt.cfg invoicereceipt
run_tlc ApplicationCanary.tla ApplicationCanary.cfg applicationcanary
run_negative_tlc ApplicationCanaryNetwork.cfg applicationcanarynetwork 'Invariant NoProductionConnectivity is violated' ApplicationCanary.tla
run_negative_tlc ApplicationCanaryCleanup.cfg applicationcanarycleanup 'Invariant DatabaseOutlivesApp is violated' ApplicationCanary.tla
run_negative_tlc ApplicationCanaryFalseGreen.cfg applicationcanaryfalsegreen 'Invariant VerifiedReceipt is violated' ApplicationCanary.tla
run_tlc ReleaseJournal.tla ReleaseJournal.cfg releasejournal
run_negative_tlc ReleaseJournalNoIntent.cfg releasejournalnointent 'Invariant DurableIntentBeforeEffect is violated' ReleaseJournal.tla
run_negative_tlc ReleaseJournalNewNonce.cfg releasejournalnewnonce 'Invariant OneReservedRelease is violated' ReleaseJournal.tla
run_negative_tlc ReleaseJournalWrongObservation.cfg releasejournalwrongobservation 'Invariant BoundCompletion is violated' ReleaseJournal.tla
# Separate terminal-succession contract; original journal model stays unchanged.
run_tlc ReleaseJournalSuccession.tla ReleaseJournalSuccession.cfg releasejournalsuccession
run_tlc ReleaseJournalSuccession.tla ReleaseJournalSuccessionTwo.cfg releasejournalsuccessiontwo
run_negative_tlc ReleaseJournalSuccessionUnlocked.cfg releasejournalsuccessionunlocked 'Invariant OneGlobalLock is violated' ReleaseJournalSuccession.tla
run_negative_tlc ReleaseJournalSuccessionPending.cfg releasejournalsuccessionpending 'Invariant TerminalPredecessor is violated' ReleaseJournalSuccession.tla
run_negative_tlc ReleaseJournalSuccessionStaleTip.cfg releasejournalsuccessionstaletip 'Invariant ExactTipCompared is violated' ReleaseJournalSuccession.tla
run_negative_tlc ReleaseJournalSuccessionReusedNonce.cfg releasejournalsuccessionreusednonce 'Invariant UniqueNonces is violated' ReleaseJournalSuccession.tla
run_negative_tlc ReleaseJournalSuccessionNoIntent.cfg releasejournalsuccessionnointent 'Invariant DurableIntentBeforeEffect is violated' ReleaseJournalSuccession.tla
run_negative_tlc ReleaseJournalSuccessionWrongObservation.cfg releasejournalsuccessionwrongobservation 'Invariant BoundCompletion is violated' ReleaseJournalSuccession.tla
run_negative_tlc ReleaseJournalSuccessionDiscardHistory.cfg releasejournalsuccessiondiscardhistory 'Invariant HistoryChain is violated' ReleaseJournalSuccession.tla
run_negative_tlc ReleaseJournalSuccessionForgetWrites.cfg releasejournalsuccessionforgetwrites 'Invariant WritesNeverForgotten is violated' ReleaseJournalSuccession.tla
# Reachability witness, NOT a guard mutation: the correct model must reach three
# complete releases and violate this intentionally false invariant. It uses the
# existing expected-counterexample helper; report it separately from mutations.
run_negative_tlc ReleaseJournalSuccessionWitness.cfg releasejournalsuccessionwitness 'Invariant ThreeCompletedUnreachable is violated' ReleaseJournalSuccession.tla
run_tlc LogicalBackup.tla LogicalBackup.cfg logicalbackup
run_negative_tlc LogicalBackupLostParent.cfg logicalbackuplostparent 'Invariant OneInflightBackup is violated' LogicalBackup.tla
run_negative_tlc LogicalBackupFailedWork.cfg logicalbackupfailedwork 'Invariant CompletedReceipt is violated' LogicalBackup.tla
run_negative_tlc LogicalBackupChangedSource.cfg logicalbackupchangedsource 'Invariant SourceBoundReceipt is violated' LogicalBackup.tla
run_negative_tlc InvoiceReceiptIgnoreInvoiceLock.cfg invoicereceiptnolock 'Invariant UniqueInvoice is violated' InvoiceReceipt.tla
run_negative_tlc InvoiceReceiptStaleCounter.cfg invoicereceiptstalecounter 'Invariant UniqueNumber is violated' InvoiceReceipt.tla
run_negative_tlc InvoiceReceiptIgnoreReplayBinding.cfg invoicereceiptunbound 'Invariant BoundReplay is violated' InvoiceReceipt.tla
run_negative_tlc InvoiceReceiptIgnoreAuthorization.cfg invoicereceiptunauthorized 'Invariant AuthorizedCommit is violated' InvoiceReceipt.tla
run_tlc NotificationLease.tla NotificationLease.cfg notificationlease
run_negative_tlc NotificationLeaseStaleClock.cfg notificationleasestale 'Invariant UnexpiredCompletion is violated' NotificationLease.tla
run_negative_tlc NotificationLeaseForeign.cfg notificationleaseforeign 'Invariant CurrentLease is violated' NotificationLease.tla
run_negative_tlc NotificationLeaseClaimClock.cfg notificationleaseclaimclock 'Invariant FreshClaim is violated' NotificationLease.tla
run_tlc OperationsManualReceipt.tla OperationsManualReceipt.cfg operationsmanualreceipt
run_tlc ManualBankTransfer.tla ManualBankTransfer.cfg manualbanktransfer
run_negative_tlc ManualBankTransferSharedReviewer.cfg manualbanktransfersharedreviewer 'Invariant IndependentApproval is violated' ManualBankTransfer.tla
run_negative_tlc ManualBankTransferOutsider.cfg manualbanktransferoutsider 'Invariant IndependentApproval is violated' ManualBankTransfer.tla
run_negative_tlc ManualBankTransferExpired.cfg manualbanktransferexpired 'Invariant LiveApproval is violated' ManualBankTransfer.tla
run_negative_tlc ManualBankTransferUnbounded.cfg manualbanktransferunbounded 'Invariant HoldBound is violated' ManualBankTransfer.tla
run_negative_tlc ManualBankTransferSecondRail.cfg manualbanktransfersecondrail 'Invariant NoSecondCharge is violated' ManualBankTransfer.tla
run_negative_tlc ManualBankTransferReplay.cfg manualbanktransferreplay 'Invariant SingleCharge is violated' ManualBankTransfer.tla
run_negative_tlc ManualBankTransferReopen.cfg manualbanktransferreopen 'Invariant ExpiryFinal is violated' ManualBankTransfer.tla
run_negative_tlc ManualBankTransferSplitIssuance.cfg manualbanktransfersplitissuance 'Invariant PaidHasTickets is violated' ManualBankTransfer.tla
run_tlc TaxDocumentSubmission.tla TaxDocumentSubmission.cfg taxdocumentsubmission
run_negative_tlc TaxDocumentSubmissionSkipStart.cfg taxdocumentsubmissionskipstart 'Invariant AtMostOneDocument is violated' TaxDocumentSubmission.tla
run_negative_tlc TaxDocumentSubmissionUnguardedStart.cfg taxdocumentsubmissionunguardedstart 'Invariant AtMostOneDocument is violated' TaxDocumentSubmission.tla
run_negative_tlc TaxDocumentSubmissionLostAsRejected.cfg taxdocumentsubmissionlostasrejected 'Invariant AtMostOneDocument is violated' TaxDocumentSubmission.tla
run_negative_tlc TaxDocumentSubmissionRetryUncertain.cfg taxdocumentsubmissionretryuncertain 'Invariant AtMostOneDocument is violated' TaxDocumentSubmission.tla
run_negative_tlc OperationsManualReceiptReuseSourceKey.cfg operationsmanualreusesourcekey 'Invariant SourceSeparation is violated' OperationsManualReceipt.tla
run_negative_tlc OperationsManualReceiptUnboundReplay.cfg operationsmanualunboundreplay 'Invariant BoundReplay is violated' OperationsManualReceipt.tla
run_negative_tlc OperationsManualReceiptPartialCommit.cfg operationsmanualpartialcommit 'Invariant AtomicCreation is violated' OperationsManualReceipt.tla
run_tlc OperationsApproval.tla OperationsApproval.cfg operationsapproval
run_negative_tlc OperationsApprovalUnboundReplay.cfg operationsapprovalunboundreplay 'Invariant BoundReplay is violated' OperationsApproval.tla
run_negative_tlc OperationsApprovalDuplicateAudit.cfg operationsapprovalduplicateaudit 'Invariant SingleEvidence is violated' OperationsApproval.tla
run_negative_tlc OperationsApprovalReopen.cfg operationsapprovalreopen 'Invariant DecisionGuard is violated' OperationsApproval.tla
run_negative_tlc OperationsApprovalIgnoreExpiry.cfg operationsapprovalignoreexpiry 'Invariant DecisionGuard is violated' OperationsApproval.tla
run_negative_tlc OperationsApprovalSelfApprove.cfg operationsapprovalselfapprove 'Invariant DecisionGuard is violated' OperationsApproval.tla
run_tlc AccountDeletionOperator.tla AccountDeletionOperator.cfg account-deletion-operator
run_tlc AccountDeletionAuthority.tla AccountDeletionAuthority.cfg account-deletion-authority
run_negative_tlc AccountDeletionAuthorityStale.cfg deletion-authority-stale 'Invariant CurrentAuthorityAtEffect is violated' AccountDeletionAuthority.tla
run_negative_tlc AccountDeletionAuthorityNoSessionLock.cfg deletion-authority-session 'Invariant CurrentAuthorityAtEffect is violated' AccountDeletionAuthority.tla
run_negative_tlc AccountDeletionAuthorityNoGrantLock.cfg deletion-authority-grants 'Invariant CurrentAuthorityAtEffect is violated' AccountDeletionAuthority.tla
run_negative_tlc AccountDeletionOperatorRoleOnly.cfg deletion-operator-role-only 'Invariant OnlyAuthorizedPrivacyEffects is violated' AccountDeletionOperator.tla
run_negative_tlc AccountDeletionOperatorModuleOnly.cfg deletion-operator-module-only 'Invariant OnlyAuthorizedPrivacyEffects is violated' AccountDeletionOperator.tla
run_tlc AccountDeletionIntake.tla AccountDeletionIntake.cfg account-deletion-intake
run_tlc AccountDeletionResolution.tla AccountDeletionResolution.cfg account-deletion-resolution
run_negative_tlc AccountDeletionResolutionNoOwnerLock.cfg deletion-resolution-race 'Invariant ResolutionFirstGetsFreshReceipt is violated' AccountDeletionResolution.tla
run_negative_tlc AccountDeletionIntakeNoOwnerLock.cfg deletion-intake-race 'Invariant OnePendingOwnerReceipt is violated' AccountDeletionIntake.tla
run_negative_tlc AccountDeletionIntakeReplayNotification.cfg deletion-intake-replay 'Invariant OneNoticePerNewReceipt is violated' AccountDeletionIntake.tla
run_negative_tlc AccountDeletionIntakeGeneralAudience.cfg deletion-intake-audience 'Invariant OnlyPrivacyAudience is violated' AccountDeletionIntake.tla
run_negative_tlc AccountDeletionIntakeInvalidLegacy.cfg deletion-intake-legacy 'Invariant OnlyValidReceipt is violated' AccountDeletionIntake.tla
run_tlc PrivacyDeletionWorkflow.tla PrivacyDeletionWorkflow.cfg privacy-deletion
run_negative_tlc PrivacyDeletionEarlyClose.cfg privacy-deletion-earlyclose 'Invariant ClosureEvidence is violated' PrivacyDeletionWorkflow.tla
run_negative_tlc PrivacyDeletionDeadlineReset.cfg privacy-deletion-deadline 'Invariant FixedDeadline is violated' PrivacyDeletionWorkflow.tla
run_negative_tlc PrivacyDeletionStaleWrite.cfg privacy-deletion-stale 'Invariant NoStaleCommit is violated' PrivacyDeletionWorkflow.tla
run_tlc MetaRedelivery.tla MetaRedelivery.cfg meta-redelivery
run_negative_tlc MetaRedeliveryRepeatResetsReply.cfg meta-redelivery-repeat-resets-reply 'Invariant ConsideredOnce is violated' MetaRedelivery.tla
run_negative_tlc MetaRedeliveryRepeatRewritesText.cfg meta-redelivery-repeat-rewrites-text 'Invariant TextKept is violated' MetaRedelivery.tla
run_negative_tlc MetaRedeliveryRepeatMovesDeletion.cfg meta-redelivery-repeat-moves-deletion 'Invariant DeletionTimeKept is violated' MetaRedelivery.tla
run_tlc SessionCacheIsolation.tla SessionCacheIsolation.cfg session-cache
run_negative_tlc SessionCacheShared.cfg session-cache-shared "Invariant PrivateProjection is violated" SessionCacheIsolation.tla
run_negative_tlc SessionCacheActorReuse.cfg session-cache-actor-reuse "Invariant PrivateProjection is violated" SessionCacheIsolation.tla
run_negative_tlc SessionCacheStaleExpiry.cfg session-cache-stale-expiry "Invariant CurrentExpiry is violated" SessionCacheIsolation.tla
run_tlc CatalogReorder.tla CatalogReorder.cfg catalog-reorder
run_negative_tlc CatalogReorderPartialRollback.cfg catalog-reorder-partialrollback 'Invariant NoRejectedEffect is violated' CatalogReorder.tla
run_negative_tlc CatalogReorderStaleRevision.cfg catalog-reorder-stalerevision 'Invariant NoStaleCommit is violated' CatalogReorder.tla
run_negative_tlc CatalogReorderNoAuthorization.cfg catalog-reorder-noauthorization 'Invariant NoUnauthorizedCommit is violated' CatalogReorder.tla
run_negative_tlc CatalogReorderNoAudit.cfg catalog-reorder-noaudit 'Invariant AtomicEvidence is violated' CatalogReorder.tla
run_tlc StudioBookingProjection.tla StudioBookingProjection.cfg studiobookingprojection
run_negative_tlc StudioBookingProjectionStale.cfg studiobookingprojection-stale 'Invariant ProjectionCurrent is violated' StudioBookingProjection.tla
run_negative_tlc StudioBookingProjectionReactivation.cfg studiobookingprojection-reactivation 'Invariant ProjectionCurrent is violated' StudioBookingProjection.tla
run_negative_tlc StudioBookingProjectionOverlap.cfg studiobookingprojection-overlap 'Invariant NoActiveOverlap is violated' StudioBookingProjection.tla
run_tlc StudioBookingScope.tla StudioBookingScope.cfg studiobookingscope
run_negative_tlc StudioBookingScopeModuleOnly.cfg studiobookingscope-moduleonly 'Invariant NoForeignAccess is violated' StudioBookingScope.tla
run_negative_tlc StudioBookingScopeRequestedAssignment.cfg studiobookingscope-requestedassignment 'Invariant NoForeignAccess is violated' StudioBookingScope.tla
run_negative_tlc StudioBookingScopeRevokedWrite.cfg studiobookingscope-revokedwrite 'Invariant NoRevokedCommit is violated' StudioBookingScope.tla
run_tlc RestoreIsolation.tla RestoreIsolation.cfg restore-isolation
run_negative_tlc RestoreIsolationNoLock.cfg restore-nolock 'Invariant ExclusiveOwners is violated' RestoreIsolation.tla
run_negative_tlc RestoreIsolationOrphanRetry.cfg restore-orphanretry 'Invariant AtMostOneIsolate is violated' RestoreIsolation.tla
run_negative_tlc RestoreIsolationSourceTarget.cfg restore-sourcetarget 'Invariant NoSourceMutation is violated' RestoreIsolation.tla
run_negative_tlc RestoreIsolationEarlyReceipt.cfg restore-earlyreceipt 'Invariant ReceiptSound is violated' RestoreIsolation.tla
run_negative_tlc RestoreIsolationLateCreate.cfg restore-late-create 'Invariant AtMostOneIsolate is violated' RestoreIsolation.tla
run_tlc RecoveryExpiry.tla RecoveryExpiry.cfg recovery-expiry
run_negative_tlc RecoveryExpiryNoExpiry.cfg recovery-no-expiry 'Invariant UnexpiredAtConsumption is violated' RecoveryExpiry.tla
run_negative_tlc RecoveryExpiryStaleClock.cfg recovery-stale-clock 'Invariant UnexpiredAtConsumption is violated' RecoveryExpiry.tla
run_negative_tlc RecoveryExpiryLegacy.cfg recovery-legacy 'Invariant LegacyFailsClosed is violated' RecoveryExpiry.tla
run_negative_tlc RecoveryExpiryRebound.cfg recovery-rebound 'Invariant BoundCredential is violated' RecoveryExpiry.tla
run_tlc DirectoryClaimReview.tla DirectoryClaimReview.cfg directory-claim-review
run_negative_tlc DirectoryClaimReviewConcurrent.cfg directory-claim-race 'Invariant GrantMatchesClaim is violated' DirectoryClaimReview.tla
run_negative_tlc DirectoryClaimReviewModuleOnly.cfg directory-claim-module-only 'Invariant AdminRoleRequired is violated' DirectoryClaimReview.tla
run_negative_tlc DirectoryClaimReviewSelfReview.cfg directory-claim-self-review 'Invariant SeparatedReview is violated' DirectoryClaimReview.tla
run_negative_tlc DirectoryClaimReviewReplay.cfg directory-claim-replay 'Invariant NoReplayRegrant is violated' DirectoryClaimReview.tla
run_tlc SignupIdentity.tla SignupIdentity.cfg signup-identity
run_negative_tlc SignupIdentityPublicClaim.cfg signup-public-claim 'Invariant IndependentPrincipal is violated' SignupIdentity.tla
run_negative_tlc SignupIdentityUnreviewed.cfg signup-unreviewed 'Invariant ReviewedManagement is violated' SignupIdentity.tla
run_negative_tlc SignupIdentityRebind.cfg signup-rebind 'Invariant IndependentPrincipal is violated' SignupIdentity.tla
run_tlc CredentialLifecycle.tla CredentialLifecycle.cfg credential-lifecycle
run_tlc ChatMutationBoundary.tla ChatMutationBoundary.cfg chat-mutation-boundary
run_tlc ChatNotificationAuthorization.tla ChatNotificationAuthorization.cfg chat-notification-authorization
run_negative_tlc ChatNotificationCachedDisplay.cfg chat-notification-cached-display 'Invariant NoCachedDisplay is violated' ChatNotificationAuthorization.tla
run_negative_tlc ChatNotificationEarlyRead.cfg chat-notification-early-read 'Invariant NoEarlyRead is violated' ChatNotificationAuthorization.tla
run_tlc ChatNotificationNavigation.tla ChatNotificationNavigation.cfg chat-notification-navigation
run_negative_tlc ChatNotificationNavigationCached.cfg chat-notification-navigation-cached 'Invariant NoStaleNavigationDisplay is violated' ChatNotificationNavigation.tla
run_negative_tlc ChatNotificationNavigationEarlyRead.cfg chat-notification-navigation-read 'Invariant NoStaleNavigationRead is violated' ChatNotificationNavigation.tla
run_negative_tlc ChatNotificationNavigationOldResponse.cfg chat-notification-navigation-old-response 'Invariant NoStaleNavigationDisplay is violated' ChatNotificationNavigation.tla
run_negative_tlc ChatMutationEarlyCommit.cfg chat-mutation-early-commit 'Invariant RejectedLeavesNoMutation is violated' ChatMutationBoundary.tla
run_negative_tlc ChatMutationNoRollback.cfg chat-mutation-no-rollback 'Invariant RejectedLeavesNoMutation is violated' ChatMutationBoundary.tla
run_negative_tlc CredentialLifecycleConcurrentReset.cfg credential-reset-race 'Invariant SingleUseReset is violated' CredentialLifecycle.tla
run_negative_tlc CredentialLifecycleEarlyCommit.cfg credential-early-commit 'Invariant AtomicChallengeConsumption is violated' CredentialLifecycle.tla
run_negative_tlc CredentialLifecycleGoogleSession.cfg credential-google-session 'Invariant NoSessionsAfterDisable is violated' CredentialLifecycle.tla
run_tlc WhatsAppConsent.tla WhatsAppConsent.cfg whatsapp-consent
run_negative_tlc WhatsAppConsentStaleReply.cfg whatsapp-consent-stale-reply 'Invariant ConfirmedByOwnLaterReply is violated' WhatsAppConsent.tla
run_negative_tlc WhatsAppConsentWithdrawnReply.cfg whatsapp-consent-withdrawn-reply 'Invariant WithdrawalWins is violated' WhatsAppConsent.tla
run_negative_tlc WhatsAppConsentStaleWithdrawalClock.cfg whatsapp-consent-stale-withdrawal-clock 'Invariant WithdrawalWins is violated' WhatsAppConsent.tla
run_negative_tlc WhatsAppConsentWithdrawalReset.cfg whatsapp-consent-withdrawal-reset 'Invariant DeliveredRequestsSpaced is violated' WhatsAppConsent.tla
run_negative_tlc WhatsAppConsentUnboundFailure.cfg whatsapp-consent-unbound-failure 'Invariant DeliveredRequestsSpaced is violated' WhatsAppConsent.tla
run_negative_tlc WhatsAppConsentReleasedFailure.cfg whatsapp-consent-released-failure 'Invariant UndeliveredStaysPending is violated' WhatsAppConsent.tla
run_negative_tlc WhatsAppConsentLongUnsentWindow.cfg whatsapp-consent-long-unsent-window 'Invariant ConfirmedInsideWindow is violated' WhatsAppConsent.tla
run_negative_tlc WhatsAppConsentRenewedUnsent.cfg whatsapp-consent-renewed-unsent 'Invariant ConfirmedInsideWindow is violated' WhatsAppConsent.tla
run_tlc PublicRouteAdmission.tla PublicRouteAdmission.cfg public-route-admission
run_negative_tlc PublicRouteAdmissionUnregistered.cfg public-route-admission-unregistered 'The invariant of ServedRoutesRegistered is equal to FALSE' PublicRouteAdmission.tla
run_negative_tlc PublicRouteAdmissionStale.cfg public-route-admission-stale 'The invariant of NoStaleEntries is equal to FALSE' PublicRouteAdmission.tla
run_negative_tlc PublicRouteAdmissionUnreviewedMutation.cfg public-route-admission-unreviewed-mutation 'The invariant of MutationReviewed is equal to FALSE' PublicRouteAdmission.tla
run_negative_tlc PublicRouteAdmissionDebtConformant.cfg public-route-admission-debt-conformant 'The invariant of DebtNeverConformant is equal to FALSE' PublicRouteAdmission.tla
run_tlc ReadinessProbe.tla ReadinessProbe.cfg readiness-probe
run_negative_tlc ReadinessProbeSingleAttempt.cfg readiness-probe-single-attempt 'Invariant ProbeReportsDatabase is violated' ReadinessProbe.tla
run_negative_tlc ReadinessProbePoolSizeAttempts.cfg readiness-probe-pool-size-attempts 'Invariant ProbeReportsDatabase is violated' ReadinessProbe.tla
run_tlc MarketplaceStorage.tla MarketplaceStorage.cfg marketplace-storage
run_tlc InvoiceSubmission.tla InvoiceSubmission.cfg invoice-submission
run_negative_tlc InvoiceSubmissionUnfencedStart.cfg invoice-submission-unfenced-start 'Invariant AtMostOneProviderDocument is violated' InvoiceSubmission.tla
run_negative_tlc InvoiceSubmissionStaleFinish.cfg invoice-submission-stale-finish 'Invariant AuthorizedIsFinal is violated' InvoiceSubmission.tla
run_negative_tlc InvoiceSubmissionUnknownAsRefused.cfg invoice-submission-unknown-as-refused 'Invariant AtMostOneProviderDocument is violated' InvoiceSubmission.tla
run_negative_tlc InvoiceSubmissionRetryUncertain.cfg invoice-submission-retry-uncertain 'Invariant AtMostOneProviderDocument is violated' InvoiceSubmission.tla
run_negative_tlc MarketplaceStorageUnsafeCache.cfg marketplace-storage-cache 'Invariant NoStorageExceptionEscapes is violated' MarketplaceStorage.tla
run_negative_tlc MarketplaceStorageUnsafeKey.cfg marketplace-storage-key 'Invariant NoDispatchWithoutDurableKey is violated' MarketplaceStorage.tla
run_tlc MusicDdexExport.tla MusicDdexExport.cfg music-ddex-export
run_negative_tlc MusicDdexExportUnfenced.cfg music-ddex-export-unfenced 'Invariant CurrentWriter is violated' MusicDdexExport.tla
run_negative_tlc MusicDdexExportReopen.cfg music-ddex-export-reopen 'Invariant ValidForever is violated' MusicDdexExport.tla
run_negative_tlc MusicDdexExportNoCommitGate.cfg music-ddex-export-no-commit-gate 'Invariant ValidOnlyUnderRules is violated' MusicDdexExport.tla
run_negative_tlc MusicDdexExportReplayFailed.cfg music-ddex-export-replay-failed 'Invariant FailedIsFinal is violated' MusicDdexExport.tla
run_negative_tlc MusicDdexExportSplitDeadLetter.cfg music-ddex-export-split-dead-letter 'Invariant PendingHasLiveJob is violated' MusicDdexExport.tla
run_tlc OptionalTokenRecovery.tla OptionalTokenRecovery.cfg optional-token-recovery
run_negative_tlc OptionalTokenRecoveryUnsafeStorage.cfg optional-token-storage 'Temporal properties were violated' OptionalTokenRecovery.tla
run_negative_tlc OptionalTokenRecoveryUnsafeFragment.cfg optional-token-fragment 'Invariant FragmentPrecedence is violated' OptionalTokenRecovery.tla
run_negative_tlc NativeLandingMarkerOnly.cfg native-landing-marker-only 'Invariant CurrentSessionSkipsMarker is violated' NativeLanding.tla
run_negative_tlc NativeArtistFollowNamespace.cfg native-artist-namespace 'Invariant SuccessfulFollowQualifies is violated' NativeArtistFollow.tla
run_negative_tlc NativeArtistFollowSession.cfg native-artist-session 'Invariant CurrentSession is violated' NativeArtistFollow.tla

run_tlc MarketplaceCatalogRead.tla MarketplaceCatalogRead.cfg marketplace-catalog-read
run_tlc MarketplaceCatalogRead.tla MarketplaceCatalogReadUnapproved.cfg marketplace-catalog-unapproved
run_negative_tlc MarketplaceCatalogReadUnsafe.cfg marketplace-catalog-unsafe 'Invariant SelectedRentalKeepsApprovedTerms is violated' MarketplaceCatalogRead.tla
run_tlc CalendarConnection.tla CalendarConnection.cfg calendar-connection
run_negative_tlc CalendarConnectionReplay.cfg calendar-replay 'Invariant AtMostOneAutomaticExchange is violated' CalendarConnection.tla
run_negative_tlc CalendarConnectionStorage.cfg calendar-storage 'Invariant OnlyPersistedConnection is violated' CalendarConnection.tla
run_negative_tlc CalendarConnectionStale.cfg calendar-stale 'Invariant CurrentSessionReceipt is violated' CalendarConnection.tla
run_tlc DirectoryFavoriteAuthority.tla DirectoryFavoriteAuthority.cfg directory-favorite-authority
run_negative_tlc DirectoryFavoriteAuthorityUnsafeDispatch.cfg directory-favorite-dispatch 'Invariant AuthorizedDispatch is violated' DirectoryFavoriteAuthority.tla
run_negative_tlc DirectoryFavoriteAuthorityUnsafeReceipt.cfg directory-favorite-receipt 'Invariant CurrentSessionReceipt is violated' DirectoryFavoriteAuthority.tla
run_negative_tlc ExperimentAuthorityStale.cfg experiment-stale 'Invariant AccountAndEligibilityAuthority is violated' ExperimentAuthority.tla
run_negative_tlc ExperimentAuthorityDuplicate.cfg experiment-duplicate 'Invariant ExposureAtMostOnce is violated' ExperimentAuthority.tla
run_negative_tlc ExperimentAuthorityAccount.cfg experiment-account 'Invariant AccountAndEligibilityAuthority is violated' ExperimentAuthority.tla
run_negative_tlc EventLifecycleUnsafeFinance.cfg unsafe-finance 'Invariant AcceptedAuditIsAuthorized is violated'
run_negative_tlc EventLifecycleUnsafeArchive.cfg unsafe-archive 'Invariant AcceptedAuditIsAuthorized is violated'
run_negative_tlc EventLifecycleUnsafeAudit.cfg unsafe-audit 'Action property AuditAppendOnly is violated'
run_tlc ReservationRace.tla ReservationRace.cfg reservation-race
run_tlc ReservationRace.tla ReservationOverride.cfg reservation-override
run_tlc InvitationSafety.tla InvitationSafety.cfg invitation
run_tlc TaskRaci.tla TaskRaci.cfg task-raci
run_tlc TaskCommit.tla TaskCommit.cfg task-commit
# Mutation checks must expose the precise regression, not just fail parsing.
expect_counterexample() {
  local config="$1" invariant="$2" slug="$3" module="${4:-TaskCommit.tla}" result=0
  run_tlc "${module}" "${config}" "${slug}" > "${run_root}/${slug}.log" 2>&1 || result=$?
  if [[ "${result}" != 12 ]] || ! grep -q "Invariant ${invariant} is violated" "${run_root}/${slug}.log"; then
    cat "${run_root}/${slug}.log"
    echo "Expected counterexample not detected: ${config}" >&2
    exit 1
  fi
  echo "Expected mutation counterexample: ${config}: ${invariant}"
  printf 'TDF_TLC_RESULT negative %s %s\n' "${module}" "${config}"
}
run_tlc LedgerPosting.tla LedgerPosting.cfg ledger-posting
run_negative_tlc LedgerPostingReverserMutable.cfg ledger-posting-reverser-mutable 'Invariant ReversedByPosted is violated' LedgerPosting.tla
run_negative_tlc LedgerPostingAppend.cfg ledger-posting-append 'Invariant PostedImmutable is violated' LedgerPosting.tla
run_negative_tlc LedgerPostingUnvalidated.cfg ledger-posting-unvalidated 'Invariant PostedBalanced is violated' LedgerPosting.tla
run_negative_tlc LedgerPostingReopen.cfg ledger-posting-reopen 'Invariant PostedHistory is violated' LedgerPosting.tla
run_negative_tlc LedgerPostingInsertPosted.cfg ledger-posting-insert-posted 'Invariant PostedBalanced is violated' LedgerPosting.tla
run_tlc MusicPublication.tla MusicPublication.cfg music-publication
run_negative_tlc MusicPublicationBatch.cfg music-publication-batch 'Temporal properties were violated' MusicPublication.tla
run_negative_tlc MusicPublicationReplacedOnly.cfg music-publication-replaced-only 'Invariant OnlyInvalidFails is violated' MusicPublication.tla
run_negative_tlc MusicPublicationStayWithdrawn.cfg music-publication-stay-withdrawn 'Temporal properties were violated' MusicPublication.tla
run_negative_tlc MusicPublicationPointerStale.cfg music-publication-pointer-stale 'Invariant PointerLive is violated' MusicPublication.tla
run_tlc MusicWithdrawalFairness.tla MusicWithdrawalFairness.cfg music-withdrawal-fairness
run_negative_tlc MusicWithdrawalStarvation.cfg music-withdrawal-starvation 'Temporal properties were violated' MusicWithdrawalFairness.tla
run_tlc MusicAssetProcessing.tla MusicAssetProcessing.cfg music-asset-processing
run_negative_tlc MusicAssetProcessingUnfenced.cfg music-asset-processing-unfenced 'Invariant CompletionIsCurrent is violated' MusicAssetProcessing.tla
run_negative_tlc MusicAssetProcessingPreempt.cfg music-asset-processing-preempt 'Invariant LiveLeaseIsCurrent is violated' MusicAssetProcessing.tla
run_negative_tlc MusicAssetProcessingUnverified.cfg music-asset-processing-unverified 'Invariant PromotedOnlyVerified is violated' MusicAssetProcessing.tla
run_negative_tlc MusicAssetProcessingOverLimit.cfg music-asset-processing-overlimit 'Invariant NoOverLimitRun is violated' MusicAssetProcessing.tla
run_tlc PaymentRecovery.tla PaymentRecovery.cfg payment-recovery
expect_counterexample PaymentRecoveryOverwrite.cfg NoLostRecovery payment-recovery-overwrite PaymentRecovery.tla
expect_counterexample PaymentRecoveryReturn.cfg CorrectReturn payment-recovery-return PaymentRecovery.tla
expect_counterexample PaymentRecoveryCompleted.cfg NoCompletedFlowHijack payment-recovery-completed PaymentRecovery.tla
run_tlc WorkerCompletionEvidence.tla WorkerCompletionEvidence.cfg worker-completion
expect_counterexample WorkerCompletionEvidenceFailed.cfg CompletedHasNoFailedItems worker-completion-failed WorkerCompletionEvidence.tla
expect_counterexample WorkerCompletionEvidenceUnacknowledged.cfg SuccessfulReturnHasEvidence worker-completion-unacknowledged WorkerCompletionEvidence.tla
run_tlc ProviderRetryAdmission.tla ProviderRetryAdmission.cfg provider-retry-admission
run_tlc TicketCaptureClassification.tla TicketCaptureClassification.cfg ticket-capture-classification
expect_counterexample TicketCaptureClassificationUnsafeReadback.cfg UnresolvedCaptureFenced ticket-capture-readback TicketCaptureClassification.tla
expect_counterexample TicketCaptureClassificationUnsafeResponse.cfg UnresolvedCaptureFenced ticket-capture-response TicketCaptureClassification.tla
expect_counterexample TicketCaptureClassificationBindingMismatch.cfg UnresolvedCaptureFenced ticket-capture-binding TicketCaptureClassification.tla
run_tlc TicketCaptureFence.tla TicketCaptureFence.cfg ticket-capture-fence
expect_counterexample TicketCaptureFenceStaleRead.cfg CommittedUncertaintyFencesAdmission ticket-capture-stale-read TicketCaptureFence.tla
expect_counterexample TicketCaptureFenceUnlockedWriter.cfg CommittedUncertaintyFencesAdmission ticket-capture-unlocked-writer TicketCaptureFence.tla
run_tlc PaymentIntentExpiry.tla PaymentIntentExpiry.cfg payment-intent-expiry
expect_counterexample PaymentIntentExpiryRequest.cfg ExpiredAdmissionDenied payment-intent-expiry-request PaymentIntentExpiry.tla
expect_counterexample PaymentIntentExpiryTransaction.cfg ExpiredAdmissionDenied payment-intent-expiry-transaction PaymentIntentExpiry.tla
expect_counterexample PaymentIntentExpiryBeforeLock.cfg ExpiredAdmissionDenied payment-intent-expiry-before-lock PaymentIntentExpiry.tla
expect_counterexample PaymentIntentExpiryInclusive.cfg ExpiredAdmissionDenied payment-intent-expiry-inclusive PaymentIntentExpiry.tla
run_tlc DiscoveryFallbackPrivacy.tla DiscoveryFallbackPrivacy.cfg discovery-fallback-privacy
expect_counterexample DiscoveryFallbackPrivacyPrivateSubject.cfg FallbackRequiresNoAuthority discovery-private-subject DiscoveryFallbackPrivacy.tla
expect_counterexample DiscoveryFallbackPrivacyIgnoredLink.cfg FallbackRequiresNoAuthority discovery-ignored-link DiscoveryFallbackPrivacy.tla
expect_counterexample ProviderRetryAdmissionAmbiguous.cfg OneLiveIntent provider-retry-ambiguous ProviderRetryAdmission.tla
expect_counterexample ProviderRetryAdmissionDuplicate.cfg ImmutableKey provider-retry-duplicate ProviderRetryAdmission.tla
expect_counterexample ProviderRetryAdmissionPayload.cfg BoundReplay provider-retry-payload ProviderRetryAdmission.tla
expect_counterexample ProviderRetryAdmissionClosed.cfg ClosedCreationDenied provider-retry-closed ProviderRetryAdmission.tla
expect_counterexample ProviderRetryAdmissionLate.cfg CapturedEvidenceRetained provider-retry-late ProviderRetryAdmission.tla
run_tlc HostedServicePayment.tla HostedServicePayment.cfg hosted-service-payment
expect_counterexample HostedServicePaymentAtomic.cfg NoMissingFulfillment hosted-service-atomic HostedServicePayment.tla
expect_counterexample HostedServicePaymentReplay.cfg ExactlyOnePaidAudit hosted-service-replay HostedServicePayment.tla
expect_counterexample HostedServicePaymentProgress.cfg NoFulfillmentRegression hosted-service-progress HostedServicePayment.tla
run_tlc RefundRecovery.tla RefundRecovery.cfg refund-recovery
expect_counterexample RefundRecoveryExecution.cfg NoDuplicateExecution refund-recovery-execution RefundRecovery.tla
expect_counterexample RefundRecoveryAccounting.cfg NoDuplicateAccounting refund-recovery-accounting RefundRecovery.tla
expect_counterexample RefundRecoveryAuthority.cfg CurrentAuthorityAtApply refund-recovery-authority RefundRecovery.tla
expect_counterexample TaskCommitEarlyValidation.cfg NoBlockedCompletion early-validation
expect_counterexample TaskCommitWriteSkew.cfg NoOrphanResponsibilities write-skew
run_tlc TaskCompletion.tla TaskCompletion.cfg task-completion
expect_counterexample TaskCompletionAuthority.cfg CurrentAuthority task-completion-authority TaskCompletion.tla
expect_counterexample TaskCompletionVersion.cfg NoStaleCompletion task-completion-version TaskCompletion.tla
expect_counterexample TaskCompletionDependencies.cfg NoBlockedCompletion task-completion-dependencies TaskCompletion.tla
expect_counterexample TaskCompletionRaci.cfg CurrentAccountability task-completion-raci TaskCompletion.tla
expect_counterexample TaskCompletionLifecycle.cfg ValidLifecycle task-completion-lifecycle TaskCompletion.tla
expect_counterexample TaskCompletionReplay.cfg ExactRetry task-completion-replay TaskCompletion.tla
expect_counterexample TaskCompletionAudit.cfg AuditCoupled task-completion-audit TaskCompletion.tla
run_tlc TaskRevision.tla TaskRevision.cfg task-revision
expect_counterexample TaskRevisionRaci.cfg NoStaleCommit task-revision-raci TaskRevision.tla
expect_counterexample TaskRevisionEarly.cfg NoStaleCommit task-revision-early TaskRevision.tla
run_tlc TaskRevisionRead.tla TaskRevisionRead.cfg task-revision-read
expect_counterexample TaskRevisionReadMixed.cfg CoherentRevisionRead task-revision-read-mixed TaskRevisionRead.tla
expect_counterexample TaskRevisionReadEarly.cfg NoExpiredDisclosure task-revision-read-early TaskRevisionRead.tla
run_tlc RaciReassignment.tla RaciReassignment.cfg raci-reassignment
run_tlc CommandBoundary.tla CommandBoundary.cfg command-boundary
run_tlc TaskCompletionClient.tla TaskCompletionClient.cfg task-completion-client
expect_counterexample TaskCompletionClientCapture.cfg OriginalRequestSent completion-client-capture TaskCompletionClient.tla
expect_counterexample TaskCompletionClientShape.cfg ValidatedReceipt completion-client-shape TaskCompletionClient.tla
expect_counterexample TaskCompletionClientBinding.cfg ValidatedReceipt completion-client-binding TaskCompletionClient.tla
expect_counterexample TaskCompletionClientRetry.cfg SingleDispatch completion-client-retry TaskCompletionClient.tla
run_tlc RaciEditorContext.tla RaciEditorContext.cfg raci-editor-context
run_tlc RaciWebEditor.tla RaciWebEditor.cfg raci-web-editor
expect_counterexample RaciWebEditorConsent.cfg ExplicitConfirmation raci-web-consent RaciWebEditor.tla
expect_counterexample RaciWebEditorContext.cfg CurrentEditor raci-web-context RaciWebEditor.tla
expect_counterexample RaciWebEditorFlight.cfg OneFlight raci-web-flight RaciWebEditor.tla
expect_counterexample RaciWebEditorRetry.cfg SameRetry raci-web-retry RaciWebEditor.tla
expect_counterexample RaciWebEditorReceipt.cfg ValidatedSuccess raci-web-receipt RaciWebEditor.tla
expect_counterexample RaciEditorContextEarly.cfg PrivateOptions raci-context-early RaciEditorContext.tla
expect_counterexample RaciEditorContextCandidate.cfg EligibleOptions raci-context-candidate RaciEditorContext.tla
expect_counterexample RaciEditorContextMixed.cfg CoherentContext raci-context-mixed RaciEditorContext.tla
expect_counterexample CommandBoundaryEarly.cfg ValidatedCommit command-boundary-early CommandBoundary.tla
expect_counterexample CommandBoundaryUnbound.cfg ValidatedCommit command-boundary-unbound CommandBoundary.tla
expect_counterexample RaciReassignmentEarly.cfg CurrentAuthority raci-reassignment-early RaciReassignment.tla
expect_counterexample RaciReassignmentVersion.cfg NoStaleReassignment raci-reassignment-version RaciReassignment.tla
expect_counterexample RaciReassignmentReplay.cfg ExactRetry raci-reassignment-replay RaciReassignment.tla
expect_counterexample RaciReassignmentScope.cfg TaskKeyIsolation raci-reassignment-scope RaciReassignment.tla
expect_counterexample RaciReassignmentSplit.cfg NoOrphanResponsibilities raci-reassignment-split RaciReassignment.tla
expect_counterexample RaciReassignmentAudit.cfg AuditCoupled raci-reassignment-audit RaciReassignment.tla
run_tlc ReceiptReplay.tla ReceiptReplay.cfg receipt-replay
expect_counterexample ReceiptReplayBypass.cfg NoUnauthorizedDisclosure replay-bypass ReceiptReplay.tla
expect_counterexample ReceiptReplayStaleClock.cfg NoUnauthorizedDisclosure replay-clock ReceiptReplay.tla
expect_counterexample ReceiptReplayStaleSnapshot.cfg NoUnauthorizedDisclosure replay-snapshot ReceiptReplay.tla
run_tlc PublicKnowledgeIndex.tla PublicKnowledgeIndex.cfg public-index
expect_counterexample PublicKnowledgeIndexLegacy.cfg NoLegacyAfterCleanup public-index-legacy PublicKnowledgeIndex.tla
expect_counterexample PublicKnowledgeIndexPrivate.cfg NoPrivateEmbedding public-index-private PublicKnowledgeIndex.tla
expect_counterexample PublicKnowledgeIndexEmpty.cfg SuccessfulReplacement public-index-empty PublicKnowledgeIndex.tla
expect_counterexample PublicKnowledgeIndexFailure.cfg FailedRefreshPreservesIndex public-index-failure PublicKnowledgeIndex.tla
expect_counterexample PublicKnowledgeIndexCollision.cfg NoAmbiguousEmbedding public-index-collision PublicKnowledgeIndex.tla
run_tlc PublicKnowledgeRead.tla PublicKnowledgeRead.cfg public-knowledge
expect_counterexample PublicKnowledgeReadUnfiltered.cfg AllowedSource public-knowledge-source PublicKnowledgeRead.tla
expect_counterexample PublicKnowledgeReadCachedMetadata.cfg CurrentMetadata public-knowledge-stale PublicKnowledgeRead.tla
expect_counterexample PublicKnowledgeReadMissingFallback.cfg ExistingAtRead public-knowledge-missing PublicKnowledgeRead.tla
expect_counterexample PublicKnowledgeReadRawText.cfg NoPrivateText public-knowledge-private PublicKnowledgeRead.tla
expect_counterexample PublicKnowledgeReadCollision.cfg NoAmbiguousRead public-knowledge-collision PublicKnowledgeRead.tla
expect_counterexample PublicKnowledgeReadMixedSnapshot.cfg NoAmbiguousRead public-knowledge-mixed-snapshot PublicKnowledgeRead.tla
run_tlc SnapshotRead.tla SnapshotRead.cfg snapshot-read
expect_counterexample SnapshotReadEarlyAuth.cfg NoUnauthorizedSnapshot snapshot-auth SnapshotRead.tla
expect_counterexample SnapshotReadMixedClock.cfg CoherentProjection snapshot-clock SnapshotRead.tla
expect_counterexample SnapshotReadRawLog.cfg LogFieldsAllowlisted snapshot-log SnapshotRead.tla
run_tlc CommandPrivacy.tla CommandPrivacy.cfg command-privacy
expect_counterexample CommandPrivacyExistenceLeak.cfg OpaqueTarget command-existence CommandPrivacy.tla
expect_counterexample CommandPrivacyReceiptLeak.cfg OpaqueTarget command-receipt CommandPrivacy.tla
run_tlc SessionFence.tla SessionFence.cfg session-fence
for mutation in Stale Unlocked Party Credential Purpose Witness; do
  expect_counterexample "SessionFence${mutation}.cfg" CurrentBoundSession "session-${mutation}" SessionFence.tla
done
run_tlc ContractPayment.tla ContractPayment.cfg contract-payment
run_tlc CheckoutReadiness.tla CheckoutReadiness.cfg checkout-readiness
run_tlc CheckoutCancellation.tla CheckoutCancellation.cfg checkout-cancellation
run_negative_tlc CheckoutCancellationUnsafe.cfg checkout-cancellation-unsafe 'Invariant NoAbandonedReservation is violated' CheckoutCancellation.tla
run_negative_tlc CheckoutCancellationPaymentUnsafe.cfg checkout-payment-unsafe 'Invariant NoLostPayment is violated' CheckoutCancellation.tla
run_negative_tlc CheckoutReadinessUnavailable.cfg checkout-unavailable 'Invariant ReadyBeforeReservation is violated' CheckoutReadiness.tla
run_negative_tlc CheckoutReadinessStale.cfg checkout-stale 'Invariant CurrentReservation is violated' CheckoutReadiness.tla
run_negative_tlc CheckoutReadinessDuplicate.cfg checkout-duplicate 'Invariant SingleCurrentFlight is violated' CheckoutReadiness.tla
run_tlc CheckoutSnapshot.tla CheckoutSnapshot.cfg checkout-snapshot
run_negative_tlc CheckoutSnapshotLateLines.cfg checkout-snapshot-late-lines 'Invariant SnapshotFixed is violated' CheckoutSnapshot.tla
run_negative_tlc CheckoutSnapshotUnvalidated.cfg checkout-snapshot-unvalidated 'Invariant CommittedSum is violated' CheckoutSnapshot.tla
run_negative_tlc CheckoutSnapshotMutableTerms.cfg checkout-snapshot-mutable-terms 'Invariant TermsFixed is violated' CheckoutSnapshot.tla
run_tlc TermsIdentityUpgrade.tla TermsIdentityUpgrade.cfg terms-identity-upgrade
run_tlc TermsIdentityHistory.tla TermsIdentityHistory.cfg terms-identity-history
expect_counterexample TermsIdentityHistoryLiveOnly.cfg HistoricalIdentityPreserved terms-identity-history-live-only TermsIdentityHistory.tla
expect_counterexample TermsIdentityHistoryDelete.cfg AppendOnlyHistory terms-identity-history-delete TermsIdentityHistory.tla
expect_counterexample TermsIdentityHistoryUpdate.cfg AppendOnlyHistory terms-identity-history-update TermsIdentityHistory.tla
expect_counterexample TermsIdentityHistoryTruncate.cfg AppendOnlyHistory terms-identity-history-truncate TermsIdentityHistory.tla
run_negative_tlc TermsIdentityUpgradeUnfenced.cfg terms-identity-upgrade-unfenced 'Invariant InstalledHistoryValid is violated' TermsIdentityUpgrade.tla
run_negative_tlc TermsIdentityUpgradeEarlyUnlock.cfg terms-identity-upgrade-early-unlock 'Invariant InstalledHistoryValid is violated' TermsIdentityUpgrade.tla
run_negative_tlc TermsIdentityUpgradeUnchecked.cfg terms-identity-upgrade-unchecked 'Invariant InstalledHistoryValid is violated' TermsIdentityUpgrade.tla

run_tlc DriveReplay.tla DriveReplay.cfg drive-replay
run_tlc DriveReplay.tla DriveReplayCrossFolder.cfg drive-replay-cross-folder
run_negative_tlc DriveReplayCrossFolderObjects.cfg drive-replay-cross-folder-objects 'Invariant SingleObject is violated' DriveReplay.tla
run_negative_tlc DriveReplayUnchecked.cfg drive-replay-unchecked 'Invariant ReuseBinds is violated' DriveReplay.tla
run_negative_tlc DriveReplayActorIgnored.cfg drive-replay-actor-ignored 'Invariant NoForeignReuse is violated' DriveReplay.tla
run_negative_tlc DriveReplayLegacyAdopted.cfg drive-replay-legacy-adopted 'Invariant NoLegacyAdoption is violated' DriveReplay.tla
# Documented non-guarantee of lookup-then-create (MEDIA-DRIVE-001): expected counterexample, not a control.
run_negative_tlc DriveReplayRace.cfg drive-replay-race 'Invariant SingleObject is violated' DriveReplay.tla
run_tlc OperationsAuxiliaryWrite.tla OperationsAuxiliaryWrite.cfg operations-auxiliary-write
run_negative_tlc OperationsAuxiliaryWriteUnlocked.cfg operations-auxiliary-write-unlocked 'Invariant CurrentAuthority is violated' OperationsAuxiliaryWrite.tla
run_negative_tlc OperationsAuxiliaryWriteResolvedRetry.cfg operations-auxiliary-write-resolved-retry 'Invariant CurrentAuthority is violated' OperationsAuxiliaryWrite.tla
run_negative_tlc OperationsAuxiliaryWritePartialAudit.cfg operations-auxiliary-write-partial-audit 'Invariant AtomicEvidence is violated' OperationsAuxiliaryWrite.tla
run_negative_tlc OperationsAuxiliaryWriteRetryTwice.cfg operations-auxiliary-write-retry-twice 'Invariant OneRetry is violated' OperationsAuxiliaryWrite.tla
run_negative_tlc OperationsAuxiliaryWriteUnencrypted.cfg operations-auxiliary-write-unencrypted 'Invariant EncryptedPush is violated' OperationsAuxiliaryWrite.tla
run_tlc NavigationVisit.tla NavigationVisit.cfg navigation-visit
run_negative_tlc NavigationVisitUnsafe.cfg navigation-visit-unsafe 'Invariant NoFailedVisits is violated' NavigationVisit.tla
run_tlc ProviderRollback.tla ProviderRollback.cfg provider-rollback
run_tlc ProviderRollback.tla ProviderRollbackCompatible.cfg provider-rollback-compatible
run_tlc ProviderRollback.tla ProviderRollbackMixed.cfg provider-rollback-mixed
run_tlc ProviderRollback.tla ProviderRollbackForward.cfg provider-rollback-forward
run_negative_tlc ProviderRollbackPartial.cfg provider-rollback-partial 'Invariant StoppedFleetSafe is violated' ProviderRollback.tla
run_negative_tlc ProviderRollbackUnsafe.cfg provider-rollback-unsafe 'Invariant NoUnsafeRestoration is violated' ProviderRollback.tla
run_tlc OperationalLiveness.tla OperationalLiveness.cfg operational-liveness
run_tlc FanHubOnboarding.tla FanHubOnboarding.cfg fanhub-onboarding
run_tlc FanHubOnboarding.tla FanHubOnboardingLiveness.cfg fanhub-liveness
run_negative_tlc FanHubOnboardingUnfair.cfg fanhub-unfair 'Temporal properties were violated' FanHubOnboarding.tla
run_negative_tlc FanHubOnboardingConsent.cfg fanhub-consent 'Invariant ConsentOnly is violated' FanHubOnboarding.tla
run_negative_tlc FanHubOnboardingContext.cfg fanhub-context 'Invariant CurrentContext is violated' FanHubOnboarding.tla
run_negative_tlc FanHubOnboardingFlight.cfg fanhub-flight 'Invariant SingleFlight is violated' FanHubOnboarding.tla
run_negative_tlc FanHubOnboardingTerminal.cfg fanhub-terminal 'Invariant TerminalOnly is violated' FanHubOnboarding.tla

run_tlc AccessCodeValidation.tla AccessCodeValidation.cfg access-code-validation
run_negative_tlc AccessCodeValidationStale.cfg access-code-stale 'Invariant CurrentCredential is violated' AccessCodeValidation.tla
run_negative_tlc AccessCodeValidationSuperficial.cfg access-code-superficial 'Invariant VerifiedAccount is violated' AccessCodeValidation.tla
run_tlc InboundCapture.tla InboundCapture.cfg inbound-capture
run_negative_tlc InboundCaptureDisabled.cfg inbound-capture-disabled 'Invariant CaptureRespectsEnablement is violated' InboundCapture.tla
run_negative_tlc InboundCaptureApplicationDirectionRefused.cfg inbound-capture-application-direction-refused 'Invariant RequestsCaptured is violated' InboundCapture.tla
run_negative_tlc InboundCaptureEveryIncomingRow.cfg inbound-capture-every-incoming-row 'Invariant OnlyRequestsCaptured is violated' InboundCapture.tla
run_negative_tlc InboundCaptureEveryDelivery.cfg inbound-capture-every-delivery 'Invariant CapturedOnce is violated' InboundCapture.tla
run_tlc LiveIntakeAuthority.tla LiveIntakeAuthority.cfg live-intake-authority
run_negative_tlc LiveIntakeAmbient.cfg live-intake-ambient 'Invariant ExplicitAuthority is violated' LiveIntakeAuthority.tla
run_negative_tlc LiveIntakeStale.cfg live-intake-stale 'Invariant CurrentReceipt is violated' LiveIntakeAuthority.tla
run_negative_tlc LiveIntakeUnpersisted.cfg live-intake-unpersisted 'Invariant PersistedReceipt is violated' LiveIntakeAuthority.tla
run_tlc ArtistActivation.tla ArtistActivation.cfg artist-activation
run_negative_tlc ArtistActivationUnsafe.cfg artist-activation-unsafe 'Invariant CurrentContext is violated' ArtistActivation.tla
run_tlc MusicEntitlement.tla MusicEntitlement.cfg music-entitlement
run_tlc MusicPurchaseReplay.tla MusicPurchaseReplay.cfg music-purchase-replay
run_negative_tlc MusicPurchaseReplayUnbound.cfg music-purchase-replay-unbound 'Invariant ReplayPayloadMatches is violated' MusicPurchaseReplay.tla
run_negative_tlc MusicPurchaseReplayGlobalKey.cfg music-purchase-replay-global-key 'Invariant ReplayOwnerMatches is violated' MusicPurchaseReplay.tla
run_negative_tlc MusicPurchaseReplayStaleLookup.cfg music-purchase-replay-stale-lookup 'Invariant OneOrderPerBuyerKey is violated' MusicPurchaseReplay.tla
run_negative_tlc MusicEntitlementForgedPaid.cfg music-entitlement-forged-paid 'Invariant PaidNeedsEvidence is violated' MusicEntitlement.tla
run_negative_tlc MusicEntitlementRepeatedGrant.cfg music-entitlement-repeated-grant 'Invariant OneEntitlement is violated' MusicEntitlement.tla
run_negative_tlc MusicEntitlementNoLimit.cfg music-entitlement-no-limit 'Invariant LimitHolds is violated' MusicEntitlement.tla
run_negative_tlc MusicEntitlementEndlessReplay.cfg music-entitlement-endless-replay 'Invariant UrlsInsideLifetime is violated' MusicEntitlement.tla
run_negative_tlc MusicEntitlementFreshReplayUrl.cfg music-entitlement-fresh-replay-url 'Invariant UrlsEndWithAuthorization is violated' MusicEntitlement.tla
run_negative_tlc MusicEntitlementRefundedDownload.cfg music-entitlement-refunded-download 'Invariant ActiveWhenIssued is violated' MusicEntitlement.tla
run_tlc MusicPaymentOutcome.tla MusicPaymentOutcome.cfg music-payment-outcome
run_negative_tlc MusicPaymentOutcomeFailure.cfg music-payment-outcome-failure 'Invariant FailureKeepsPayable is violated' MusicPaymentOutcome.tla
run_negative_tlc MusicPaymentOutcomeClosed.cfg music-payment-outcome-closed 'Invariant ActiveOnlyForPaidOrder is violated' MusicPaymentOutcome.tla
run_negative_tlc MusicPaymentOutcomeReview.cfg music-payment-outcome-review 'Invariant ClosedMoneyNeedsReview is violated' MusicPaymentOutcome.tla
run_negative_tlc MusicPaymentOutcomeReplay.cfg music-payment-outcome-replay 'Invariant OneEntitlement is violated' MusicPaymentOutcome.tla
run_negative_tlc MusicPaymentOutcomeRetry.cfg music-payment-outcome-retry 'Invariant NoFailedRetry is violated' MusicPaymentOutcome.tla
run_negative_tlc MusicPaymentOutcomeFence.cfg music-payment-outcome-fence 'Invariant NoQuarantinedRetry is violated' MusicPaymentOutcome.tla
run_tlc ArtistClaimKind.tla ArtistClaimKind.cfg artist-claim-kind
run_negative_tlc ArtistClaimKindUnsafe.cfg artist-claim-kind-unsafe 'Invariant OnlyArtist is violated' ArtistClaimKind.tla
run_tlc ArtistClaimTarget.tla ArtistClaimTarget.cfg artist-claim-target
run_negative_tlc ArtistClaimTargetUnsafe.cfg artist-claim-target-unsafe 'Invariant UniqueTarget is violated' ArtistClaimTarget.tla
run_tlc OperationsFailureReplay.tla OperationsFailureReplay.cfg operations-failure-replay
run_negative_tlc OperationsFailureReplayRelabelOnly.cfg operations-failure-replay-relabel-only 'Invariant RetryingIsQueued is violated' OperationsFailureReplay.tla
run_negative_tlc OperationsFailureReplayUnsettled.cfg operations-failure-replay-unsettled 'Invariant RunIsSettled is violated' OperationsFailureReplay.tla
run_negative_tlc OperationsFailureReplayUnmerged.cfg operations-failure-replay-unmerged 'Invariant OneOpenFailure is violated' OperationsFailureReplay.tla
run_tlc TicketManualReview.tla TicketManualReview.cfg ticket-manual-review
run_tlc TicketManualReview.tla TicketManualReviewLockReleasedGuarded.cfg ticket-manual-review-lock-released-guarded
run_negative_tlc TicketManualReviewLockReleased.cfg ticket-manual-review-lock-released 'Invariant EveryoneAnswered is violated' TicketManualReview.tla
run_negative_tlc TicketManualReviewSubmitterReviews.cfg ticket-manual-review-submitter-reviews 'Invariant NoSelfApproval is violated' TicketManualReview.tla
run_negative_tlc TicketManualReviewLockReleasedUnguarded.cfg ticket-manual-review-lock-released-unguarded 'Invariant OnePayment is violated' TicketManualReview.tla
run_tlc IndexDeclaration.tla IndexDeclaration.cfg index-declaration
run_negative_tlc IndexDeclarationKeyUnique.cfg index-declaration-key-unique 'Invariant NothingNewRefused is violated' IndexDeclaration.tla
run_negative_tlc IndexDeclarationKeyTimeUnique.cfg index-declaration-key-time-unique 'Invariant NothingNewRefused is violated' IndexDeclaration.tla
run_negative_tlc IndexDeclarationIdentityNotUnique.cfg index-declaration-identity-not-unique 'Invariant IdentityStoredOnce is violated' IndexDeclaration.tla
run_tlc InteractionWriteBudget.tla InteractionWriteBudget.cfg interaction-write-budget
run_negative_tlc InteractionWriteBudgetErrorRefunds.cfg interaction-write-budget-error-refunds 'Invariant ChargeCounted is violated' InteractionWriteBudget.tla
run_negative_tlc InteractionWriteBudgetOneExtra.cfg interaction-write-budget-one-extra 'Invariant LimitHolds is violated' InteractionWriteBudget.tla
run_negative_tlc InteractionWriteBudgetUnlockedIncrement.cfg interaction-write-budget-unlocked-increment 'Invariant ChargeCounted is violated' InteractionWriteBudget.tla
run_negative_tlc InteractionWriteBudgetOneCounter.cfg interaction-write-budget-one-counter 'Invariant RefusedOnlyWhenSpent is violated' InteractionWriteBudget.tla
run_tlc EventOrganizer.tla EventOrganizer.cfg event-organizer
run_negative_tlc EventOrganizerAnyoneClaims.cfg event-organizer-anyone-claims 'Invariant OnlyAllowedAssigned is violated' EventOrganizer.tla
run_negative_tlc EventOrganizerUnconditionalWrite.cfg event-organizer-unconditional-write 'Invariant OrganizerStable is violated' EventOrganizer.tla
run_negative_tlc EventOrganizerTrustEarlierRead.cfg event-organizer-trust-earlier-read 'Invariant ManagersAreOrganizer is violated' EventOrganizer.tla
run_tlc TaskRead.tla TaskRead.cfg task-read
run_tlc TaskView.tla TaskView.cfg task-view
expect_counterexample TaskViewLate.cfg CurrentView task-view-late TaskView.tla
expect_counterexample TaskViewRetained.cfg CurrentView task-view-retained TaskView.tla
expect_counterexample TaskViewInvalid.cfg ValidatedView task-view-invalid TaskView.tla
for mutation in Scope Event Early; do
  expect_counterexample "TaskRead${mutation}.cfg" NoUnauthorizedTask "task-read-${mutation}" TaskRead.tla
done
expect_counterexample TaskReadMixed.cfg CoherentTaskProjection task-read-mixed TaskRead.tla
run_tlc WebOnboardingRecovery.tla WebOnboardingRecovery.cfg web-onboarding
expect_counterexample WebOnboardingRecoveryStale.cfg CurrentSessionOnly web-onboarding-stale WebOnboardingRecovery.tla
expect_counterexample WebOnboardingRecoveryReceipt.cfg AuthoritativeOnly web-onboarding-receipt WebOnboardingRecovery.tla
expect_counterexample WebOnboardingRecoveryOverlap.cfg SingleFlight web-onboarding-overlap WebOnboardingRecovery.tla
run_tlc ArtistFollowConsent.tla ArtistFollowConsent.cfg artist-follow
expect_counterexample ArtistFollowConsentClick.cfg NoUnconfirmedMutation artist-follow-click ArtistFollowConsent.tla
expect_counterexample ArtistFollowConsentUnknown.cfg NoUnconfirmedMutation artist-follow-unknown ArtistFollowConsent.tla
expect_counterexample ArtistFollowConsentStale.cfg CurrentTargetReceipt artist-follow-stale ArtistFollowConsent.tla


# Preserve original scopes and require a detected mutation for every Alloy assertion.
node --test "${SCRIPT_DIR}/__tests__/alloy-controls.test.mjs"
node "${SCRIPT_DIR}/verify-alloy-controls.mjs" --output "${TDF_ALLOY_EVIDENCE_DIR:-${run_root}/alloy}"

echo "Event operations formal verification passed within the documented finite bounds."
