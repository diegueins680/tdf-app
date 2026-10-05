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
  "${JAVA_BIN}" -XX:+UseParallelGC -jar "${TLA2TOOLS_JAR}" \
    -workers 1 \
    -metadir "${run_root}/tlc-${slug}" \
    -config "${config}" \
    "${module}"
}

cd "${MODEL_DIR}"

# Reused from PR #372: TLC checks generated TLA+, so first verify its source link.
export JAVA_BIN TLA2TOOLS_JAR
node "${SCRIPT_DIR}/verify-event-operations-pluscal.mjs"
node --test "${SCRIPT_DIR}/__tests__/event-operations-pluscal.test.mjs"

# Keep TLC sequential: 1.7.2 materializes standard modules through a shared temp location.
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
}
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
run_negative_tlc ChatMutationEarlyCommit.cfg chat-mutation-early-commit 'Invariant RejectedLeavesNoMutation is violated' ChatMutationBoundary.tla
run_negative_tlc ChatMutationNoRollback.cfg chat-mutation-no-rollback 'Invariant RejectedLeavesNoMutation is violated' ChatMutationBoundary.tla
run_negative_tlc CredentialLifecycleConcurrentReset.cfg credential-reset-race 'Invariant SingleUseReset is violated' CredentialLifecycle.tla
run_negative_tlc CredentialLifecycleEarlyCommit.cfg credential-early-commit 'Invariant AtomicChallengeConsumption is violated' CredentialLifecycle.tla
run_negative_tlc CredentialLifecycleGoogleSession.cfg credential-google-session 'Invariant NoSessionsAfterDisable is violated' CredentialLifecycle.tla
run_tlc MarketplaceStorage.tla MarketplaceStorage.cfg marketplace-storage
run_negative_tlc MarketplaceStorageUnsafeCache.cfg marketplace-storage-cache 'Invariant NoStorageExceptionEscapes is violated' MarketplaceStorage.tla
run_negative_tlc MarketplaceStorageUnsafeKey.cfg marketplace-storage-key 'Invariant NoDispatchWithoutDurableKey is violated' MarketplaceStorage.tla
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
}
run_tlc PaymentRecovery.tla PaymentRecovery.cfg payment-recovery
expect_counterexample PaymentRecoveryOverwrite.cfg NoLostRecovery payment-recovery-overwrite PaymentRecovery.tla
expect_counterexample PaymentRecoveryReturn.cfg CorrectReturn payment-recovery-return PaymentRecovery.tla
expect_counterexample PaymentRecoveryCompleted.cfg NoCompletedFlowHijack payment-recovery-completed PaymentRecovery.tla
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
run_tlc LiveIntakeAuthority.tla LiveIntakeAuthority.cfg live-intake-authority
run_negative_tlc LiveIntakeAmbient.cfg live-intake-ambient 'Invariant ExplicitAuthority is violated' LiveIntakeAuthority.tla
run_negative_tlc LiveIntakeStale.cfg live-intake-stale 'Invariant CurrentReceipt is violated' LiveIntakeAuthority.tla
run_negative_tlc LiveIntakeUnpersisted.cfg live-intake-unpersisted 'Invariant PersistedReceipt is violated' LiveIntakeAuthority.tla
run_tlc ArtistActivation.tla ArtistActivation.cfg artist-activation
run_negative_tlc ArtistActivationUnsafe.cfg artist-activation-unsafe 'Invariant CurrentContext is violated' ArtistActivation.tla
run_tlc ArtistClaimKind.tla ArtistClaimKind.cfg artist-claim-kind
run_negative_tlc ArtistClaimKindUnsafe.cfg artist-claim-kind-unsafe 'Invariant OnlyArtist is violated' ArtistClaimKind.tla
run_tlc ArtistClaimTarget.tla ArtistClaimTarget.cfg artist-claim-target
run_negative_tlc ArtistClaimTargetUnsafe.cfg artist-claim-target-unsafe 'Invariant UniqueTarget is violated' ArtistClaimTarget.tla
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


scenario_output="$("${JAVA_BIN}" -jar "${ALLOY_JAR}" exec \
  -c 0 -s sat4j -t none -o "${run_root}/alloy-scenario" EventStructure.als 2>&1)"
printf '%s\n' "${scenario_output}"
if grep -q 'UNSAT' <<<"${scenario_output}" || ! grep -q 'SAT' <<<"${scenario_output}"; then
  echo "Alloy integrated scenario must be satisfiable." >&2
  exit 1
fi

for command_index in 1 2 3 4 5 6 7 8; do
  check_output="$("${JAVA_BIN}" -jar "${ALLOY_JAR}" exec \
    -c "${command_index}" -s sat4j -t none \
    -o "${run_root}/alloy-check-${command_index}" EventStructure.als 2>&1)"
  printf '%s\n' "${check_output}"
  if ! grep -q 'UNSAT' <<<"${check_output}"; then
    echo "Alloy command ${command_index} found a counterexample or did not complete." >&2
    exit 1
  fi
done

for command_index in 0 1 2 3 4 5; do
  task_output="$("${JAVA_BIN}" -jar "${ALLOY_JAR}" exec \
    -c "${command_index}" -s sat4j -t none \
    -o "${run_root}/alloy-task-read-${command_index}" TaskReadStructure.als 2>&1)"
  printf '%s\n' "${task_output}"
  if [[ "${command_index}" = 0 ]]; then
    if grep -q 'UNSAT' <<<"${task_output}" || ! grep -q 'SAT' <<<"${task_output}"; then
      echo 'Alloy task read scenario must be satisfiable.' >&2
      exit 1
    fi
  elif ! grep -q 'UNSAT' <<<"${task_output}"; then
    echo "Alloy task read assertion ${command_index} failed." >&2
    exit 1
  fi
done

echo "Event operations formal verification passed within the documented finite bounds."
