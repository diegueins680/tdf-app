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

# Keep TLC sequential: 1.7.2 materializes standard modules through a shared temp location.
run_tlc EventLifecycle.tla EventLifecycle.cfg event-lifecycle
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
expect_counterexample TaskCommitEarlyValidation.cfg NoBlockedCompletion early-validation
expect_counterexample TaskCommitWriteSkew.cfg NoOrphanResponsibilities write-skew
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
run_tlc TaskRead.tla TaskRead.cfg task-read
for mutation in Scope Event Early; do
  expect_counterexample "TaskRead${mutation}.cfg" NoUnauthorizedTask "task-read-${mutation}" TaskRead.tla
done
expect_counterexample TaskReadMixed.cfg CoherentTaskProjection task-read-mixed TaskRead.tla
run_tlc OperationalLiveness.tla OperationalLiveness.cfg operational-liveness
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

for command_index in 0 1 2 3; do
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
