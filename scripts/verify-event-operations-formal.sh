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
run_negative_tlc EventLifecycleUnsafeFinance.cfg unsafe-finance 'Invariant AcceptedAuditIsAuthorized is violated'
run_negative_tlc EventLifecycleUnsafeArchive.cfg unsafe-archive 'Invariant AcceptedAuditIsAuthorized is violated'
run_negative_tlc EventLifecycleUnsafeAudit.cfg unsafe-audit 'Action property AuditAppendOnly is violated'
run_tlc ReservationRace.tla ReservationRace.cfg reservation-race
run_tlc ReservationRace.tla ReservationOverride.cfg reservation-override
run_tlc InvitationSafety.tla InvitationSafety.cfg invitation
run_tlc TaskRaci.tla TaskRaci.cfg task-raci
run_tlc ContractPayment.tla ContractPayment.cfg contract-payment
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

echo "Event operations formal verification passed within the documented finite bounds."
