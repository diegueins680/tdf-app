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
run_tlc ContractPayment.tla ContractPayment.cfg contract-payment
run_tlc OperationalLiveness.tla OperationalLiveness.cfg operational-liveness

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
