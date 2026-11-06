#!/usr/bin/env bash
# Unbounded safety proofs by inductive invariants (Apalache) plus bounded TLC
# exploration and negative controls for formal/payments and formal/auth.
#
# Requires the pinned official artifacts:
#   APALACHE_TGZ   apalache.tgz from apalache-mc/apalache release v0.62.4
#   TLA2TOOLS_JAR  tla2tools.jar from tlaplus/tlaplus release v1.7.2
# and Java 17+ (JAVA_BIN optional).
#
# A proof obligation passes only when Apalache reports NoError. A negative
# control passes only when the expected invariant violation is reported. Any
# parser, type-checker or tool failure fails the run.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
MODEL_DIR="${REPO_DIR}/formal/payments"
AUTH_MODEL_DIR="${REPO_DIR}/formal/auth"
JAVA_BIN="${JAVA_BIN:-java}"
APALACHE_TGZ="${APALACHE_TGZ:-}"
TLA2TOOLS_JAR="${TLA2TOOLS_JAR:-}"

expected_apalache_sha="8ac8488422c30d552735bae11b92a2d014d8e02e8608702acc08f9a2d8a9abb2"
expected_tla_sha="fa18543e44ed5974a85bd2c60c0dc16620ae117680ea8e693d2691999ed90b22"

sha256() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  else
    shasum -a 256 "$1" | awk '{print $1}'
  fi
}

if [[ -z "${APALACHE_TGZ}" || ! -f "${APALACHE_TGZ}" || -z "${TLA2TOOLS_JAR}" || ! -f "${TLA2TOOLS_JAR}" ]]; then
  echo "APALACHE_TGZ and TLA2TOOLS_JAR must point to the pinned official artifacts." >&2
  exit 2
fi
if [[ "$(sha256 "${APALACHE_TGZ}")" != "${expected_apalache_sha}" ]]; then
  echo "Unexpected Apalache archive checksum." >&2
  exit 2
fi
if [[ "$(sha256 "${TLA2TOOLS_JAR}")" != "${expected_tla_sha}" ]]; then
  echo "Unexpected TLA+ tools checksum." >&2
  exit 2
fi

run_root="$(mktemp -d "${TMPDIR:-/tmp}/tdf-inductive-proofs.XXXXXX")"
cleanup() {
  case "${run_root}" in
    */tdf-inductive-proofs.*) rm -rf -- "${run_root}" ;;
    *) echo "Refusing to remove unexpected temporary path: ${run_root}" >&2 ;;
  esac
}
trap cleanup EXIT

mkdir -p "${run_root}/apalache"
tar -xzf "${APALACHE_TGZ}" -C "${run_root}/apalache" --strip-components=1
APALACHE="${run_root}/apalache/bin/apalache-mc"
export JAVA_HOME="${JAVA_HOME:-}"

# apalache_expect MODULE CINIT INIT INV LENGTH EXPECT(NoError|Error) LABEL
apalache_expect() {
  local module="$1" cinit="$2" init="$3" inv="$4" length="$5" expect="$6" label="$7"
  local log="${run_root}/apalache-${label}.log"
  local status=0
  (cd "${CURRENT_MODEL_DIR:-${MODEL_DIR}}" && "${APALACHE}" check \
      --out-dir="${run_root}/apalache-out-${label}" \
      --cinit="${cinit}" --init="${init}" --inv="${inv}" --length="${length}" \
      "${module}") >"${log}" 2>&1 || status=$?
  local outcome
  outcome="$(grep -o 'The outcome is: [A-Za-z]*' "${log}" | tail -1 | awk '{print $4}')"
  if [[ "${expect}" == "NoError" && "${status}" -eq 0 && "${outcome}" == "NoError" ]]; then
    echo "TDF_APALACHE_RESULT proof ${module} ${label}"
    return 0
  fi
  if [[ "${expect}" == "Error" && "${outcome}" == "Error" ]] && grep -Eq "state invariant [0-9]+ violated" "${log}"; then
    echo "TDF_APALACHE_RESULT negative ${module} ${label}"
    return 0
  fi
  cat "${log}" >&2
  echo "Unexpected Apalache result for ${module} ${label}: exit ${status}, outcome '${outcome}', expected ${expect}" >&2
  return 1
}

# tlc_expect MODULE CONFIG EXPECT(pass|violation)
tlc_expect() {
  local module="$1" config="$2" expect="$3"
  local log="${run_root}/tlc-${config}.log"
  local status=0
  (cd "${CURRENT_MODEL_DIR:-${MODEL_DIR}}" && "${JAVA_BIN}" "-Djava.io.tmpdir=${run_root}" -XX:+UseParallelGC \
      -jar "${TLA2TOOLS_JAR}" -workers 1 -metadir "${run_root}/tlc-${config}" \
      -config "${config}" "${module}") >"${log}" 2>&1 || status=$?
  if [[ "${expect}" == "pass" && "${status}" -eq 0 ]] && grep -q "No error has been found" "${log}"; then
    echo "TDF_TLC_RESULT positive ${module} ${config}"
    return 0
  fi
  if [[ "${expect}" == "violation" ]] && grep -q "Invariant .* is violated" "${log}"; then
    echo "TDF_TLC_RESULT negative ${module} ${config}"
    return 0
  fi
  cat "${log}" >&2
  echo "Unexpected TLC result for ${module} ${config}: exit ${status}, expected ${expect}" >&2
  return 1
}

M=StripeIntentOutcome.tla

# Bounded exploration and negative control.
tlc_expect "${M}" StripeIntentOutcome.cfg pass
tlc_expect "${M}" StripeIntentOutcomeLegacyAssumed.cfg pass
tlc_expect "${M}" StripeIntentOutcomeLegacyUnassumed.cfg violation

# Unbounded proofs: Init => IndInv, IndInv /\ Next => IndInv', IndInv => Safe,
# for every subset of up to six concurrent deliveries.
for cinit in CInitConditional CInitLegacyAssumed; do
  apalache_expect "${M}" "${cinit}" Init IndInv 0 NoError "${cinit}-initiation"
  apalache_expect "${M}" "${cinit}" IndInv IndInv 1 NoError "${cinit}-consecution"
  apalache_expect "${M}" "${cinit}" IndInv Safe 0 NoError "${cinit}-implication"
done

# Negative control: without conditional updates or the provider assumption,
# a reachable state violates Safe.
apalache_expect "${M}" CInitLegacyUnassumed Init Safe 4 Error "CInitLegacyUnassumed-reachable-violation"

C=CaptureAccounting.tla

# PAY-CAPTURE-RECON-001: captured money is always verified or reconciled.
tlc_expect "${C}" CaptureAccounting.cfg pass
tlc_expect "${C}" CaptureAccountingLegacy.cfg violation
apalache_expect "${C}" CInitReconcile Init IndInv 0 NoError "CaptureAccounting-initiation"
apalache_expect "${C}" CInitReconcile IndInv IndInv 1 NoError "CaptureAccounting-consecution"
apalache_expect "${C}" CInitReconcile IndInv Safe 0 NoError "CaptureAccounting-implication"
apalache_expect "${C}" CInitLegacy Init Safe 6 Error "CaptureAccountingLegacy-reachable-violation"

P=ProviderInbox.tla

# PAY-PROVIDER-INBOX-001: lease writes fenced by the claimed attempt number.
tlc_expect "${P}" ProviderInbox.cfg pass
tlc_expect "${P}" ProviderInboxLegacy.cfg violation
apalache_expect "${P}" CInitFence Init IndInv 0 NoError "ProviderInbox-initiation"
apalache_expect "${P}" CInitFence IndInv IndInv 1 NoError "ProviderInbox-consecution"
apalache_expect "${P}" CInitFence IndInv Safe 0 NoError "ProviderInbox-implication"
apalache_expect "${P}" CInitLegacy Init Safe 6 Error "ProviderInboxLegacy-reachable-violation"

I=SessionInvoiceIssue.tla

# PAY-INVOICE-ISSUE-001: the SRI script runs at most once per key, and two
# unresolved attempts are never open together without portal verification.
tlc_expect "${I}" SessionInvoiceIssue.cfg pass
tlc_expect "${I}" SessionInvoiceIssueNoLock.cfg violation
apalache_expect "${I}" CInitLock Init IndInv 0 NoError "SessionInvoiceIssue-initiation"
apalache_expect "${I}" CInitLock IndInv IndInv 1 NoError "SessionInvoiceIssue-consecution"
apalache_expect "${I}" CInitLock IndInv Safe 0 NoError "SessionInvoiceIssue-implication"
apalache_expect "${I}" CInitNoLock Init Safe 6 Error "SessionInvoiceIssueNoLock-reachable-violation"

K=ContractSend.tla

# CONTRACT-SEND-001: "sent" only after SMTP accepted the PDF, and every
# accepted message has a delivery row even if the request stops.
tlc_expect "${K}" ContractSend.cfg pass
tlc_expect "${K}" ContractSendUnconfigured.cfg pass
tlc_expect "${K}" ContractSendRecordAfter.cfg violation
apalache_expect "${K}" CInitRecordFirst Init IndInv 0 NoError "ContractSend-initiation"
apalache_expect "${K}" CInitRecordFirst IndInv IndInv 1 NoError "ContractSend-consecution"
apalache_expect "${K}" CInitRecordFirst IndInv Safe 0 NoError "ContractSend-implication"
apalache_expect "${K}" CInitRecordAfter Init Safe 4 Error "ContractSendRecordAfter-reachable-violation"

L=LogoutIntent.tla
CURRENT_MODEL_DIR="${AUTH_MODEL_DIR}"

# AUTH-LOGOUT-001: explicit logout suppresses restoration until fresh auth;
# stale callbacks never own a new login.
tlc_expect "${L}" LogoutIntent.cfg pass
tlc_expect "${L}" LogoutIntentLegacy.cfg violation
apalache_expect "${L}" CInitFence Init IndInv 0 NoError "LogoutIntent-initiation"
apalache_expect "${L}" CInitFence IndInv IndInv 1 NoError "LogoutIntent-consecution"
apalache_expect "${L}" CInitFence IndInv Safe 0 NoError "LogoutIntent-implication"
apalache_expect "${L}" CInitLegacy Init Safe 5 Error "LogoutIntentLegacy-reachable-violation"
unset CURRENT_MODEL_DIR

echo "Inductive proofs and controls passed."
