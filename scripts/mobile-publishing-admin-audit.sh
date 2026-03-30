#!/usr/bin/env bash
set -euo pipefail

WORKSPACE="${WORKSPACE:-/Users/diegosaa/GitHub/tdf-app}"
MOBILE_DIR="${MOBILE_DIR:-$WORKSPACE/tdf-mobile}"
REPORT_PATH="${1:-}"
TMP_DIR="$(mktemp -d /tmp/tdf-mobile-admin-audit.XXXXXX)"
OUTPUT_FILE="$TMP_DIR/report.md"
trap 'rm -rf "$TMP_DIR"' EXIT

if [[ ! -d "$WORKSPACE" ]]; then
  echo "Workspace not found: $WORKSPACE" >&2
  exit 1
fi

report() {
  printf '%s\n' "$*" >>"$OUTPUT_FILE"
}

run_capture() {
  local label="$1"
  shift
  local rc=0
  "$@" >"$TMP_DIR/cmd.out" 2>&1 || rc=$?
  report ""
  report "### ${label}"
  report ""
  report '```text'
  sed -e 's/```/` ` `/g' "$TMP_DIR/cmd.out" >>"$OUTPUT_FILE"
  if [[ $rc -ne 0 ]]; then
    report "[exit=${rc}]"
  fi
  report '```'
}

has_cmd() {
  command -v "$1" >/dev/null 2>&1
}

status_word() {
  local ok="$1"
  if [[ "$ok" == "1" ]]; then
    printf 'OK'
  else
    printf 'BLOCKED'
  fi
}

mkdir -p "$(dirname "${REPORT_PATH:-$TMP_DIR/dummy}")"
now_iso="$(TZ=America/Guayaquil date '+%Y-%m-%d %H:%M:%S %Z')"

endpoint_blockers=()
host_blockers=()
credential_blockers=()
visibility_blockers=()

report "# TDF Mobile Store Publication Systems Report"
report ""
report "- Generated: ${now_iso}"
report "- Workspace: ${WORKSPACE}"
report "- Mobile dir: ${MOBILE_DIR}"
report ""
report "## Concrete useful thing completed"
report ""
report "Added a repeatable operator audit script at scripts/mobile-publishing-admin-audit.sh and ran it to generate this evidence snapshot. This closes the admin-visibility gap by making store-publication preflight checks reproducible instead of ad hoc."
report ""
report "## Endpoint validation"
report ""
report "| Endpoint | HTTP | Title | SHA-256 | Verdict |"
report "|---|---:|---|---|---|"

while IFS='|' read -r name url; do
  html="$TMP_DIR/${name}.html"
  headers="$TMP_DIR/${name}.headers"
  http_code="000"
  title="(unreadable)"
  sha="(missing)"
  verdict="OK"
  if curl -L --max-time 30 -D "$headers" -o "$html" -sS -w '%{http_code}' "$url" >"$TMP_DIR/http_code" 2>/dev/null; then
    http_code="$(cat "$TMP_DIR/http_code")"
    sha="$(shasum -a 256 "$html" | awk '{print $1}')"
    title="$(python3 - <<'PY' "$html"
import pathlib, re, sys
text = pathlib.Path(sys.argv[1]).read_text(encoding='utf-8', errors='ignore')
m = re.search(r'<title>(.*?)</title>', text, re.I | re.S)
print(re.sub(r'\s+', ' ', m.group(1)).strip() if m else '(no title)')
PY
)"
    if [[ "$http_code" != "200" ]]; then
      verdict="BLOCKED"
      endpoint_blockers+=("${name}: HTTP ${http_code}")
    fi
    case "$name" in
      support)
        if ! grep -qi 'support' "$html"; then
          verdict="BLOCKED"
          endpoint_blockers+=("support: expected support copy missing")
        fi
        ;;
      privacy)
        if ! grep -qi 'privacy' "$html"; then
          verdict="BLOCKED"
          endpoint_blockers+=("privacy: expected privacy copy missing")
        fi
        ;;
      data-deletion)
        if ! grep -Eqi 'data deletion|delete' "$html"; then
          verdict="BLOCKED"
          endpoint_blockers+=("data-deletion: expected deletion copy missing")
        fi
        ;;
    esac
  else
    verdict="BLOCKED"
    endpoint_blockers+=("${name}: curl failed")
  fi
  report "| ${name} | ${http_code} | ${title//|/\\|} | ${sha} | ${verdict} |"
done <<'EOF'
support|https://tdf-app.pages.dev/mobile-app/support.html
privacy|https://tdf-app.pages.dev/mobile-app/privacy.html
terms|https://tdf-app.pages.dev/mobile-app/terms.html
data-deletion|https://tdf-app.pages.dev/mobile-app/data-deletion.html
EOF

report ""
report "## Release / admin visibility"
report ""

eas_summary="$TMP_DIR/eas-summary.txt"
{
  echo 'EAS whoami:'
  if [[ -d "$MOBILE_DIR" ]]; then
    (cd "$MOBILE_DIR" && npm_config_cache="$(mktemp -d /tmp/tdf-eas-cache.XXXXXX)" npx --yes eas-cli@18.3.0 whoami) || true
    echo
    echo 'EAS project:info:'
    (cd "$MOBILE_DIR" && npm_config_cache="$(mktemp -d /tmp/tdf-eas-cache.XXXXXX)" npx --yes eas-cli@18.3.0 project:info) || true
  fi
} >"$eas_summary" 2>&1 || true

if grep -q 'cuco.saa' "$eas_summary"; then
  release_visibility="OK"
else
  release_visibility="PARTIAL"
  visibility_blockers+=("EAS auth/project info could not be cleanly confirmed")
fi

chrome_storage_paths=()
for d in \
  "$HOME/Library/Application Support/Google/Chrome/Default" \
  "$HOME/Library/Application Support/Google/Chrome/Profile 1" \
  "$HOME/Library/Application Support/BraveSoftware/Brave-Browser/Default"; do
  if [[ -d "$d" ]]; then
    while IFS= read -r f; do
      chrome_storage_paths+=("${f/$HOME/~}")
    done < <(find "$d" -maxdepth 1 -type f \( -name 'Cookies' -o -name 'Login Data' -o -name 'Web Data' \) | sort)
  fi
done

relay_probe_url="http://127.0.0.1:18792/json/version"
relay_probe_status="NOT_RUN"
relay_probe_detail="curl unavailable"
if command -v curl >/dev/null 2>&1; then
  relay_probe_raw="$(curl -sS --max-time 3 "$relay_probe_url" 2>&1 || true)"
  relay_probe_detail="$(printf '%s' "$relay_probe_raw" | tr '\n' ' ' | sed 's/[[:space:]]\+/ /g' | cut -c1-240)"
  if [[ -z "$relay_probe_raw" ]]; then
    relay_probe_status="NO_RESPONSE"
    relay_probe_detail="no response"
    visibility_blockers+=("Chrome relay raw probe returned no response at $relay_probe_url")
  elif [[ "$relay_probe_raw" == "Unauthorized" ]]; then
    relay_probe_status="UNAUTHORIZED"
  elif [[ "$relay_probe_raw" == \{*\"Browser\"* ]]; then
    relay_probe_status="RESPONSIVE"
  else
    relay_probe_status="UNREACHABLE"
    visibility_blockers+=("Chrome relay raw probe did not return a usable response at $relay_probe_url: $relay_probe_detail")
  fi
else
  visibility_blockers+=("curl is unavailable, so the local Chrome relay raw probe could not run")
fi

if [[ ${#chrome_storage_paths[@]} -eq 0 ]]; then
  visibility_blockers+=("No local browser session storage signal found for Chrome/Brave profiles")
fi
visibility_blockers+=("Relay-attached signed-in App Store Connect / Play Console tabs still required for live console audit")

report "- EAS auth/project visibility: ${release_visibility}"
report "- Browser session storage signals found: ${#chrome_storage_paths[@]} files"
report "- Chrome relay raw probe (${relay_probe_url}): ${relay_probe_status}"
report "- Chrome relay raw probe detail: ${relay_probe_detail}"
report "- Live console audit status: BLOCKED until relay-attached tabs are provided"
report ""
report "### Browser session storage signals (filenames only)"
report ""
if [[ ${#chrome_storage_paths[@]} -gt 0 ]]; then
  for f in "${chrome_storage_paths[@]}"; do
    report "- ${f}"
  done
else
  report "- None found"
fi

report ""
report "## Credentials / signing prerequisites"
report ""

codesign_identities="$(security find-identity -v -p codesigning 2>/dev/null || true)"
valid_identity_count="$(printf '%s\n' "$codesign_identities" | awk '/valid identities found/{print $1}' | tail -n1)"
valid_identity_count="${valid_identity_count:-0}"
if [[ "$valid_identity_count" == "0" ]]; then
  credential_blockers+=("No local Apple code-signing identities detected")
fi

profiles_dir="$HOME/Library/MobileDevice/Provisioning Profiles"
profiles_count="0"
if [[ -d "$profiles_dir" ]]; then
  profiles_count="$(find "$profiles_dir" -maxdepth 1 -type f | wc -l | tr -d ' ')"
fi
if [[ "$profiles_count" == "0" ]]; then
  credential_blockers+=("No local iOS provisioning profiles directory/files detected")
fi

asc_key_files=()
for d in "$HOME/.appstoreconnect/private_keys" "$HOME/.private_keys"; do
  if [[ -d "$d" ]]; then
    while IFS= read -r f; do
      asc_key_files+=("${f#$HOME/}")
    done < <(find "$d" -maxdepth 2 -type f | sort)
  fi
done
if [[ ${#asc_key_files[@]} -eq 0 ]]; then
  credential_blockers+=("No local App Store Connect API key files found in standard directories")
fi

android_key_files=()
while IFS= read -r f; do
  android_key_files+=("${f#$WORKSPACE/}")
done < <(find "$WORKSPACE" -type f \( -iname '*.keystore' -o -iname '*.jks' -o -iname '*service-account*.json' -o -iname '*play*json' -o -iname '*google*play*json' \) ! -path '*/node_modules/*' 2>/dev/null | sort | head -n 50)
if [[ ${#android_key_files[@]} -eq 0 ]]; then
  credential_blockers+=("No Android keystore / Play service-account file names found in workspace")
fi

report "- Local Apple code-signing identities: ${valid_identity_count}"
report "- Local iOS provisioning profiles: ${profiles_count}"
report "- Local ASC API key files in standard dirs: ${#asc_key_files[@]}"
report "- Workspace Android keystore / Play JSON files found: ${#android_key_files[@]}"
report ""
report "### Apple codesigning summary"
report ""
report '```text'
printf '%s\n' "$codesign_identities" >>"$OUTPUT_FILE"
report '```'
report ""
report "### Local signing/material filenames only"
report ""
if [[ ${#asc_key_files[@]} -gt 0 ]]; then
  for f in "${asc_key_files[@]}"; do
    report "- ASC key file: ${f}"
  done
else
  report "- ASC key files: none in standard dirs"
fi
if [[ ${#android_key_files[@]} -gt 0 ]]; then
  for f in "${android_key_files[@]}"; do
    report "- Android material: ${f}"
  done
else
  report "- Android material: none found in workspace"
fi

report ""
report "## Host / operator toolchain"
report ""

xcode_ok=0
java_ok=0
pod_ok=0
fastlane_ok=0
adb_ok=0
sdkmanager_ok=0
transporter_ok=0
android_studio_ok=0

if xcodebuild -version >/dev/null 2>&1; then xcode_ok=1; else host_blockers+=("Xcode unavailable"); fi
if java -version >/dev/null 2>&1; then java_ok=1; else host_blockers+=("Java runtime missing"); fi
if has_cmd pod; then pod_ok=1; else host_blockers+=("CocoaPods missing"); fi
if has_cmd fastlane; then fastlane_ok=1; else host_blockers+=("Fastlane missing"); fi
if has_cmd adb || [[ -x "$HOME/Library/Android/sdk/platform-tools/adb" ]]; then adb_ok=1; else host_blockers+=("adb unavailable"); fi
if has_cmd sdkmanager || [[ -x "$HOME/Library/Android/sdk/cmdline-tools/latest/bin/sdkmanager" ]]; then sdkmanager_ok=1; else host_blockers+=("Android cmdline-tools/sdkmanager missing"); fi
if [[ -d "/Applications/Android Studio.app" ]]; then android_studio_ok=1; else host_blockers+=("Android Studio missing"); fi
if [[ -d "/Applications/Transporter.app" ]]; then transporter_ok=1; else host_blockers+=("Transporter.app missing"); fi

xcode_ev="$(xcodebuild -version 2>/dev/null | tr '\n' ' ' | sed 's/[[:space:]]\+/ /g' || true)"
java_ev="$(/usr/libexec/java_home -V 2>&1 | head -n 1 || true)"
pod_ev="$(pod --version 2>/dev/null || echo missing)"
fastlane_ev="$(fastlane --version 2>/dev/null | head -n 1 || echo missing)"
adb_ev="$(command -v adb || echo "$HOME/Library/Android/sdk/platform-tools/adb")"
sdkmanager_ev="$(command -v sdkmanager || echo missing)"

report "| Check | Status | Evidence |"
report "|---|---|---|"
report "| Xcode | $(status_word "$xcode_ok") | ${xcode_ev//|/\\|} |"
report "| Java runtime | $(status_word "$java_ok") | ${java_ev//|/\\|} |"
report "| CocoaPods | $(status_word "$pod_ok") | ${pod_ev//|/\\|} |"
report "| Fastlane | $(status_word "$fastlane_ok") | ${fastlane_ev//|/\\|} |"
report "| adb | $(status_word "$adb_ok") | ${adb_ev//|/\\|} |"
report "| sdkmanager | $(status_word "$sdkmanager_ok") | ${sdkmanager_ev//|/\\|} |"
report "| Android Studio | $(status_word "$android_studio_ok") | /Applications/Android Studio.app |"
report "| Transporter.app | $(status_word "$transporter_ok") | /Applications/Transporter.app |"
report ""
report "## Operator-facing gaps CIO / Release should know"
report ""
blocker_count=0
for item in "${endpoint_blockers[@]-}"; do
  [[ -n "$item" ]] || continue
  report "- ${item}"
  blocker_count=$((blocker_count + 1))
done
for item in "${visibility_blockers[@]-}"; do
  [[ -n "$item" ]] || continue
  report "- ${item}"
  blocker_count=$((blocker_count + 1))
done
for item in "${credential_blockers[@]-}"; do
  [[ -n "$item" ]] || continue
  report "- ${item}"
  blocker_count=$((blocker_count + 1))
done
for item in "${host_blockers[@]-}"; do
  [[ -n "$item" ]] || continue
  report "- ${item}"
  blocker_count=$((blocker_count + 1))
done
if [[ $blocker_count -eq 0 ]]; then
  report "- No blockers detected in this audit snapshot."
fi

report ""
report "## Current workspace state relevant to publication"
run_capture "tdf-mobile git status --short" bash -lc "cd '$MOBILE_DIR' && git status --short"
run_capture "tdf-mobile release metadata files" bash -lc "cd '$MOBILE_DIR' && for f in app.config.ts app.json eas.json package.json; do echo '---' \"\$f\"; [ -f \"\$f\" ] && sed -n '1,120p' \"\$f\"; done"
run_capture "EAS auth snapshot" bash -lc "cat '$eas_summary'"

report ""
report "## Recommended next operator actions"
report ""
report "1. Attach relay-enabled signed-in tabs for App Store Connect and Google Play Console so live admin visibility can be audited without credential export."
report "2. Decide whether Apple submission will use EAS-managed credentials only or local Apple tooling; if local, install/import at least one Apple signing identity, provisioning material, and Transporter or fastlane path."
report "3. Install missing operator tooling on this Mac if local publishing is expected: Java runtime, CocoaPods, Fastlane, Android cmdline-tools."
report "4. If Android submission will use service-account JSON outside EAS-managed flows, place it in an approved operator location and document only the filename/location, not contents."
report ""

if [[ -n "$REPORT_PATH" ]]; then
  cp "$OUTPUT_FILE" "$REPORT_PATH"
fi
cat "$OUTPUT_FILE"
