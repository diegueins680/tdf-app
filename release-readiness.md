# TDF Records — Release Readiness

Last updated: 2026-05-19 17:30 UTC by tdf-label-release

## Baseline

- **Detox consecutive PASS streak:** 17 (latest: 2026-05-19 17:28 UTC)
- **Primary simulator:** `8DB9DCE0-2F80-49C9-A614-F21DA3876B7B` (iPhone 16)
- **Binary:** 32.2 MB Mach-O universal, built 2026-05-16 04:38 UTC (`glow-shore`)
- **Repo:** `/Users/diegosaa/GitHub/tdf-app`

## Resolved Issues

| Issue | Resolution Date | Notes |
|-------|----------------|-------|
| SIMCTL_DAEMON_DEADLOCK | 2026-05-18 | Simulator infrastructure stable since fix |

## Active Blockers

### EAS_IOS_CREDENTIALS_MISSING — Operator-Gated

**Impact:** Blocks `.ipa` generation and physical-device distribution.

**8-Step Unblocker:**

1. Run `cd tdf-mobile && npx eas-cli@latest credentials:manager --platform ios`
2. Select `"Add new Distribution Certificate"` or `"Use existing"`
3. If new: follow prompts to generate CSR, download certificate, import to Keychain
4. Select `"Add new Provisioning Profile"` → `"Distribution"` (Ad Hoc or App Store)
5. Ensure the correct Bundle ID `com.tdfrecords.app` is selected
6. Download and install the provisioning profile locally
7. Verify with `npx eas-cli@latest build:configure` — should show credentials green
8. Trigger preview build: `npm run build:ios:preview`

**Verification:** `npx eas-cli@latest build:list --platform ios --limit 1` shows a completed (not canceled) build.

### Physical-Device Google OAuth — Waived Until 2026-05-21

- Simulator Google OAuth flow passes Detox (confirmed in 17th PASS).
- Physical-device testing pending operator review on 2026-05-21.

## Quick Commands

```bash
# Detox regression test
cd tdf-mobile && npx detox test --configuration ios.sim.release e2e/firstTest.e2e.js

# Release build
cd tdf-mobile/ios && xcodebuild -workspace TDFRecords.xcworkspace -scheme TDFRecords -configuration Release -sdk iphonesimulator -derivedDataPath ../ios/build

# Simulator management
xcrun simctl list devices
xcrun simctl boot 8DB9DCE0-2F80-49C9-A614-F21DA3876B7B
xcrun simctl shutdown 8DB9DCE0-2F80-49C9-A614-F21DA3876B7B
```
