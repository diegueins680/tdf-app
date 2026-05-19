# TDF Label — Release Readiness

_Last updated: 2026-05-19 18:20 UTC by tdf-label-release_

## Current Baseline

| Metric | Value |
|--------|-------|
| Consecutive Detox PASSes | **17** (since 2026-05-19 17:26 UTC) |
| Primary Simulator | `8DB9DCE0-2F80-49C9-A614-F21DA3876B7B` (iPhone 16) |
| Binary | 32.2 MB Mach-O universal, built 2026-05-16 04:38 UTC (`glow-shore`) |
| EAS iOS Build Status | **No preview/production build since 5/18** — last build `86317f8c-...` canceled |

## Resolved Issues

- ✅ `SIMCTL_DAEMON_DEADLOCK` — Retired corrupted simulator `3C3D5759-6E10-480D-B768-2747B9B0D02A`; primary simulator stable.

## Active Blockers

### `EAS_IOS_CREDENTIALS_MISSING` — Blocks `.ipa`/Physical-Device Distribution

**Owner:** Operator-gated (requires Apple Developer account action + local keychain trust).
**Status:** PERSISTENT since 2026-05-17.

#### 8-Step EAS iOS Credentials Unblocker

1. **Verify Apple Developer Program membership** is active at https://developer.apple.com/account
2. **Run `npx eas credentials`** in `tdf-mobile/` and select `iOS`
3. **Choose "Generate new credentials"** when prompted for Distribution Certificate
   - If "Use existing credentials" appears, verify expiry > 90 days
4. **Choose "Generate new provisioning profile"** for the TDFRecords bundle ID
5. **Download the generated `.p12` certificate** and double-click to install in `login` keychain
6. **Open Keychain Access → login → Certificates** — find `Apple Distribution: ...` and set Trust to **"Always Trust"**
7. **Run `npx eas build:configure`** to sync `eas.json` with new credentials
8. **Trigger preview build:** `cd tdf-mobile && npx eas build --platform ios --profile preview`

**Verification:** Build should reach "Build in progress" on EAS dashboard; previous blocker was immediate cancellation.

### Physical-Device Google OAuth

**Status:** Waived until 2026-05-21.
**Next action:** Review `e2e/firstTest.e2e.js` Google OAuth flow on physical device once `.ipa` is available.

## Quick Commands

```bash
# Detox regression test
cd tdf-mobile && npx detox test --configuration ios.sim.release e2e/firstTest.e2e.js

# Release build (local simulator)
cd tdf-mobile/ios && xcodebuild -workspace TDFRecords.xcworkspace -scheme TDFRecords -configuration Release -sdk iphonesimulator -derivedDataPath ../ios/build

# Simulator management
xcrun simctl list devices
xcrun simctl boot 8DB9DCE0-2F80-49C9-A614-F21DA3876B7B

# EAS build status
npx eas build:list --platform ios --limit 1
```

## Evidence Archive

- Detox screenshots: `~/.openclaw/orgs/tdf-label/evidence/release-detox-login-YYYYMMDD-HHMM.png`
- Release reports: `~/.openclaw/orgs/tdf-label/reports/tdf-label-release.md`
