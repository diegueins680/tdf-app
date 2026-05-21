# TDF Label — Release Readiness

_Last updated: 2026-05-21 04:52 UTC by tdf-label-release_

## Current Baseline

| Metric | Value |
|--------|-------|
| Consecutive Detox PASSes | **21** (since 2026-05-20 20:21 UTC) |
| Primary Simulator | `8DB9DCE0-2F80-49C9-A614-F21DA3876B7B` (iPhone 16) |
| Binary | 32.2 MB Mach-O universal, built 2026-05-16 04:38 UTC (`glow-shore`) |
| EAS iOS Build Status | **✅ BUILD SUCCESS** — Preview build `2d8b5544-...` finished 2026-05-20 07:14 GMT-5 |

## Resolved Issues

- ✅ `SIMCTL_DAEMON_DEADLOCK` — Retired corrupted simulator `3C3D5759-6E10-480D-B768-2747B9B0D02A`; primary simulator stable.
- ✅ `EAS_IOS_CREDENTIALS_MISSING` — Distribution certificate + provisioning profile active until Nov 2026. First preview build successful.

## Active Blockers

### Physical-Device Google OAuth

**Status:** Review scheduled for 2026-05-21 (tomorrow).
**Next action:** Test Google OAuth flow on physical device with new `.ipa`.
**Install link:** https://expo.dev/accounts/cuco.saa/projects/tdf-mobile/builds/2d8b5544-4304-4a19-a018-42c83930bce9

## Latest Build

| Field | Value |
|-------|-------|
| Build ID | `2d8b5544-4304-4a19-a018-42c83930bce9` |
| Platform | iOS |
| Profile | preview |
| Distribution | internal |
| Version | 1.0.0 |
| Build Number | 14 |
| Commit | `132a5fba8` (fix: update frozen lane simulator UUID) |
| Started | 2026-05-20 13:07:00 PM |
| Finished | 2026-05-20 13:07:00 PM |
| IPA URL | https://expo.dev/artifacts/eas/6Cc91jFMt9UvTeTNAvLXRB.ipa |

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
cd tdf-mobile && npx eas build:list --platform ios --limit 1

# Trigger preview build
cd tdf-mobile && npx eas build --platform ios --profile preview
```

## Evidence Archive

- Detox screenshots: `~/.openclaw/orgs/tdf-label/evidence/release-detox-login-YYYYMMDD-HHMM.png`
- Release reports: `~/.openclaw/orgs/tdf-label/reports/tdf-label-release.md`
