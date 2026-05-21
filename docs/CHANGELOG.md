# TDF Records — Changelog

_All user-facing changes per build. Keep this updated before every release._

## Unreleased

### Added
- _(populate before next release)_

### Fixed
- _(populate before next release)_

### Changed
- _(populate before next release)_

---

## Build History

### Preview Build — 2026-05-20
- **Build ID**: `2d8b5544-4304-4a19-a018-42c83930bce9`
- **EAS Profile**: `preview`
- **Artifact**: [`.ipa` download](https://expo.dev/artifacts/eas/6Cc91jFMt9UvTeTNAvLXRB.ipa)
- **Status**: Awaiting physical-device Google OAuth validation (due 2026-05-21)
- **Changes since baseline**:
  - A/B experiment infrastructure added
  - EAS iOS credentials resolved (distribution cert active until Nov 2026)
- **Verification**:
  - Simulator: 21 consecutive Detox PASSes as of 2026-05-20 20:21 UTC
  - Physical device: pending operator execution of [`google-oauth-physical-device-review.md`](./google-oauth-physical-device-review.md)

### Baseline — `glow-shore` (2026-05-16)
- **Build ID**: `8d91fabe-a01c-41d1-bc6b-b55dc9c689e9`
- **Status**: FINISHED 2026-05-13 20:20 UTC
- **Binary verified**: Mach-O universal (x86_64 + arm64), ~32.2 MB, 2,232 object files
- **Auth paths proven**: username/password + Google OAuth (simulator)
- **Detox streak origin**: 10th consecutive PASS achieved 2026-05-18 after `SIMCTL_DAEMON_DEADLOCK` resolved

---

## Known Issues

| Issue | Status | Since | Notes |
|-------|--------|-------|-------|
| Physical-device Google OAuth | **PENDING VALIDATION** | 2026-05-20 | Operator-gated; see [`google-oauth-physical-device-review.md`](./google-oauth-physical-device-review.md) |
| `SIMCTL_DAEMON_DEADLOCK` | **RESOLVED** | 2026-05-18 | Host restart cleared orphaned runtime processes |
| `EAS_IOS_CREDENTIALS_MISSING` | **RESOLVED** | 2026-05-20 | Distribution cert + provisioning profile active |

---

## Next Release Targets

- [ ] Physical-device Google OAuth PASS → promote preview to RC
- [ ] Tag `v1.0.0-build-15` and draft GitHub Release
- [ ] Add telemetry/PostHog verification to release checklist once telemetry lane is active
- [ ] Update App Store screenshots if UI changed

---

_Keep this file version-controlled. Update the "Unreleased" section as PRs land._
