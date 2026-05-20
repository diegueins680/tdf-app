# TDF Label — Release Checklist

_One-page checklist for every TDF Records mobile release._

## Pre-Build

- [ ] **Version / build number bumped** in `tdf-mobile/app.json` (or `package.json`).
- [ ] **CHANGELOG.md** updated with user-facing changes since last release.
- [ ] **EAS credentials valid** — run `npx eas credentials --platform ios` and confirm distribution cert + provisioning profile are not expired.
- [ ] **Detox baseline green** — last recorded PASS count ≥ current baseline (see `release-readiness.md`).
- [ ] **No uncommitted release-critical changes** — `git status` clean on release branch.

## Build

- [ ] **Trigger EAS preview build** — `cd tdf-mobile && npx eas build --platform ios --profile preview`.
- [ ] **Build succeeds** — verify in EAS dashboard or `npx eas build:list --platform ios --limit 1`.
- [ ] **Artifact URL recorded** — paste `.ipa` / install link into `release-readiness.md`.

## Validation

- [ ] **Simulator smoke test** — Detox release-suite PASS on primary simulator (`8DB9DCE0-2F80-49C9-A614-F21DA3876B7B`).
- [ ] **Physical-device smoke test** — install preview `.ipa` on a real iPhone, verify both login paths (username/password + Google OAuth).
- [ ] **No new crashes** — check Console / Xcode Organizer for regressions.

## Post-Ship

- [ ] **Git tag created** — `git tag -a v<version>-build-<build> -m "Release <version> build <build>"`.
- [ ] **GitHub Release drafted** — attach `.ipa`, link to CHANGELOG, mark pre-release if applicable.
- [ ] **Release report updated** — append entry to `reports/tdf-label-release.md` with PASS count, build ID, and blocker status.
- [ ] **Baseline incremented** — update `release-readiness.md` with new consecutive PASS count and build metadata.

---

_Keep this checklist version-controlled. Update when process changes._
