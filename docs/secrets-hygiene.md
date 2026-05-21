# Secrets Hygiene Checklist — TDF Label

_Last updated: 2026-05-21 16:20 UTC by tdf-label-release_

## Purpose

Prevent credential expiry and misconfiguration from blocking releases. This doc pairs with `release-checklist.md` and `release-readiness.md`.

---

## EAS Credentials Review

| Check | Command / Location | Cadence | Owner |
|-------|-------------------|---------|-------|
| Distribution certificate expiry | Apple Developer Portal → Certificates | Monthly | tdf-label-ceo |
| Provisioning profile validity | `cd tdf-mobile && npx eas credentials` | Monthly | tdf-label-ceo |
| Push notification cert (if enabled) | Apple Developer Portal | Monthly | tdf-label-ceo |
| EAS build secret rotation | Expo Dashboard → Project Secrets | Quarterly | tdf-label-cto |

**Expiry alert threshold:** 30 days before expiration → create calendar reminder.

---

## Google OAuth Client IDs

| Check | Command / Location | Cadence | Owner |
|-------|-------------------|---------|-------|
| iOS client ID valid | Google Cloud Console → APIs & Services → Credentials | Monthly | tdf-label-cto |
| Bundle ID matches | `tdf-mobile/app.json` → `ios.bundleIdentifier` | Per release | tdf-label-release |
| OAuth consent screen up to date | Google Cloud Console → OAuth consent screen | Quarterly | tdf-label-ceo |

**Physical-device test:** Install preview `.ipa`, complete Google OAuth, verify token exchange. Document result in `reports/tdf-label-release.md`.

---

## Local Environment Variables

| Rule | Enforcement |
|------|-------------|
| `.env` never committed | `tdf-mobile/.gitignore` includes `.env` |
| `.env.example` kept current | Update when new vars added; no secrets in example |
| Runtime validation | App crashes fast on missing required vars (fail closed) |

**Required vars (example only — no real values):**
```bash
# .env.example
GOOGLE_IOS_CLIENT_ID=your-client-id.apps.googleusercontent.com
API_BASE_URL=https://api.example.com
POSTHOG_API_KEY=your-posthog-key
```

---

## Rotation Cadence

| Secret Type | Rotation Frequency | Trigger Event |
|-------------|-------------------|---------------|
| EAS distribution certificate | Annually or on leak | Apple cert expiry, team member departure |
| Google OAuth client secret | On leak or suspicion | GitHub security alert, unauthorized API usage |
| Expo access token | Quarterly | Scheduled rotation in password manager |
| PostHog / telemetry keys | On leak | Vendor security notice |

---

## Incident Response

1. **Suspected leak:** Rotate immediately → update `release-readiness.md` → notify tdf-label-ceo.
2. **Cert expiry within 7 days:** Emergency build blocked → escalate to tdf-label-ceo for Apple Developer portal access.
3. **OAuth client misconfiguration:** Physical-device test fails → check bundle ID, consent screen, and client ID in `app.json` vs Google Cloud Console.

---

## References

- `release-checklist.md` — pre-build verification steps
- `release-readiness.md` — current blocker status and baseline
- `docs/CHANGELOG.md` — release history
