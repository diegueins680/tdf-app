# Physical Device Test Guide — TDF Label

_One-page guide for installing the preview `.ipa` and validating Google OAuth on a real iPhone._

_Last updated: 2026-05-21 19:50 UTC by tdf-label-release_

---

## Prerequisites

- iPhone with iOS 16+
- Preview `.ipa` install link (see `release-readiness.md`)
- Apple ID enrolled in the same team / Ad Hoc provisioning profile
- Screen recording enabled (Settings → Control Center → Screen Recording)

---

## Install Steps

1. Open the install link on the iPhone (or scan QR code from EAS dashboard).
2. Tap **Install** when prompted.
3. Go to **Settings → General → VPN & Device Management**.
4. Trust the developer profile associated with the TDF Records app.
5. Launch the app. It should open to the login screen without crashing.

---

## Google OAuth Test Steps

1. On the login screen, tap **Sign in with Google**.
2. Complete the system OAuth consent flow (select account, approve scopes).
3. Verify the app receives the token and transitions to the main screen.
4. **Success criteria:**
   - No crash during or after OAuth.
   - Main screen loads with authenticated user context.
   - Token is valid (app does not immediately kick back to login).

---

## Evidence Capture

- **Screen record** the entire flow from app launch through OAuth success.
- **Screenshot** the main screen showing authenticated state.
- Save files as:
  - `evidence/google-oauth-physical-YYYYMMDD-HHMM.mov`
  - `evidence/google-oauth-physical-YYYYMMDD-HHMM.png`
- Append result to `reports/tdf-label-release.md` with timestamp and PASS/FAIL.

---

## Troubleshooting

| Symptom | Likely Cause | Fix |
|---------|-------------|-----|
| "Unable to install" | Provisioning profile mismatch | Verify device UDID is in the Ad Hoc profile; rebuild if needed |
| "Untrusted developer" | Profile not trusted | Settings → General → VPN & Device Management → Trust |
| App crashes on launch | Bundle ID mismatch or missing Google OAuth config | Check `app.json` `ios.bundleIdentifier` matches Google Cloud Console; verify `GoogleService-Info.plist` is included |
| OAuth hangs / cancels | Wrong `iosClientId` or redirect URI | Cross-check `app.json` vs Google Cloud Console iOS client ID |

---

## References

- `release-readiness.md` — current install link and blocker status
- `docs/secrets-hygiene.md` — bundle ID and OAuth client ID verification
- `reports/tdf-label-release.md` — where to append test result
