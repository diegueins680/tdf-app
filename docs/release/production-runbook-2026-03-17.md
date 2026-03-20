# Production release runbook — 2026-03-17

## Purpose
Freeze the current release identity, Android RC-of-record, exact production commands, and the remaining operator-only iOS signing step.

## Canonical release identity
- App name: `TDF Records`
- Expo slug: `tdf-mobile`
- Canonical bundle/package identifier: `com.tdfrecords.app`
- EAS project ID: `218aca4d-c096-4892-a353-c1dd7df23448`
- Expo project/account of record: `@cuco.saa/tdf-mobile`
- Production EAS profile: `production`
- Production channel: `production`

Repo sources:
- `tdf-mobile/app.config.ts`
- `tdf-mobile/eas.json`

Control-plane sources:
- `~/.openclaw/orgs/tdf-label/reports/tdf-label-cto.md`
- `~/.openclaw/orgs/tdf-label/reports/tdf-label-release.md`

## Android RC of record
Treat Android build `96085d99-784f-4d33-a3c7-75dea775b076` as the current RC of record until a newer readable production build explicitly supersedes it.

- RC: `yes`
- Commit: `ba50be07614c24354c911ddb91eb510e3c332770`
- Status from control-plane evidence: `FINISHED`
- Artifact: `https://expo.dev/artifacts/eas/jwxJytjvDbTXJCkuABix7E.aab`
- Engineering blocker before Play submission: `none known`

Why this is the RC now:
- The repo already has production build and submit commands wired.
- Prior release evidence for build `738e7792-3307-442e-8640-74eacd606c5f` was not locally readable in this workspace.
- The latest manager-visible evidence already proves `96085d99-784f-4d33-a3c7-75dea775b076` finished and produced the Android `.aab` artifact.

## Exact production commands
Run from `tdf-mobile/`.

### 1) Final local validation
```bash
npm run release:prepare
```

### 2) Android build / submit
```bash
npm run eas:build:android
npm run eas:submit:android
```

### 3) iOS interactive signing/build
```bash
npm run eas:build:ios
```

If Expo stops for credentials, continue interactively with the Apple account/team that owns `com.tdfrecords.app`.

### 4) iOS submit after a successful build
```bash
npm run eas:submit:ios
```

## iOS build of record
Treat iOS build `f326183b-e98d-4c29-b9cf-6737de91275b` as the current build of record until a newer readable production build explicitly supersedes it.

- RC: `yes`
- Status from control-plane evidence: `FINISHED`
- Bundle identifier: `com.tdfrecords.app`
- App Store Connect app ID: `6754828747`
- Apple team of record: `83J23NPXG7 (Diego Saa (Individual))`
- Artifact: `https://expo.dev/artifacts/eas/w2sgPbSdsYFztWQ3y6xUyB.ipa`
- iOS credential/setup blocker: `cleared`
- Engineering blocker before App Store submission: `none known`

## Submission packet freeze
- **Android submission anchor**: build `96085d99-784f-4d33-a3c7-75dea775b076`
  - Artifact: `https://expo.dev/artifacts/eas/jwxJytjvDbTXJCkuABix7E.aab`
  - Promotion rule: do **not** promote queued-only Android build `738e7792-3307-442e-8640-74eacd606c5f` from current evidence.
- **iOS submission anchor**: build `f326183b-e98d-4c29-b9cf-6737de91275b`
  - Artifact: `https://expo.dev/artifacts/eas/w2sgPbSdsYFztWQ3y6xUyB.ipa`
  - App Store Connect app: `6754828747`
  - Apple team: `83J23NPXG7 (Diego Saa (Individual))`
- **Review contact**: `Diego Saa` / `0984755301` / `cuco.saa@gmail.com`
- **Operator inputs still missing**:
  - reviewer/demo access path or bearer token
  - final privacy/data-safety sign-off
  - final Android/iOS screenshots
  - store credential ownership / handoff
- **Engineering blocker from build/signing evidence**: none from current readable evidence.
- **Submission-copy blocker still open**: the repo currently still exposes in-app QR/vCard scanning via `CameraView` + `onBarcodeScanned` in `tdf-mobile/app/(tabs)/vcard.tsx`, so the objective's planned post-scanner wording (`barcode scanning removed` / `manual QR payload import remains`) is **not** source-backed yet. Do not ship that wording until engineering removes the scanner flow or the objective is corrected.

## App Store review contact card

- First name: `Diego`
- Last name: `Saa`
- Phone: `0984755301`
- Email: `cuco.saa@gmail.com`

Enter these values directly in App Store Connect review contact fields for the current submission.

## Store-copy blocker and paste-ready wording

Before anyone reuses older submission text: **do not claim the scanner was removed or that the vCard flow is manual-import-only.** The current repo still opens `CameraView` and handles `onBarcodeScanned` in `tdf-mobile/app/(tabs)/vcard.tsx`, so any scanner-removed wording would drift from source.

### App Review note

```text
TDF Records is intended for internal staff and approved collaborators.

The app does not support public sign-up. Protected features require an organization-provided bearer token.

Review flow:
1. Launch the app and complete onboarding.
2. Open the auth screen and paste the review token provided separately for this submission.
3. Verify parties, bookings, pipeline stages, events, social/vCard tools, inventory flows, and venue explorer.

Permissions used by the current build:
- Camera: optional; used to scan vCard QR codes on the social/vCard screen and to capture inventory images
- Photo library: optional; used to attach inventory images
- Location: optional foreground-only; used to show nearby venues in venue explorer

If reviewer access to backend endpoints is limited, protected CRM screens may show restricted-access states instead of editable data.
```

### Play App Access

```text
No public sign-up is available. App access is restricted to internal staff and approved collaborators. Reviewers need the review bearer token or equivalent demo access path provided separately with the submission.
```

### Privacy / data-safety alignment

```text
Current release-path permissions and features from source:
- Camera: optional; used to scan vCard QR codes in the social/vCard flow and to capture inventory images
- Photo library: optional; used to attach inventory images
- Location: optional foreground-only; used to show nearby venues
- Microphone/audio recording: not requested in the release config
```

### Permission explanations

```text
Camera: Lets staff scan a vCard QR code in the social flow and optionally capture inventory photos.
Photo library: Lets staff attach an existing image to an inventory item.
Location: Lets staff find nearby venues in the venue explorer.
```

### Exact native permission strings

```text
Camera (expo-camera / image-picker camera): Allow TDF Records to use your camera to scan vCard QR codes and capture inventory images.
Photo library: Allow TDF Records to access your photos so you can attach inventory images.
Location: Allow TDF Records to use your location to show nearby venues.
```

### Do not say

- `com.tdf.records`
- `barcode scanning was removed`
- `manual QR payload import only`
- any claim that camera access is broader than `scan vCard QR codes and capture inventory images`
- any claim that the app scans broader barcode types; current source-backed behavior is QR-only in the vCard flow

## Submission blockers now
Engineering blockers are closed from the current readable evidence. Remaining work is submission operations and external review.

### Exact facts still needed from the operator
Release still needs these inputs before Android/iOS submission can be treated as ready:
- **Reviewer/demo access path** — bearer token or equivalent internal review path, plus any sample Party ID / seed data / step-by-step reviewer notes needed to reach protected flows.
- **Privacy / data-safety confirmation** — explicit sign-off that store answers match the current app behavior: optional camera, optional photo library, optional foreground location, no release-path audio recording, and the current account/data-deletion handling.
- **Final screenshots** — store-ready Android and iOS screenshots from the current release candidate build(s).
- **Store credential handoff** — who will execute `eas submit` / console submission, which account already has access, and whether any Play/App Store credential entry still has to be completed interactively. Live EAS evidence now shows:
  - iOS EAS Submit is **not yet set up** to use an App Store Connect API key for this project.
  - Android EAS Submit has **no** Google Service Account Key assigned for Play Store submissions.

### Observed App Store Connect state from 2026-03-18 operator screenshot
- App Store Connect record exists for `TDF Records` at app ID `6754828747`.
- iOS version `1.0` is currently in `Prepare for Submission`.
- No build is attached to version `1.0` yet; the version page still shows `Add Build`.

## Source-backed permission and feature truth
- **Camera** — optional; used for in-app QR scanning on the vCard tab (`tdf-mobile/app/(tabs)/vcard.tsx`) and optional inventory photo capture (`tdf-mobile/app/(tabs)/inventory.tsx`).
- **Photo library** — optional; used for inventory image selection (`tdf-mobile/app/(tabs)/inventory.tsx`).
- **Location** — optional foreground-only; used to show nearby venues in venue explorer (`tdf-mobile/app/venueExplorer.tsx`).
- **Microphone / audio recording** — not part of the current release path; `tdf-mobile/app.config.ts` sets `microphonePermission: false` for `expo-camera` and blocks Android `android.permission.RECORD_AUDIO`.

Use these statements for Play/App Store privacy answers and reviewer notes; they are the current repo-backed truth.

## Ownership buckets
### Engineering-closed
- Android RC of record is still `96085d99-784f-4d33-a3c7-75dea775b076` until superseded.
- iOS build of record is `f326183b-e98d-4c29-b9cf-6737de91275b`.
- Apple/Expo iOS credential setup is cleared for `com.tdfrecords.app` under `83J23NPXG7 (Diego Saa (Individual))`.
- Production build/submit commands are already wired in repo.
- Stop repeating the old identifier `com.tdf.records`; current engineering truth is `com.tdfrecords.app`.

### Operator-only
- Fill the submission packet items above in App Store Connect / Play Console.
- Upload/select the iOS build in App Store Connect version `1.0`; current console evidence shows no build attached yet.
- Provide the exact reviewer/demo access instructions and any required bearer token.
- Upload/confirm final screenshots and complete privacy/data-safety answers.
- Execute or delegate Android/iOS submission with the correct store credentials.
- If the EAS submit path is chosen, first set up the App Store Connect API key for iOS submit and assign the Google Service Account Key for Play Store submissions.

### External wait-state
- Expo build queue time if a replacement build is intentionally triggered.
- Google Play review.
- App Store / TestFlight review.

## Stale claims to stop repeating
- Do **not** use the old identifier `com.tdf.records`; the current release identity is `com.tdfrecords.app`.
- Do **not** say the scanner was removed or that the vCard flow is manual-import-only; current source still includes in-app QR/vCard scanning on the vCard tab.
- Do **not** say the app requests `RECORD_AUDIO`; Android release config explicitly blocks that permission.
- Do **not** describe camera access as broad or always-on; current release-facing camera use is optional and limited to QR/vCard scanning plus inventory photo capture.

## Store-facing delta check
One release-copy blocker was identified in this run: older handoff text says scanner removal/manual QR import only, but current source still exposes QR/vCard scanning in the vCard tab. Submission wording must stay on the source-backed variant above until engineering changes that flow.

## Exact next-step sequence (copy/paste handoff)
1. **Release** — freeze the build-of-record verdicts exactly as follows unless newer terminal evidence is added to the control plane:
   - Android RC of record: `96085d99-784f-4d33-a3c7-75dea775b076`
   - iOS build of record: `f326183b-e98d-4c29-b9cf-6737de91275b`
2. **Operator** — provide the submission packet items still missing:
   - reviewer/demo access path or bearer token
   - privacy / data-safety confirmation
   - final Android/iOS screenshots
   - store credential ownership / handoff
3. **Release** — from `tdf-mobile/`, run Android submission after step 2 is complete:
   ```bash
   npm run eas:submit:android
   ```
4. **Release / Operator** — upload/select the iOS build for App Store Connect app `6754828747` so version `1.0` no longer shows `Add Build`; this can be satisfied by the iOS EAS submit/upload path if the correct App Store credentials are available.
5. **Operator** — complete any Play Console/App Store Connect interactive metadata or credential prompts that EAS submission cannot finish autonomously.
6. **Release** — from `tdf-mobile/`, run iOS submission after App Store Connect metadata, review notes, and build attachment are ready:
   ```bash
   npm run eas:submit:ios
   ```
7. **External review** — wait on Google Play review and App Store / TestFlight review.

## Immediate next action
**Owner: Operator** — supply the four submission-packet items above so release can execute `npm run eas:submit:android` first, then `npm run eas:submit:ios` without drift.
