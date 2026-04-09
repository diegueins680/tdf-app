# Mobile App Store Submission Checklist

This checklist is for the current TDF Records Expo mobile app and the public legal pages added under `tdf-hq-ui/public/mobile-app/`.

## Canonical release identity

Use these repo-backed values as the source of truth for the current mobile release unless the repo is intentionally changed:

- App name: `TDF Records`
- Expo account/project: `@cuco.saa/tdf-mobile`
- EAS project ID: `218aca4d-c096-4892-a353-c1dd7df23448`
- iOS bundle identifier: `com.tdfrecords.app`
- Android application ID: `com.tdfrecords.app`
- Primary EAS release build profile: `production`
- Android submit profile: `production` → Play track `internal`, release status `draft`
- Runtime/app version source in repo: `app.config.ts` uses `APP_VERSION` with `runtimeVersion = APP_VERSION`

Important: older notes may still reference `com.tdf.records`. Treat that older identifier as stale unless a deliberate repo change says otherwise.

## Packet A login proof gate before any Packet B store action

Do not advance any Play Console or App Store Connect submission step until the release lane has one same-run canonical login proof pack.

For the current release lane, the control-plane anchors are:

- Android active anchor: `d5802e5c-2446-473a-9fd0-55f6979eacd6`
- iOS active anchor: `26be7bda-9195-4944-adea-665028aec528`
- Android build to keep excluded from promotion decisions unless a newer canonical proof pack explicitly supersedes that exclusion: `738e7792-3307-442e-8640-74eacd606c5f`

Before any Packet B store action, confirm all of the following:

- Platform evidence includes the exact line `Release-lane login proof = complete`.
- One canonical proof-pack path covers successful `username/password` and successful `Google login` on both active anchors above.
- If that proof is absent, stop and record exactly `Packet B gate = frozen behind Packet A proof` instead of mutating either store console.

## Publish the public URLs first

Deploy the existing public web app so these routes are reachable without authentication:

- `/mobile-app/support.html`
- `/mobile-app/privacy.html`
- `/mobile-app/terms.html`
- `/mobile-app/data-deletion.html`

The current repo-documented public host is `https://tdf-app.pages.dev`, so the store-facing URLs are:

- `https://tdf-app.pages.dev/mobile-app/support.html`
- `https://tdf-app.pages.dev/mobile-app/privacy.html`
- `https://tdf-app.pages.dev/mobile-app/terms.html`
- `https://tdf-app.pages.dev/mobile-app/data-deletion.html`

## App Store Connect

- Set the **Support URL** to the public support page.
- Set the **Privacy Policy URL** to the public privacy page.
- Optionally set the **User Privacy Choices URL** to the public deletion page.
- If you want a reviewer-friendly marketing or reference URL, use the mobile overview page or support page.
- Add reviewer notes that explain the current permission usage:
  - Camera is used for QR scanning and inventory photo capture.
  - Photo library access is used for attaching existing inventory images.
  - Location is optional and only used for nearby venue discovery.
- Provide a working review credential or token flow explanation. The current app relies on a Bearer token entry flow rather than a public sign-up screen.
- Include instructions for any feature that depends on pre-existing TDF data, such as Party IDs, venues, or inventory records.

## Google Play Console

- Set the **Privacy Policy** field to the public privacy page.
- Set the **App support** or public support/contact destination to the support page.
- Use the privacy page and deletion page when filling the **Data safety** narrative so the public answers match the app behavior.
- If Google treats the app as subject to the account deletion requirement, use the deletion page and explain the review path handled by email.
- Make sure the store listing explains that location, camera, and photo access are optional and only enable specific features.

## Reviewer notes grounded in the current code

- `tdf-mobile/app/(tabs)/inventory.tsx`
  - Requests camera permission for inventory photo capture.
  - Opens the photo library for existing inventory image selection.
- `tdf-mobile/app/(tabs)/vcard.tsx`
  - Requests camera permission for QR and vCard scanning.
- `tdf-mobile/app/venueExplorer.tsx`
  - Requests foreground location permission to show nearby venues.
- `tdf-mobile/src/providers/AuthProvider.tsx`
  - Persists the auth token locally so users do not need to paste it again every session.
- `tdf-mobile/src/providers/UserSettingsProvider.tsx`
  - Persists optional identity helpers such as Party ID and display name locally on the device.

## Release sanity checks

- Verify every public URL above loads in an incognito browser window.
- Verify each page works on mobile width as well as desktop.
- Confirm the contact emails resolve to active inboxes:
  - `soporte@tdfrecords.com`
  - `privacidad@tdfrecords.com`
  - `info@tdfrecords.net`
- Confirm the public mailing address is still correct before submission:
  - `Felix Oralabal N45-55, Quito, Ecuador`
- Confirm the app binary permission strings match the actual feature use described above.
- Prepare a reviewer note with a sample token or a reproducible internal review path.

## Known gap to verify before release

The repo exposes a public mailing address and several public email aliases, but it does not expose a clearly approved public support phone number. If Apple, Google, local law, or counsel require one for listing compliance, add it before submission rather than inventing one in the public pages.

## Verified in the current app build

The current `tdf-mobile` app already exposes the public support, privacy, terms, and data deletion links from the in-app About / Support and legal section (`app/(tabs)/about.tsx`, `app/about.tsx`, `src/components/PublicLinksSection.tsx`). This is no longer a release blocker, but the links should still be smoke-tested on a real release candidate before submission.

## Official references checked on 2026-03-13

- Apple App Store Connect app information:
  - `https://developer.apple.com/help/app-store-connect/reference/app-information/app-information`
- Apple app privacy management:
  - `https://developer.apple.com/help/app-store-connect/manage-app-information/manage-app-privacy`
- Apple App Review Guidelines, section 5.1.1:
  - `https://developer.apple.com/app-store/review/guidelines/`
- Google Play Developer Program Policy:
  - `https://support.google.com/googleplay/android-developer/answer/16329168`
- Google Play User Data help:
  - `https://support.google.com/googleplay/android-developer/answer/10144311`
