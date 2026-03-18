# iOS signing blocker handoff — 2026-03-16

## Scope
Unblock the first iOS production build for the TDF mobile app by resolving the exact Expo/Apple credential step that still requires a human.

## Confirmed release identity from the repo
- Expo project: `@cuco.saa/tdf-mobile`
- Expo project ID: `218aca4d-c096-4892-a353-c1dd7df23448`
- iOS bundle identifier in the live repo config: `com.tdfrecords.app`
- Production build profile exists in `tdf-mobile/eas.json`

## Confirmed blocking step
The iOS production build is not failing in app code; it is stopping at credential setup before the build can start non-interactively.

Observed failure text captured in prior release reports:
- `Using remote iOS credentials (Expo server)`
- `Distribution Certificate is not validated for non-interactive builds.`
- `Failed to set up credentials.`
- `Credentials are not set up. Run this command again in interactive mode.`

Related interactive prompt previously seen:
- `Do you want to log in to your Apple account?`

## Important identity mismatch to resolve before clicking through Apple screens
Earlier release notes referenced `com.tdf.records`, but the repo currently ships with `ios.bundleIdentifier = com.tdfrecords.app`.

Treat `com.tdfrecords.app` as the current source of truth unless the repo is changed intentionally. Do **not** guess the Apple team based on the older identifier.

## Minimum human action required
From `tdf-mobile/`, run an **interactive** Expo iOS production build or credentials flow using the Apple account/team that owns the app identifier in the repo:

```bash
cd /Users/diegosaa/GitHub/tdf-app/tdf-mobile
npx eas-cli@latest build --platform ios --profile production
```

If Expo still stops at credentials setup, continue interactively and:
1. Sign in to the Apple account that has access to the correct Apple Developer team.
2. Confirm that the selected team owns or can create/manage the App ID for `com.tdfrecords.app`.
3. Let Expo validate or generate the iOS distribution certificate / provisioning assets for that exact bundle identifier.
4. Capture the Apple team name/ID and the resulting build ID or exact error.

## If the operator sees a mismatch
If the Apple account/team only recognizes `com.tdf.records` and not `com.tdfrecords.app`, stop and treat that as the active blocker. At that point the next decision is not another blind retry; it is to decide whether:
- the repo bundle identifier must be changed, or
- the Apple-side app registration must be created/fixed for `com.tdfrecords.app`.

## Evidence sources
- Repo config: `tdf-mobile/app.config.ts`
- EAS build profile: `tdf-mobile/eas.json`
- Prior release evidence: `~/.openclaw/orgs/tdf-label/reports/tdf-label-release.md`
- Prior CTO evidence: `~/.openclaw/orgs/tdf-label/reports/tdf-label-cto.md`

## Expected output from the next human step
The operator should report back with exactly these facts:
- Apple account email used
- Apple Developer team name and team ID
- Whether that team owns `com.tdfrecords.app`
- Whether Expo validated/created the distribution certificate successfully
- New iOS build ID, or the exact new blocking error text
