# Localhost auth exact-lane release checklist

Use this only for the frozen localhost lane in `objectives/tdf-label-release.md`.
Do not switch simulator, app path, bundle id, backend lane, or test plan.

## Frozen lane of record

- Simulator id: `8DB9DCE0-2F80-49C9-A614-F21DA3876B7B`
- App path: `/Users/diegosaa/GitHub/tdf-app/tdf-mobile/ios/build/Build/Products/Debug-iphonesimulator/TDFRecords.app`
- Bundle id: `com.tdfrecords.app`
- Step 1: `cd '/Users/diegosaa/GitHub/tdf-app/tdf-mobile' && npx expo start --dev-client --host localhost`
- Step 2: `xcrun simctl install 8DB9DCE0-2F80-49C9-A614-F21DA3876B7B '/Users/diegosaa/GitHub/tdf-app/tdf-mobile/ios/build/Build/Products/Debug-iphonesimulator/TDFRecords.app'`
- Step 3: `xcrun simctl launch 8DB9DCE0-2F80-49C9-A614-F21DA3876B7B com.tdfrecords.app`
- Baseline smoke evidence anchor: `/Users/diegosaa/.openclaw/orgs/tdf-label/evidence/ios-six-category-smoke-20260426-2055`

## Allowed localhost proof paths

Use exactly one of these and record the exact command used:

1. `make seed` on the running backend
2. local `stack run` with `config/default.env`

## Same-run proof capture

Fill these before any smoke rerun:

- Proof path used: `[make seed on running backend | stack run with config/default.env]`
- Exact command: `[paste exact command]`
- Localhost auth evidence path: `[path to screenshot/log/output]`
- Proof result: `[success | first exact blocker]`

## Exact-lane smoke rerun capture

- Same-run evidence directory: `[path]`
- Username/password login: `[pass | fail | blocker]`
- Google login: `[pass | fail | blocker]`
- Onboarding: `[pass | fail | blocker]`
- Primary navigation: `[pass | fail | blocker]`
- Core task flow: `[pass | fail | blocker]`
- Top error states: `[pass | fail | blocker]`

## Stop rules

- Stop at the first exact blocker.
- If the localhost auth proof fails, publish the exact failing command/output plus one evidence path.
- Do not broaden scope beyond the frozen lane.

## Release decision block

- RC verdict: `[GO | NO-GO]`
- Shipping decision: `[SHIPPABLE | NOT YET SHIPPABLE]`
- Highest-risk remaining failure (if any): `[single item]`
- Owner: `[single owner]`
- Immediate next fix lane: `[single lane]`
