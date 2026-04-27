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

## Pre-rerun gate

Before any smoke rerun, record one of these exactly:

- Platform rerun-ready handoff cited: `[timestamp + report path + exact line summary]`
- If no fresh rerun-ready handoff exists: `[first exact blocker]`

If the handoff is missing or the chosen localhost proof path fails, stop and publish the blocker instead of broadening scope.

## Allowed localhost proof paths

Use exactly one of these frozen commands and record it verbatim:

1. `cd /Users/diegosaa/GitHub/tdf-app/tdf-hq && make seed`
2. `cd /Users/diegosaa/GitHub/tdf-app/tdf-hq && set -a && source config/default.env && set +a && stack run`

## Same-run proof capture

Fill these before any smoke rerun:

- Platform handoff cited: `[timestamp + report path + exact line summary | first exact blocker]`
- Proof path used: `[make seed | stack run]`
- Exact command: `[paste exact command verbatim]`
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
