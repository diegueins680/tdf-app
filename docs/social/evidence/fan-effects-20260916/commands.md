# Actual local commands, 2026-09-16

Worktree: `tdf-app-social-fanclub-20260916`. All final commands exited 0.

- `TDF_SOCIAL_JAVA=/Users/diegosaa/.local/java/jdk-17.0.12+7-jre/Contents/Home/bin/java TLA_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar TDF_SOCIAL_RESULTS=/private/tmp/social-fan-effects-model-unfollow bash scripts/social/check-fan-effects-model.sh`
- TLC with `-dump dot,actionlabels`, then `python3 scripts/social/generate-fan-effects-cases.py /private/tmp/social-fan-effects-unfollow-traces.dot scripts/social/FanEffectsModelCases.hs`: 708 observations.
- `TDF_SOCIAL_FAN_EFFECTS_BENCHMARK=1 TDF_SOCIAL_HTTP_NATIVE=1 TDF_SOCIAL_HTTP_BUILD=/var/folders/0s/0tg301f95s51dvsjf74ksxjm0000gn/T/tmp.mgq3mnPebJ bash scripts/social/test-http.sh`
- `stack --stack-yaml tdf-hq/stack.yaml test --fast`
- `TDF_SOCIAL_SCHEMA_NATIVE=1 bash scripts/social/test-schema-compatibility.sh`
- From `tdf-hq-ui`: `../node_modules/.bin/tsc -p tsconfig.app.json --noEmit`
- From `tdf-hq-ui`: `../node_modules/.bin/eslint src/pages/FanClubMemberProfilePage.tsx --max-warnings 0`
- From `tdf-hq-ui`: `NODE_OPTIONS=--experimental-vm-modules ../node_modules/.bin/jest --runInBand --runTestsByPath src/pages/PartyRelationshipMigration.test.ts`
- `npm run audit:catalog-lists`
- `git diff --check`

Empty TypeScript/ESLint logs mean successful silent commands, not omitted checks.
PostgreSQL native 16.10, GHC 9.10.3/lts24.42, Java17.0.12, Node24.8.
Model counterexample nonzero exits are required; development failures have separate excerpts.

Committed log copies normalize trailing whitespace only; full-log fingerprints refer to original local bytes.
