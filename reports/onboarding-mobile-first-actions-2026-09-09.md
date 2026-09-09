# Onboarding mobile first actions — 2026-09-09

## Outcome

Mobile `learning` and `professional_tools` onboarding intents now continue to concrete public tasks instead of dropping the user into the generic Explore list. Learning opens the public trial-lessons workflow; professional tools opens the public Music maker.

Both destinations are resolved from the generated feature registry and must pass its current view-access rule. A validated, authorized native `returnTo` still takes precedence for an interrupted task. When an intent opens a browser workflow, authentication first replaces the auth screen with the useful native directory landing, so a browser-open failure or a later return never strands the user on a completed login form.

The intent remains personalization only. It grants no role, module, permission, feature flag, or experiment enrollment.

## Verification

- Focused mobile auth and intent Jest passed: 2 suites, 32 tests.
- The complete mobile Jest corpus passed: 70 suites, 386 tests. A 15-second command-line timeout was used to tolerate local machine contention; no source or test timeout was changed.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions. The existing reviewed decision for the governed intent switch was migrated to its new structural ID.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `ad16d28`.

This slice changes no API path, database schema, authorization rule, analytics taxonomy, experiment assignment, or release flag.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
