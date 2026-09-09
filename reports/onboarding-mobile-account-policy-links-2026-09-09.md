# Onboarding mobile account-policy links — 2026-09-09

## Outcome

Mobile signup now links consent to the same account-specific terms and privacy notice represented by the submitted `tdf-account-terms-v1` policy version. It no longer sends a user to the separate mobile-app store policy while recording acceptance of the account policy.

This aligns password and Google signup disclosure with the existing web signup contract. The public mobile-app terms, privacy, support, and deletion pages remain available from the app's general legal-links surface for their separate store and companion-app purpose.

The account policy pages currently contain reviewed English copy. A Spanish legal equivalent still requires content/legal approval and is not invented by this engineering slice.

## Verification

- Focused mobile auth Jest passed: 1 suite, 22 tests, without React `act` warnings.
- The complete mobile Jest corpus passed: 70 suites, 387 tests. A 15-second command-line timeout was used to tolerate local machine contention; no source or test timeout was changed.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `2713be1`.

This slice changes no policy text, accepted policy version, API path, database schema, authorization rule, analytics taxonomy, experiment assignment, or release flag.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
