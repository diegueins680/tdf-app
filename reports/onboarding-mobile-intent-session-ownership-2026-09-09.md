# Onboarding mobile intent session ownership — 2026-09-09

## Outcome

Existing-account login no longer lets a late onboarding-intent acknowledgement clear pending state owned by a newer authentication attempt.

Password and Google login now bind best-effort cleanup to the token that started the authenticated persistence request. Cleanup also compares the acknowledged intent with the value currently stored before removing it. A per-route promise deduplicates deep-link intent persistence so a delayed duplicate write cannot recreate the key after successful cleanup.

Authentication and navigation still complete even when intent persistence fails. The validated intent remains available for a later retry in that case.

## Verification

- Focused mobile Jest passed: 3 suites, 46 tests.
- The complete mobile Jest corpus passed: 70 suites, 390 tests.
- Mobile TypeScript passed.
- Strict mobile ESLint passed with zero warnings.
- Root catalog-list audit passed after scanning 1,279 files and 945 candidates, with no unreviewed candidates or stale decisions.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed.

## Scope

Mobile implementation commit: `0431e1f`.

This slice changes no API path or payload, database schema, authorization rule, onboarding destination, analytics taxonomy, experiment assignment, or release flag.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
