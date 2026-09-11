# Onboarding directory-favorite continuity — 2026-09-08

## Outcome

Authenticated directory results now reflect the current Party's persisted favorites instead of presenting every result as unsaved. A user can add or remove a favorite through the existing desired-state API, the Party-scoped React Query cache updates after success, and a failed change remains visible and retryable.

Favorite state is keyed by Party identity and stays disabled while a newly selected account is loading, so a previous Party's saved state is never presented as the next Party's state. Anonymous users see an action that truthfully says they are signing in to save, and the login return preserves the active directory search and filters.

## Verification

- Focused directory Jest passed twice: 1 suite, 5 tests each run.
- Web TypeScript passed.
- Strict ESLint passed with zero warnings for the three touched TypeScript files.
- Strict catalog-list audit passed with no decision changes.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- The complete UI test corpus ran 190 suites and 1,797 tests: 189 suites and 1,796 tests passed. One parent-branch artist follow-continuity assertion missed its existing one-second polling window under concurrent Haskell compilation; that exact test passed in isolation when the polling window was temporarily extended to five seconds, and the diagnostic edit was reverted. The changed directory suite passed within the complete run.
- The production UI build passed; Vite transformed 12,416 modules and the initial-JavaScript budget remained within its limit at 5 preloads and 412,496 gzip bytes.
- `git diff --check` passed.

## Scope

This slice reuses the existing authenticated directory favorite endpoints and changes no API path, database schema, authorization rule, experiment state, or release flag. The result-card mutation boundary is exported for focused regression testing; Party ownership continues to be enforced by the server and the client cache is partitioned by authenticated Party ID.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
