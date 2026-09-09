# Onboarding personal-draft session boundary — 2026-09-08

## Outcome

Buyer contact and public-booking profile drafts now follow the authenticated session boundary in the current browser tab. An anonymous draft is preserved through the intended sign-in continuation, and a refresh for the same account keeps it available. Logout, session expiry, and a switch from one authenticated party to another clear every registered personal-data draft before the new session becomes active.

The storage helper now maintains a session-scoped registry, so future personal-draft keys using the helper inherit the same cleanup behavior. It also explicitly removes the two known pre-registry keys during cleanup, including legacy `localStorage` values, so an upgrade cannot expose an older buyer or booking draft to a later user of the browser.

## Verification

- Focused session-storage and provider Jest: 3 suites, 15 tests passed.
- Web TypeScript passed.
- Strict ESLint passed with zero warnings for the four touched TypeScript files.
- Strict catalog-list audit passed with no decision changes.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- Full UI quality passed all 190 suites and 1,793 tests. Whole-tree lint completed with 0 errors and 102 existing warnings; TypeScript and the production build passed; Vite transformed 12,416 modules and the initial-JavaScript budget remained within its limit at 5 preloads and 412,512 gzip bytes.
- `git diff --check` passed.

## Scope

This slice changes no API, database schema, authorization rule, analytics taxonomy, experiment state, or release flag. It preserves anonymous-to-authenticated draft continuity by design and clears drafts only when leaving an authenticated party identity. Browser behavior is covered through unit and `SessionProvider` integration tests in jsdom; a live cross-account browser session was not performed.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
