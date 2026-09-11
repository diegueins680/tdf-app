# Web onboarding professional action — 2026-09-08

## Outcome

The `professional_tools` onboarding intent now lands an authenticated user on the public Creador musical workflow instead of discarding the intent and choosing the account's generic landing page. The destination exposes an immediate, concrete action: enter a prompt, generate a Tidal pattern, preview it in the browser, and copy the result.

The existing redirect contract remains authoritative. When authentication started from a validated task-specific return URL, such as resuming contact with a particular directory profile, that accessible URL still wins over the intent fallback. The intent continues to be personalization only and does not assign a role, module, or permission.

The `learning` intent already resolves to the public trials workflow. Focused coverage now locks both public first-action destinations to the same access-control model used for redirects.

## Verification

- Focused login-routing Jest: 1 suite, 14 tests passed.
- Web TypeScript passed.
- Strict ESLint passed for both changed TypeScript files.
- Strict catalog-list audit passed with no decision change.
- Repository quality passed, including formal, release, CI-selection, and persona-program checks.
- The first full UI run reproduced the known unrelated `CourseRegistrationsAdminPage` timing cascade: 188 of 189 suites passed, with all 54 failures confined to that untouched suite. The changed login-routing suite passed in that run.
- The untouched course-registration suite then passed 526/526 in a fresh process.
- A clean full UI rerun passed all 189 suites and 1,786 tests. Lint completed with 0 errors and 102 existing warnings; TypeScript and the production build passed; Vite transformed 12,416 modules and the initial-JavaScript budget remained within its limit at 5 preloads and 412,198 gzip bytes.
- `git diff --check` passed.

## Scope

This slice changes no API, database schema, authorization rule, analytics taxonomy, experiment state, or release flag. It performs no authentication against a live provider, production mutation, customer communication, merge, deployment, or experiment activation.
