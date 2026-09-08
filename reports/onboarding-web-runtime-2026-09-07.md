# Onboarding web first-action runtime coverage — 2026-09-07

## Outcome

The public directory-contact and artist-follow handoffs now have route-level component coverage across anonymous and authenticated states. The tests execute real React Router transitions and React Query mutations with synthetic API fixtures; they do not send a contact, create a real follow, authenticate against a live provider, or mutate production data.

Directory contact coverage proves that an anonymous user receives an internal, profile-bound authentication return path; only the exact server-returned profile identifier unlocks the post-auth confirmation; the confirmation explains that nothing is sent automatically; continuing opens the existing protected composer with the exact target; cancelling removes the resume query; and a mismatched target fails closed to the normal explicit contact action.

Artist follow coverage proves that the anonymous CTA carries the exact artist and onboarding intent through signup; an authenticated return never follows automatically; explicit confirmation calls the follow operation for the rendered artist; only a successful mutation requests authoritative `artist_followed` completion and removes the resume query; a mismatched target does nothing; an already-followed artist removes stale resume state without another mutation or conversion; and a failed follow preserves retry context and emits no completion.

Automated accessibility checks found that the pages' primary and secondary Material UI progress indicators lacked accessible names. The profile, reviews, review-eligibility, artist, and release loading indicators now expose distinct Spanish labels, and the final component tests directly assert each loading-state name.

## Verification

- Final focused Jest: 2 suites, 11 tests passed.
- Automated serious/critical axe checks passed for the settled guest directory and artist pages; JSDOM color contrast remains outside this helper by design.
- Strict touched-file ESLint passed with zero warnings.
- Web TypeScript passed.
- Full UI quality passed on the final tree: 187 suites, 1,766 tests, TypeScript, lint with zero errors and the unchanged 102 repository warnings, production Vite build, and initial bundle budget at 5 preloads / 412,181 gzip bytes.
- Strict catalog-list audit passed with no unreviewed or stale decisions.
- `git diff --check` passed.

## Scope and residual risk

This slice changes no endpoint, database schema, authorization rule, role, analytics taxonomy, experiment assignment, or release configuration. It adds accessible names and controlled browser-DOM coverage only. No production deployment, merge, experiment activation, real OAuth, real message, real follow, physical-device run, or customer communication occurred.

The tests use JSDOM and mocked APIs, so a controlled end-to-end browser run with a real test backend remains useful for focus restoration, network timing, and full login lifecycle evidence. Native iOS/Android accessibility and deep-link verification remain separate. The onboarding experiment remains disabled and requires explicit operational approval before activation.
