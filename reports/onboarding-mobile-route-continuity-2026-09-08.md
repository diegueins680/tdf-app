# Mobile onboarding route continuity — 2026-09-08

## Outcome

Supported mobile deep links and protected-route authentication now share a closed, registry-backed internal routing boundary. A link such as `tdf://event/42?tab=moments` and its triple-slash equivalent both resolve to the same encoded event destination, while directory service/filter queries remain attached to the registered native search route.

When the mobile route guard sends an anonymous user to authentication, it now carries the exact normalized route query. After authentication, the destination is checked first against the generated feature registry and then against the roles/modules returned by that authentication response. Scheme-relative URLs, external schemes, credentials, fragments, encoded path separators, unknown routes, overlong targets, and oversized query payloads are not accepted as return destinations.

The mobile implementation is published at commit `40e2c2d234c9d59cf3b2ec235e5da8715a787cdd` in [TDF-mobile draft PR #50](https://github.com/diegueins680/TDF-mobile/pull/50), stacked on the existing experiment-authority branch.

## Verification

- Focused deep-link/auth Jest: 2 suites, 30 tests passed.
- Full mobile Jest: 66 suites, 365 tests passed.
- Mobile TypeScript passed.
- Mobile ESLint passed with zero warnings.
- The root `quality:mobile` gate repeated lint, TypeScript, and all 66 suites / 365 tests successfully against the exact referenced mobile commit.
- The strict catalog-list audit passed with no decision changes.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- `git diff --check` passed before the mobile commit.

## Scope and remaining evidence

This slice changes no API, database schema, authorization rule, experiment state, or release flag. It performs no live authentication, production mutation, customer communication, merge, deployment, or experiment activation.

The URL parser and auth continuation are covered through Jest fixtures and repository gates. Native binary validation of cold/warm links, VoiceOver/TalkBack behavior, and links delivered through a real OS/app association remains a physical-device gate and is not claimed here.
