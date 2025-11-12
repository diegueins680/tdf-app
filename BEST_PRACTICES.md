# Engineering Best Practices

This repo now ships with codified guardrails for both the TypeScript frontends and the Haskell backend. The commands below should run cleanly before sending code for review.

## TypeScript / React (tdf-hq-ui)

- **Strict compiler settings** – `tsconfig.json` enables `strict`, `noImplicitReturns`, `noFallthroughCasesInSwitch`, `forceConsistentCasingInFileNames`, etc. Run `npm run typecheck --workspace=tdf-hq-ui` to ensure there are no type regressions.
- **Defensive typing for dynamic access** – `noUncheckedIndexedAccess`, `noImplicitOverride`, `noPropertyAccessFromIndexSignature`, and `useUnknownInCatchVariables` are enabled so React pages must guard optional map lookups, re-export overrides with `override`, and treat caught errors as `unknown`. This protects components such as the Kanban board from stale state writes or missing columns.
- **Vite env access** – Because `import.meta.env` is an index signature, always read entries with bracket notation (`import.meta.env['VITE_TZ']`) to satisfy the stricter compiler rules.
- **ESLint stack** – `.eslintrc.cjs` enforces React, accessibility, and TypeScript rules (type-only imports, exhaustive deps, consistent auth header handling, etc.). Run `npm run lint --workspace=tdf-hq-ui` locally or in CI.
- **HTTP client hygiene** – API layers normalize Bearer tokens, bubble up server error bodies, and avoid unsafe fallback logic by preferring `??` over `||`.
- **React Query data safety** – Query hooks are typed and their results memoized before being consumed, keeping `noImplicitAny` and the runtime stable.
- **Session + UI state** – Session utilities expose safe helpers (`getStoredSessionToken`, `setApiToken`) and UI components avoid async handlers where void callbacks are expected (wrap async calls in `void fn()` wrappers).

## Haskell backend (tdf-hq)

- **Compiler warnings** – Both the executable and the test suite compile with `-Wall -Wcompat -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints`. Treat warning-free builds as a quality gate.
- **Keep imports explicit** – Modules import only the functions they need, which keeps `-Wmissing-import-lists` eligible if we decide to flip it on later and makes future refactors less risky.
- **Test harness** – `stack test` now runs an `hspec` suite (`test/Spec.hs`). Add real examples there as you touch backend code.
- **Stack usage** – Run `stack test` (or `stack build`) from `tdf-hq/` before pushing backend changes. If you use Nix, keep Stack’s nix integration disabled or configure it explicitly.
- **Email delivery** – Configure `SMTP_HOST`, `SMTP_PORT`, `SMTP_USERNAME`, `SMTP_PASSWORD`, `SMTP_FROM`, and optionally `SMTP_FROM_NAME`/`SMTP_TLS` so welcome emails go out when admins create new accounts. Set `HQ_APP_URL` to include a login link in those emails.
- **CORS configuration** – Production instances read `ALLOW_ORIGINS`, `ALLOWED_ORIGINS`, or `CORS_ALLOW_ORIGINS` (plus the `*_ALL` variants). Always set these in Koyeb/Render so Cloudflare/Vercel deployments can reach the API.

## Submodules, Assets, and Backups

- **Submodule init** – `tdf-mobile/` is a Git submodule. Run `git submodule update --init --recursive` after cloning or pulling commits that touch the mobile app. Pushes from CI/Cloudflare will fail if the submodule isn’t initialized.
- **UI backups** – Any `tdf-hq-ui.backup.*` directories are developer snapshots and intentionally ignored by `.gitignore`. Never reference them from production builds.
- **Brand assets** – The React shell uses the `BrandLogo` component with SVG glyphs (`src/assets/tdf-*.svg`). When updating the wordmark, swap the SVGs rather than re-introducing inline text so dark/light themes stay legible.

## Suggested Workflow

1. Edit code in the relevant workspace (`tdf-hq-ui/` for React, `tdf-hq/` for Haskell).
2. Run the consolidated guardrail script from the repo root: `npm run quality`. It runs ESLint, TypeScript `--noEmit`, and `stack test` so both ecosystems stay green together. (If Stack is not configured on your workstation, run `npm run lint:ui`, `npm run typecheck:ui`, and `stack test` separately.)
3. Investigate and fix any failures (remember that the stricter TS compiler options generally mean you need better null checks instead of suppressing errors).
4. Commit only after both linters/compilers pass to keep `feature-recovery` evergreen.

Keeping these guardrails in place means we detect regressions early and ship predictable builds across the web UI, mobile clients, and the HQ API.
