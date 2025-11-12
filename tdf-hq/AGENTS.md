# Repository Guidelines

## Project Structure & Modules
- Entry point: `app/Main.hs` (starts Warp server, CORS, migrations).
- Core modules in `src/TDF/`: `API`, `Server`, `Config`, `DB`, `Models`, `DTO`, `Seed`.
- Config: `config/default.env` (copy to `.env` or `source` in shell).
- Dev script: `scripts/dev_run.sh` (exports env, builds, runs).

## Build, Run, and Dev
- Env: `set -a; source config/default.env; set +a`.
- Build: `stack setup` then `stack build`.
- Run: `stack run` (or `bash scripts/dev_run.sh`).
- Seed sample data (dev only): `curl -X POST http://localhost:8080/admin/seed`.
- REPL: `stack ghci` to load modules interactively.

## Coding Style & Naming
- Haskell2010; warnings enabled via `-Wall` (see `tdf-hq.cabal`).
- Indentation: 2 spaces, no tabs; keep lines ≤ 100 cols.
- Modules: `TDF.*` hierarchy mirrors directories (e.g., `src/TDF/Server.hs`).
- Names: Types/Constructors `UpperCamelCase`, functions/vars `lowerCamelCase`.
- Pattern for new endpoints: update `TDF.API` type, implement handlers in `TDF.Server`, DTOs in `TDF.DTO`, DB logic in `TDF.DB`/`TDF.Models`.

## Testing Guidelines
- No test suite yet. If adding tests:
  - Create `test/` and use Hspec; name files `*.Spec.hs`.
  - Add a `test-suite` to `tdf-hq.cabal`; run with `stack test`.
  - Prefer unit tests for handlers and DB queries; use factories/fixtures over ad‑hoc data.

## Commit & Pull Requests
- Commits: short, imperative subjects (e.g., "Enable CORS"). Optional prefixes like `feat:`, `fix:`, `chore:` are welcome.
- PRs must include: concise summary, rationale, how to run (`stack` steps), sample `curl` for new endpoints, and linked issues.
- Screenshots/logs helpful for behavior changes; note any migration or config impacts.

## Security & Configuration
- Do not commit secrets; use env vars (`config/default.env` as a template).
- CORS is permissive for dev; restrict `corsOrigins` in `app/Main.hs` for production.
- Seeding endpoint is for development only; remove/guard before release.

## Submodules & Backups
- `tdf-mobile/` is a Git submodule. When cloning or pulling, run `git submodule update --init --recursive` so the Expo app is available locally and for CI. Deployments that inspect the tree (Cloudflare Pages, Vercel) will fail if the submodule isn’t initialized.
- UI snapshots such as `tdf-hq-ui.backup.*` are intentionally ignored in `.gitignore`. Treat them as personal sandboxes—never reference them from build scripts or CI.

## Deployment Runbooks
- **Cloudflare Pages** – build from repo root with `npm run build:ui`, output `tdf-hq-ui/dist`. Set `NODE_VERSION=18`, `VITE_API_BASE=https://the-dream-factory.koyeb.app`, `VITE_TZ=America/Guayaquil`, and optional `VITE_API_DEMO_TOKEN`.
- **Vercel** – set the root directory to `tdf-hq-ui`, install via `npm install`, build with `npm run build`, output `dist`.
- **Koyeb (API)** – configure all `DB_*`, `SMTP_*`, `HQ_APP_URL`, and CORS vars (`ALLOW_ORIGINS`, `ALLOW_ALL_ORIGINS`). Without the CORS envs Cloudflare/Vercel frontends cannot talk to the API.
- Whenever you need to test end-to-end, ensure the frontend env vars point at the deployed API and that the API allows the frontend’s origin.

## Branding
- The React shell renders SVG logos through `BrandLogo`. Swap SVG assets (`tdf-hq-ui/src/assets/tdf-*.svg`) rather than hardcoding text to maintain contrast in both themes.
- The TopBar uses the white “alt” wordmark; keep that variant for dark surfaces and reserve the glyph/isotype for light backgrounds or iconography.
