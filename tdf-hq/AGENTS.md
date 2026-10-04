# Repository Guidelines

## Project Structure & Modules
- Entry point: `app/Main.hs` (starts Warp server, CORS, migrations).
- Core modules in `src/TDF/`: `API`, `Server`, `Config`, `DB`, `Models`, `DTO`, `Seed`.
- Config: `config/default.env` (copy to `.env` or `source` in shell).
- Dev script: `scripts/dev_run.sh` (exports env, builds, runs).

## Build, Run, and Dev
- Toolchain: **stack only** — `stack.yaml` uses `lts-24.42` (GHC 9.10.3). Do **not** use `cabal` or the system GHC; it is a different toolchain the project does not use, its `dist-newstyle/` artifacts are ignored, and a green `cabal` build does not imply a green project build.
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
- The existing `test/` Hspec/QuickCheck suite runs with `stack test`. PostgreSQL, HTTP and concurrency runners under root `scripts/` cover additional boundaries; see `.github/workflows/ci.yml` for the complete backend lane.
- Prefer observable handler/database tests with isolated fixtures. Follow `FORMAL_VERIFICATION.md` for critical invariants and negative controls.

## Commit & Pull Requests
- Commits: short, imperative subjects (e.g., "Enable CORS"). Optional prefixes like `feat:`, `fix:`, `chore:` are welcome.
- PRs must include: concise summary, rationale, how to run (`stack` steps), sample `curl` for new endpoints, and linked issues.
- Screenshots/logs helpful for behavior changes; note any migration or config impacts.

## Security & Configuration
- Do not commit secrets; use env vars (`config/default.env` as a template).
- Production CORS is fail-closed in `src/TDF/Cors.hs`; set an explicit origin allowlist and keep `ALLOW_ALL_ORIGINS=false`. See `formal/system/cors-boundary.md` at repository root.
- Seeding endpoint is for development only; remove/guard before release.

## Submodules & Backups
- `tdf-mobile/` is a Git submodule. When cloning or pulling, run `git submodule update --init --checkout --recursive` so the Expo app is available locally and for CI. Mobile CI uses an explicit checkout; web deployments on Cloudflare Pages and Vercel omit the mobile repository.
- UI snapshots such as `tdf-hq-ui.backup.*` are intentionally ignored in `.gitignore`. Treat them as personal sandboxes—never reference them from build scripts or CI.

## Deployment Runbooks
- **Cloudflare Pages** – build from repo root with `npm run build:ui`, output `tdf-hq-ui/dist`. Use the current Node 22+ package/CI requirement. Production's observed API is `https://api.tdfrecords.net`; verify browser `VITE_API_BASE` and preview-function `PUBLIC_API_BASE`. Never place bearer credentials in public `VITE_*` variables.
- **Vercel** – set the root directory to `tdf-hq-ui`, install via `npm install`, build with `npm run build`, output `dist`.
- **Backend** – start at `formal/system/README.md` and verify live identity. The October 4 baseline is Hetzner; Koyeb/Fly instructions are historical targets, not permission to redirect production. Preserve reviewed migrations and recovery guards.
- Whenever you need to test end-to-end, ensure the frontend env vars point at the deployed API and that the API allows the frontend’s origin.

## Branding
- The React shell renders SVG logos through `BrandLogo`. Swap SVG assets (`tdf-hq-ui/src/assets/tdf-*.svg`) rather than hardcoding text to maintain contrast in both themes.
- The TopBar uses the white “alt” wordmark; keep that variant for dark surfaces and reserve the glyph/isotype for light backgrounds or iconography.
