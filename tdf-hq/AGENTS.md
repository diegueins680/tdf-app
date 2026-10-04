# Repository Guidelines

## Project Structure & Modules
- Entry point: `app/Main.hs` (starts Warp server, CORS, migrations).
- Core modules in `src/TDF/`: `API`, `Server`, `Config`, `DB`, `Models`, `DTO`, `Seed`.
- Config: `config/default.env` (copy to `.env` or `source` in shell).
- Dev script: `scripts/dev_run.sh` (exports env, builds, runs).

## Build, Run, and Dev
- Toolchain: **stack only** — `stack.yaml` uses `lts-24.42` (GHC 9.10.3). Do **not** use `cabal` or the system GHC; it is a different toolchain the project does not use, its `dist-newstyle/` artifacts are ignored, and a green `cabal` build does not imply a green project build.
- Package/dependency/module authority: `tdf-hq.cabal`, consumed by Stack and Docker. The obsolete ignored `package.yaml` was removed; do not regenerate the Cabal manifest from historical Hpack metadata.
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
- Run the existing Hspec/QuickCheck suite with `stack test`; tests live in `test/`.
- Run relevant PostgreSQL integration and concurrency harnesses under `scripts/`;
  pending external-runner cases in Hspec are not a PostgreSQL test pass.
- Add focused invariant/regression coverage for changed behavior and use the
  repository's existing formal verification infrastructure for high-risk changes.

## Commit & Pull Requests
- Commits: short, imperative subjects (e.g., "Enable CORS"). Optional prefixes like `feat:`, `fix:`, `chore:` are welcome.
- PRs must include: concise summary, rationale, how to run (`stack` steps), sample `curl` for new endpoints, and linked issues.
- Screenshots/logs helpful for behavior changes; note any migration or config impacts.

## Security & Configuration
- Do not commit secrets; use env vars (`config/default.env` as a template).
- CORS is permissive for dev; restrict `corsOrigins` in `app/Main.hs` for production.
- Seeding endpoint is for development only; remove/guard before release.

## Submodules & Backups
- `tdf-mobile/` is a Git submodule. When cloning or pulling, run `git submodule update --init --checkout --recursive` so the Expo app is available locally and for CI. Mobile CI uses an explicit checkout; web deployments on Cloudflare Pages and Vercel omit the mobile repository.
- UI snapshots such as `tdf-hq-ui.backup.*` are intentionally ignored in `.gitignore`. Treat them as personal sandboxes—never reference them from build scripts or CI.

## Deployment Runbooks
- Follow `../ops/hetzner/README.md` and its validation record for the current
  production API at `https://api.tdfrecords.net`. Preserve the production database,
  backups, reviewed migration manifest and compatible recovery image.
- Cloudflare Pages builds from the repository root with Node 22 and
  `npm run build:ui`, output `tdf-hq-ui/dist`. Both `VITE_API_BASE` and
  `PUBLIC_API_BASE` must use the canonical API host; never put bearer credentials
  in public `VITE_*` configuration.
- The retained Fly runner and Koyeb references are historical/legacy scope, not
  the current production deployment procedure. Do not modify shared Trader resources.
- Inspect current rollout prerequisites and feature flags before deploying; a
  successful build does not establish production authentication or upload behavior.

## Branding
- The React shell renders SVG logos through `BrandLogo`. Swap SVG assets (`tdf-hq-ui/src/assets/tdf-*.svg`) rather than hardcoding text to maintain contrast in both themes.
- The TopBar uses the white “alt” wordmark; keep that variant for dark surfaces and reserve the glyph/isotype for light backgrounds or iconography.
