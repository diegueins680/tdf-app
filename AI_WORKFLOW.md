# AI Workflow

This file is the current source of truth for working in this repo with Codex, Copilot, or any other coding agent.

## Start Here

Run this first from the repo root:

```bash
npm run ai:doctor
```

That preflight checks:

- required workspace memory files (`SOUL.md`, `USER.md`, recent `memory/*.md`)
- core CLI availability (`codex`, `gh`, `git`)
- GitHub auth state when CI polling is enabled
- current git branch and worktree cleanliness
- risky unattended loop settings, especially pushing directly to `main`

## Canonical Sources

- Personal/session behavior: `AGENTS.md`, `SOUL.md`, `USER.md`
- Backend contract used by generated clients: `tdf-hq/docs/openapi/api.yaml`
- Generated client outputs:
  - `tdf-hq-ui/src/api/generated/types.ts`
  - `tdf-mobile/src/api/generated/types.ts`
- Continuous loop entrypoint: `scripts/continuous-improvement-loop.mjs`
- Default unattended loop config: `scripts/continuous-improvement-loop.codex.json`

Do not hand-edit generated API client files.

## Recommended Day-to-Day Flow

1. Create or switch to a feature branch.
2. Run `npm run ai:doctor`.
3. Make the smallest defensible change.
4. Run only the relevant verification:
   - Backend: `cd tdf-hq && stack test && stack build`
   - Web UI: `npm run test:ui && npm run build:ui`
   - Mobile: `npm run test:mobile && npm run typecheck:mobile`
5. If API shapes changed, update `tdf-hq/docs/openapi/api.yaml` and regenerate clients:
   - `npm run generate:api:ui`
   - `npm run generate:api:mobile`
6. Update docs when the workflow or contract changed.

## Continuous Improvement Loop

The loop is useful, but pushing to `main` is now blocked by default:

- the default config inherits your current branch
- the default unattended config keeps `pollGitHub` off until auth is healthy and you explicitly opt in
- `loop:start`, the supervisor, and direct `node scripts/continuous-improvement-loop.mjs` runs all refuse `main` unless you opt in
- the loop can still checkpoint dirty worktrees and push fixes after CI repair, so branch choice still matters

Useful toggles:

```bash
npm run loop:ci-polling-status
npm run loop:enable-ci-polling
npm run loop:disable-ci-polling
```

For unattended work, prefer a branch-scoped config copied from the default:

```json
{
  "pushRemote": "origin",
  "pushBranch": "codex/your-branch",
  "pollGitHub": true,
  "ciRepairCommand": "./scripts/codex-loop-worker.sh ci-repair {ci_report_file}"
}
```

Recommended guardrails:

- Never point unattended experiments at `main` until the loop behavior is explicitly what you want.
- Turn `pollGitHub` on only when `gh auth` is healthy or you intentionally provide a valid token.
- If you intentionally want `main`, set `"allowPushToMain": true` in the config or export `CONTINUOUS_LOOP_ALLOW_PUSH_TO_MAIN=1`.
- Review `tmp/continuous-improvement-loop/status.json` and `tmp/continuous-improvement-loop.log`.
- Only use `--allow-dirty` when you intentionally want to preserve an existing baseline.

## Known Repo-Specific Traps

- Some older docs still implied the backend OpenAPI was auto-generated. For client generation in this repo, `tdf-hq/docs/openapi/api.yaml` is the spec that matters.
- Older agent docs also claimed the backend or mobile lacked automated tests. That is stale: both exist now.
- `tdf-mobile/` is a submodule. If it is missing or incomplete, root mobile scripts will skip unless `REQUIRE_MOBILE_WORKSPACE=1`.
- `gh auth` can be shadowed by stale `GH_TOKEN` / `GITHUB_TOKEN` / `GITHUB_PAT` values; re-auth or clear them if polling looks broken.

## When Updating AI Tooling

If you change prompts, agent docs, or loop behavior:

1. Update this file first.
2. Update `.github/copilot-instructions.md` if the change affects Copilot context.
3. Keep `README.md`, `DEVELOPMENT.md`, and `CONTRIBUTING.md` aligned.
4. Re-run `npm run ai:doctor`.
