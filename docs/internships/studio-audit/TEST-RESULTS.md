# Verification record

This file records observed commands and results through 2026-08-24. A failed, blocked, pending, or unexecuted check is never represented as passing.

## Passed repository checks

| Check | Command | Observed result |
| --- | --- | --- |
| Baseline isolation | `git rev-parse HEAD`; `git status --short` in the primary checkout and isolated worktree | Pass: work began from `dac84b099b18b51032fb94f58273120f5375eb85`, confirmed as current `origin/main` immediately before branching; unrelated primary-checkout changes remained untouched |
| Identity search | Production PostgreSQL `BEGIN TRANSACTION READ ONLY` queries with a 10-second statement timeout across exact text matches and the bounded intern population | Pass: the requested “Stuart” resolves uniquely by active profile, recent time activity, and existing named internship project to Stewart Moreira; the spelling difference is recorded, and no production row was changed |
| GitHub duplicate inspection | Read-only exact-title and related issue/PR searches | No matching current issue or PR at inspection time; repeat immediately before any authorized mutation |
| Inventory and cases | `npm run generate:studio-internship-audit` | Pass: 130 inventory entries, 125 applicable features, 174 cases, 14 exploratory charters, 107 strong-evidence cases, estimated 23.4 hours |
| Inventory safety unit tests | `node --test scripts/__tests__/studio-internship-audit.test.mjs` | Pass: 5 tests, including exact-host credential isolation for the draft-preparation script |
| Draft preview | `node scripts/prepare-studio-audit-draft.mjs preview` | Pass: verified runtime-only Stewart identity displayed; exact production identifiers omitted from source control; no API mutation, activation, assignment, or notification |
| Script/static checks | `git diff --check`; `node --check` on four new JavaScript modules; `sh -n` on both new shell scripts | Pass |
| Repository quality | `bash scripts/quality-repo.sh` | Pass: deterministic audit generation/unit checks; formal audit with 0 critical, 0 errors, 309 warnings, and 8,464 info findings; improvement-loop 41 tests; formal-method 4 tests; production-release 43 tests; CI 16 tests; visual-artifact 2 tests; persona-program 3 tests |
| Reversible migration | `sh scripts/test-studio-internship-audit-migration.sh` using its disposable local PostgreSQL mode | Pass: apply/reapply, legacy compatibility, external-link evidence constraint, progress and traceability triggers, completed-plan trigger ordering, draft-report/pending-retest/stale-summary completion gates, blocker/history checks, rollback, and disposable cleanup |
| Backend compile | `TMPDIR=/private/tmp/tdf-studio-audit-ghc stack test --fast --no-run-tests --jobs 1` in `tdf-hq` | Pass |
| Backend targeted validation | Targeted Hspec execution-status and internal-feedback validation matches | Pass: 4 execution/completion-validation and 6 feedback/CSV-validation examples |
| Catalog authority audit | `npm run audit:catalog-lists`; `npm run test:catalog-list-audit` | Pass on the final integrated tree: 917 reviewed candidates, 0 unreviewed candidates, 0 stale decisions, and 1 audit unit test; internal report types are mapped to the persisted `feedback_category` authority |
| Feedback category/type adapter | Direct targeted Hspec execution of `internal feedback workflow validation` | Pass: 6 examples, including persisted category-code mapping, mismatch rejection, and spreadsheet-formula neutralization |
| Backend full suite | `TMPDIR=/private/tmp/tdf-studio-audit-ghc stack test --fast --jobs 1` in `tdf-hq` | Pass: 2,426 examples, 0 failures |
| API lifecycle E2E | `TDF_AUDIT_E2E_BACKEND_EXE=<compiled-test-binary> TDF_AUDIT_E2E_PASSWORD=<runtime-only> sh scripts/test-studio-internship-audit-api-e2e.sh` | Pass again on the final review-fix tree in disposable local PostgreSQL: synthetic Manager, assigned Intern, and unrelated Intern; non-pristine task conversion rejection; inactive draft visibility and activation; protected audit-task lifecycle; atomic numbering for two concurrent execution requests; post-submission blocker authority; assigned-only initial final summary; UI-style retest with automatic immutable execution creation; category/type mismatch rejection; evidence/history; private report CRUD; duplicate warning; admin triage/info/retest/close; daily/final/calculated completion while a sibling task keeps the project active; export; reporter and immediate team in-app notifications; undispatched test outbox; public feedback; legacy feedback readability; cleanup |
| Web type checking | `npm run typecheck --workspace=tdf-hq-ui` | Pass |
| Web audit API units | `npm test --workspace=tdf-hq-ui -- --runInBand src/api/studioAudit.test.ts` | Pass: 4 tests |
| Existing task-detail regression | `npm test --workspace=tdf-hq-ui -- --runInBand src/pages/InternTaskDetailPage.test.tsx` | Pass: 7 tests, including intern/admin enforcement that audit-task status and calculated progress remain controlled by the audit plan |
| Web full Jest suite | `npm test --workspace=tdf-hq-ui -- --runInBand --silent` | Pass: 163 suites, 1,656 tests, 0 failures |
| Web E2E and accessibility | Targeted Chromium Playwright run for `e2e/web/studio-internship-audit.spec.mjs` | Pass: 2 scenarios, covering intern trace-linked reporting and admin triage; no serious or critical axe violations in the tested views |
| OpenAPI clients | `npm run generate:api --workspace=tdf-hq-ui`; `npm --prefix tdf-mobile run generate:api` | Pass with `openapi-typescript 7.13.0` |
| Feature registry | `npm run generate:features`; `npm run audit:features` | Pass: 139 registered features, 159 web routes, and 44 mobile routes |
| Mobile type checking | `npm --prefix tdf-mobile run typecheck` | Pass |
| Mobile lint | `npm --prefix tdf-mobile run lint` | Pass |
| Mobile registry units | `npm --prefix tdf-mobile test -- --runInBand __tests__/featureRegistry.test.ts` | Pass: 10 tests |
| Mobile full Jest suite | `npm --prefix tdf-mobile test -- --runInBand --silent` | Pass: 49 suites, 256 tests, 0 failures |

## Repository-wide checks that did not pass

| Check | Command | Observed result |
| --- | --- | --- |
| Web production build | `npm run build --workspace=tdf-hq-ui` | TypeScript and Vite compilation passed (`12,299` modules; Vite completed in 22.47 s), then the initial-bundle budget check failed: 432,420 gzip bytes versus 419,840 allowed, 12,580 bytes over |
| Web full lint | `npm run lint --workspace=tdf-hq-ui` | Failed: 129 findings (22 errors, 107 warnings). The 22 errors are in pre-existing files outside this assignment; targeted lint on assignment files had 0 errors and 14 warnings |
| Feature-audit report gate | `npm run generate:feature-audit-reports` | Failed because 40 pre-existing backend endpoints remain pending disposition. The new internship-audit/internal-feedback endpoints are registered and mapped |
| First full web Jest attempt | `npm test --workspace=tdf-hq-ui -- --runInBand` | Environment failure: the isolated worktree lacked workspace-local React links; 75 suites passed, 88 could not resolve `react/jsx-runtime`, and 457 tests passed. Dependency links were repaired without source changes |
| Second full web Jest attempt | Same command after dependency repair | One assignment-adjacent mock failure remained; 162 suites passed, 1 suite failed to import `getStoredSessionToken`, and 1,651 tests passed. The mock was corrected and the final full rerun above passed |
| Docker migration attempt | `sh scripts/test-studio-internship-audit-migration.sh` in its Docker path | Docker daemon unavailable. The script's disposable local-PostgreSQL path subsequently passed |

`npm install` used to repair the local dependency environment also reported the repository's current audit state of 11 dependency vulnerabilities (6 moderate, 5 high); no vulnerability is claimed resolved by this assignment.

## External and approval-gated verification

| Check | Status |
| --- | --- |
| Deploy to a dedicated staging tenant with private evidence storage and provider test transports | Not authorized; a reviewed isolated configuration is committed, but deployment remains a manual external gate |
| Exercise real Datafast/PayPal sandbox credentials and email/WhatsApp/calendar/social sinks | Credentials/configuration not supplied; contract isolation is implemented, but provider verification remains a manual gate |
| Validate the asynchronous external outbox dispatcher/digest schedule | No dispatcher for the new outbox exists in the inspected baseline; reporter/immediate-team in-app notifications and outbox creation are tested, while grouped digest/external delivery remains a deployment task |
| Create an inactive in-app draft associated with Stewart | Not authorized; blocked by undeployed schema/API plus explicit in-app draft-creation approval and an authorized runtime token |
| Run with Stewart-equivalent access in staging | Not authorized; production credentials and personal data must not be imported |
| Real backend-backed Spanish/English and responsive-device smoke in deployed staging | Not authorized; mocked Chromium audit views were exercised in Spanish, while deployment-backed bilingual/device coverage remains a manual staging gate |
| Pull request | Pass: [PR #200](https://github.com/diegueins680/tdf-app/pull/200) is open and ready for review |
| GitHub issue | Not authorized and not performed |
| Root branch/stage/commit | `git switch -c codex/studio-internship-audit`; explicit path staging; `git commit -m "feat: add traceable studio internship audit"` | Pass: feature commit `d68b794e531501589e18c24dbb60aa00ada0a0f7`; only assignment files staged; push evidence is reported in the final handoff |
| Publish regenerated `tdf-mobile` submodule files | Integrated mobile pull request #30; generated API/registry verification; tracked push | Pass: remote mobile main commit `c537c729177f92f50c69a47edba5b5dae46afebd`; parent pointer included in this final integration |

The API E2E uses an Intern account equivalent in role and permissions to Stewart and an authorized administrative account, but it is deliberately synthetic. It does not satisfy the real-staging deployment gate.
