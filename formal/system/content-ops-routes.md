# Content and operations tooling mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/Server.hs` (CMS, seeding), `ServerExtra.hs` (pipelines, rooms, sessions), `ServerProposals.hs`, `Handlers/InputList.hs`, `EventOperations/Server.hs`, `Server/Reviews.hs`, `Seed.hs` and `Config.hs` on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Guards**
- CMS needs the Admin or Webmaster role; pipelines need ModuleScheduling plus a live `pipeline.*` grant; proposals need ModuleCRM only; rooms and sessions need ModuleScheduling; event operations re-validate the token inside the SQL command and authorize in SQL; reviews need any session plus SQL eligibility; the three seed routes are public under the operator secret.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| cms content create, publish, delete | CMS-CONTENT-ADMIN-001 | new |
| pipelines create, update, delete | PIPELINE-CARDS-001 | new |
| proposals create, update, versions | PROPOSAL-AUTHORING-001 | new (critical) |
| event-operations transitions, complete, raci reassign | EVT-OPERATIONS-COMMANDS-001 | new (critical) |
| seed, input-list inventory and sessions seed | OPS-SEED-ROUTES-001 | new |
| rooms create, patch; sessions create, patch | SCHED-ROOMS-SESSIONS-001 | new |
| reviews | REVIEW-PUBLIC-SUBMIT-001 | new |

## Defect fixed on 2026-11-07

**Proposal LaTeX was compiled with shell escape.** Proposals store LaTeX authored by any ModuleCRM holder verbatim, and `GET /proposals/{id}/pdf` compiled it with `tectonic -Z shell-escape` (and an `xelatex` fallback without `-no-shell-escape`), so a proposal could run shell commands on the server. The shipped templates only need `\includegraphics`. `tectonicArguments` drops the flag and `xelatexArguments` adds `-no-shell-escape`; tested in `ServerProposalsSpec`.

## Open behaviours for the owner to decide

**Seeding**
- **Decided 2026-11-10.** The three seed routes sit behind `AuthProtect` and need a strict administrator with the Admin module, like `POST /admin/seed`, before the seed token is read. Still open: the production guard relies on hosting-platform markers, so a self-hosted box without them can enable seeding, and no audit row records a seed run.

**Workflow**
- CMS publish does not check the row's current status, so an archived or already published row can be republished and two publishers can race.
- Pipeline card misses and invalid transitions collapse into 400; sessions have no room or engineer overlap check or status rules.
- Proposals are gated by the CRM module alone and create their first version in a second transaction.

**Concurrency**
- CMS versions, proposal versions and exact-name rooms race into unique constraints (500); deleting a pipeline card referenced by a proposal surfaces the foreign key as 500.
