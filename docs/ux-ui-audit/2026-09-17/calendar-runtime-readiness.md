# Calendar runtime readiness — UX-260917-040/041

The original tablet audit found500 on both existing calendar read endpoints.
A read-only production query confirms both Persistent calendar tables are absent;
no production tokens, events or account data were inspected. Migration introduction
`49ff02de5ade1b8923f099c22af0bbf57350a078` must remain in merge ancestry. Register
it as an additive migration, preserving existing rows and the model's VARCHAR JSON
encoding. Attach the existing archived-owner guard to the newly created relation.
The release schema gate checks columns, unique keys, FK and guard. Do not rewrite
older applied migrations or run automatic Persistent changes against production.

A new isolated PostgreSQL17 database rehearsed the entire production migration
baseline, automatic startup, idempotent second application and restart. Actual HTTP
checks assert401/403, empty200, populated config/event filtering, omission of access
and refresh tokens, and unchanged12synthetic events and config after two repeats.
The existing public-data candidate binary is used: calendar handlers did not change.
Three rollback-contained schema mutations prove the release gate rejects missing
columns, a disabled archive guard and a missing idempotency key.58release tests pass.

Six actual browser cases cover Admin/ReadOnly at320/768/1280px. The admin must reach
`Sin eventos sincronizados para este calendario.`; denial remains explicit. Initial
preview setup omitted its new loopback origin and signup was correctly denied;
qualified runs use the configured local reverse proxy. This is not a production
CORS or external Google OAuth certification.

The same journey with only calendar preference storage denied reproduced a general
error boundary. All local preference operations now tolerate denial/quota failures.
Four React tests exercise getter/getItem/setItem/removeItem; three actual browser
engines pass with the corrected production bundle and actual isolated API. The
first component harness lacked Vite's import.meta.env; that failure is not claimed
as the meaningful negative control. The original browser failure is retained.
[Versioned evidence](evidence/calendar-runtime-verification.json).

No production mutation, OAuth exchange, external calendar synchronization or real
credential operation was performed. Full2587backend examples (0failures,3existing
pending) passed on unchanged calendar handlers in the public-data candidate. Schema
and browser evidence are separate from full source/store verification. Hosted gates,
independent review, merge and guarded production release remain pending.

Recovery preserves this additive schema and any subsequently saved tokens/events;
never drop the new relations to recover an application binary. Use the existing
reviewed compatible recovery mechanism, pre-release snapshot, immutable artifact,
lease/canary and post-release service checks. Existing auth binaries already declare
these tables, so no new permission or client contract is introduced. Google consent,
provider availability and manual assistive-technology checks remain separate limits.
