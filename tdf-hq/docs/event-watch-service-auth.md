# Event watch: isolated service authentication

This lane lets an unattended event researcher save candidates and materialize
eligible third-party events without an interactive login or a general Admin role.
It does not enable discovery, approve the research pilot, sell tickets, send
messages, modify payments, or grant access to other application routes.

## Deployment and provisioning

1. Deploy the reviewed migration `2026-10-21_event_watch_service_auth` through
   the canonical Hetzner migration/release lane. Keep `RUN_MIGRATIONS=false`.
2. Use a dedicated Party named **Agenda de terceros — TDF** for audit/record
   ownership, without a password, API token or security-role assignment. This is
   the curator account, not a claim that TDF produces the events. Include the
   real promoter and original announcement URL in candidate descriptions.
3. On a trusted operator machine with a private PostgreSQL connection, run:

   ```sh
   node scripts/event-watch-client.mjs provision quito-watch PARTY_ID /private/event-watch.json --allow-publish
   ```

   The tool refuses to overwrite a file, creates it with mode 0600, stores only
   the SHA-256 hash in PostgreSQL, and prints no secret. The secret is 32 random
   bytes. Expiry is 90 days; rerunning provisioning with a **new output path**
   rotates the credential and invalidates all sessions. Without
   `--allow-publish`, materialization is restricted to drafts.
4. Transfer the private JSON to the scheduled job's secret store. Never put it
   in chat, prompts, Git, public `VITE_*` variables, command-line arguments or
   logs. The execution environment must actually support private secret delivery;
   provisioning alone does not connect ChatGPT scheduled tasks to that store.
5. Approve the existing research pilot through the interactive administrative
   lane if it is not already approved. Service credentials cannot approve it.

## Request contract

`POST /integrations/event-watch/token` accepts JSON:
`{"grant_type":"client_credentials","client_id":"quito-watch","client_secret":"…"}`.
The JSON response has `access_token`, `token_type: "Bearer"`, `expires_in` (at
most 900 seconds), and `scope`. This is a purpose-specific JSON credential
exchange, **not an implementation of the OAuth 2.0 token endpoint**.

Send `Authorization: Bearer …` to:

- `GET /integrations/event-watch/candidates`: up to 500 of this client's
  candidates, newest verification first, including existing materialized IDs.
- `PUT /integrations/event-watch/import`: JSON `{runKey, candidate, materialize,
  publish}`. `candidate` has the existing `EventResearchCandidateWriteDTO`
  contract. `runKey` is a stable ASCII job identifier, at most 80 characters.
  Provider, run ID and discovery-source ID in the submitted candidate are
  overridden by the server, so a client cannot select another importer's data.
  Candidate external ID must be stable across runs: reuse the official event ID
  or canonical source URL, never a random ID or ticket tier.

The result contains `candidateRecord` and `eventLink` (null for candidate-only
imports). A published import requires both `materialize=true` and `publish=true`
and an operator-issued publication grant. Country must be EC. Evidence,
confidence, lifecycle, artist/venue identity, image permissions, source status,
manual visibility and deduplication use the existing research checks. Unknown
end times remain null. Do not invent start times, venues, lineups or coordinates
to satisfy publication; save incomplete items as review candidates instead.
Past/cancelled events retain their source status and must not be presented as
upcoming. The existing materializer preserves manual edits on replay; this lane
does not add automatic arbitrary edits of already materialized events.

Candidate and event/audit effects commit together. If materialization fails,
the whole import rolls back; retry with `materialize=false` to retain the research
candidate for review. Same candidate identity and payload replay through the
existing dedupe and audit logic. A run is a durable open ingestion batch; this
minimal service lane does not expose general run administration or pilot approval.

## Revocation and failure handling

```sh
node scripts/event-watch-client.mjs revoke quito-watch
```

Changing the client secret, actor, expiry, active flag or publication permission
deletes outstanding sessions transactionally. An admitted request holds client
and session locks until commit; revocation waits for an already admitted
transaction, then rejects subsequent requests. Expiry is checked again before
commit, including after lock waits. Tokens exist only as hashes in the dedicated
table and never enter `api_token`. Cookies and interactive sessions cannot access
this lane. At most four recent sessions are retained for each client.

401 means missing/invalid/expired credentials; 403 means publication or country
scope denied; 400 means invalid input; 409 means existing research publication
or identity blockers; 503 means unavailable schema/database. Errors exclude SQL
and secret values. Successful credential/data responses have `Cache-Control:
no-store` and `Pragma: no-cache`. Use HTTPS and an edge rate limit on the token
endpoint; rate limiting is an operational prerequisite, not implemented here.

## Verification limits

Run `node --test scripts/event-watch-client.test.mjs`, the focused Hspec
`isolated event-watch credentials`, and
`tdf-hq/test/sql/event_watch_service_auth.sql` against a disposable migrated
PostgreSQL database. The SQL fixture verifies hash constraints, foreign keys,
expiry bounds and revocation on rotation/permission changes. A real concurrent
HTTP/database test and deployed job secret delivery are separate release gates;
unit properties do not establish those boundaries.
