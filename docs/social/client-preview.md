# Account-only client/API preview — inactive draft

Depends on #366 and the preceding draft stack. The additive contract is defined in
`tdf-hq/docs/openapi/social-v2.yaml`, referenced by the canonical `api.yaml`.
Generated TypeScript: `tdf-hq-ui/src/api/generated/social-v2.ts`, produced by:

```
./node_modules/.bin/openapi-typescript tdf-hq/docs/openapi/social-v2.yaml --output tdf-hq-ui/src/api/generated/social-v2.ts
```

`TDF.Social.API` strictly parses mutation inputs; caller-supplied actor identity is
rejected. `TDF.Social.Server` derives PartyId from authentication, checks
`SOCIAL_V2_ENABLED=true`, uses parameterized PostgreSQL calls and maps disabled,
invalid, conflict and rate-limit outcomes. Database runtime must ALSO be enabled.
No production migration, process flag, database gate or frontend flag was enabled.
Eight localhost HTTP integration tests now pass with actual bearer authentication,
Servant handlers and native PostgreSQL 16.10. Full-application qualification is broader.

`VITE_SOCIAL_V2_ENABLED=true` selects the experimental social page with Following
as the default, separate Discover, explicit consent controls, mute/block/dismiss,
profile discoverability and personalization opt-out. Query keys include the actor;
account switches remount the view. Cursor strings do not pass through JS numbers.
Existing `/artista/:id` and `/fans` links preserve established artist/interest flows.
The legacy page now generates vCard QR images locally instead of sending contacts
to a third-party QR endpoint; its query caches are also scoped per account.

## Actual tests and known limits

- Four Stack/runghc API parser/serialization tests passed.
- Fifteen Jest tests passed for API validation, default Following, empty/error
  recovery, opt-out and account cache isolation (three suites including existing API).
- Full web TypeScript check, Vite build and bundle budget passed after canonical
  contract regeneration and the reviewed upstream dependency refresh. Initial JS:
  357,190 bytes gzip across five preloads. Earlier baseline failures remain recorded
  separately; the upstream onboarding fixes are not attributed to this social PR.
- The actual SocialWorkspace component passed a local synthetic-session/API browser
  journey: default Following, keyboard tabs, explicit acceptance, no 390px horizontal
  overflow, no browser errors and zero violations in the selected axe WCAG rules.
  Desktop/mobile screenshots were captured and inspected. This is not full-app E2E.
- Eight real localhost HTTP examples passed: authentication, process/database gates,
  identity injection, caller-owned withdrawal, bilateral consent, blocked reads, cursor validation, membership
  revocation, inactive tokens and organization exclusion. Native PostgreSQL 16.10
  provided a fallback after Docker returned API 500 and its HTTP run stalled.
  GHC object-code compilation avoided the interpreter breakpoint-index limit.
  No successful mobile runtime is claimed. The mobile generated contract is synchronized in an isolated dependent draft; legacy contracts
  remain intact with flags off. Activation requires its explicit adaptation.
- Legacy endpoints/DM/notifications/media and managed-entity contexts are not yet
  consistently guarded by the new authority. There is no production-ready privacy
  cutover. Do not activate these controls in a shared environment.
- Discover returns relationship states with candidates, avoiding one HTTP request
  per candidate. SQL policy work is bounded to the daily 200-profile sample.
- The relationship list is limited to 50 recent pairs; full pagination, private
  follow requests, reporting controls and conversation/booking instrumentation
  remain incomplete. These are implementation work, not claimed delivered scope.

## Reproduction and recovery

Run the three Jest suites listed in the social-client workflow. Start the local
synthetic preview with `node scripts/social/preview.mjs`, then run
`node scripts/social/verify-preview.mjs`. The browser harness aborts requests to
non-local hosts. Run `bash scripts/social/test-http.sh` for real localhost HTTP,
authentication and PostgreSQL fixtures; it uses Stack object-code compilation and
never reads a production `.env`. Its result must be recorded separately.

A denied refetch hides previously cached content. Relationship mutations reset the
actor's social query cache before reloading current eligibility. These client
behaviors supplement server authorization; already delivered content cannot be
recalled from a device. Both flag examples are explicitly false. Legacy endpoints
remain available with the preview flag off; this does not make them compatible
with an activated block policy. All activation gates in the database document apply.

Native HTTP reproduction: `TDF_SOCIAL_HTTP_NATIVE=1 bash scripts/social/test-http.sh`.
Set `TDF_SOCIAL_PG_BIN` when PostgreSQL binaries are elsewhere; an optional
`TDF_SOCIAL_HTTP_BUILD` reuses compiler objects. The private cluster starts on a
free loopback port and is stopped on exit. Evidence: `evidence/http-refreshed-final.txt`.
The initial Docker-backed run is a failure/limitation, not a passing test result.
