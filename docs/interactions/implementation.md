# Universal interactions — implementation and acceptance

Status: implementation in progress in isolated worktrees; no production schema or feature gate changed.
User authorization (2026-09-28): implement, test, review, merge, deploy and verify
universal interactions across web/mobile. This supersedes historical draft-only
instructions in the old social design packets, but not release protections.

## Observed baseline

Root main `7e7106b36e7ac427711c2f64af650527d619f9ce`; backend production
`cc244b1f86603055997b51379b297baebfd3e7ce`; mobile main `2a0e5a9`.
API: Hetzner / PostgreSQL 17.8, reviewed SQL migrations (115), Persistent boot
migration disabled. Web: Cloudflare Pages. Old TDF Fly API stopped/cordoned;
shared Fly database remains for Trader and must not be changed by this project.
Production inspection used read-only transactions and schema/aggregate metadata;
no private discussion bodies or notification text was exported.

| Existing implementation | Current authority / reuse decision |
| --- | --- |
| FanClubPost, parentId replies; ServerFanClub | Preserve posts, IDs, ownership and club permission. Replies become canonical discussion comments with stable legacy mappings; existing endpoints become adapters. |
| fan_club_post_reaction, fan_club_memory_reaction | One slot per actor/target already. Consolidate storage and retain compatibility adapters and migration journal. No new parallel like table. |
| EventMomentReaction, EventMomentComment | Preserve moment/comment IDs and first-value evidence. Adapt legacy operations to shared rules; cross-event parent and identity validation required. |
| content_reaction_type vs reaction_type catalogs | Reuse catalog authority; explicit mappings for existing types. Four default selectable types; retain historical extra reaction identities. |
| Social v2 pair/preferences/session/relationship SQL + Haskell | Already reviewed account/block/consent boundary; absent from live DB. Stage all compatibility prerequisites together before new block controls; never grant consent from legacy follow rows. |
| PartyFollow, FanFollow, ArtistFollow, club membership/officers | Distinct semantics. Reuse each only for its actual ownership/access/subscription contract. |
| Directory profile blocks, moderation, reviews, classifieds | Compose canonical profile visibility/blocking. Reviews are transaction-backed reputation, not generic comments; preserve review authority separately. |
| notification + target_key, generated web/native resolver | Existing inbox, unread state, recipient ownership and route generator are canonical. Add typed discussion destinations and aggregated events here. |
| PartySelector / UserSelector, /parties/search | Reuse presentation/search infrastructure with a target-scoped mention context; never expose CRM-only candidate fields to public discussions. |
| EngagementEvent and PostHog conventions | Interaction analytics use identifiers/actions only, no comment body or mention text. |
| OperationsMention; internal feedback comments/history | Operational assignment and internal case notes, not public discussion. Keep scoped existing authorities. |
| SocialSyncPost external counts | Provider metrics are external observations, not local reactions. Never import aggregate numbers as local user engagement. |
| Existing polling/query invalidation, Expo router | Reuse bounded query refresh and native navigation; no new websocket service. |

Live estimates from pg_stat_user_tables: both fan reaction tables, fan posts,
event moment reactions/comments, directory blocks and synced posts currently zero;
notification approximately 30, PartyFollow 116, FanFollow 52, engagement events 5.
These estimates are not migration proof: exact counts, references, checksums and
concurrent-write fencing must be captured during the final rehearsal/cutover.
Social v2 tables are absent, although their code is present. Do not mistake a
passing prototype fixture for production rollout.

## Canonical decisions

- Explicit target-kind registry plus trusted domain adapters. Entity existence,
  visibility, live ownership and capability are checked server-side on every
  operation. Unknown kinds/system records default denied. Target registration is
  a server concern, never an arbitrary client-owned resource.
- Eligible standalone entities: club posts/memories, event moments, published
  events, recordings/videos/audio, recording sessions, record and artist releases,
  classified ads/opportunities, published directory profiles where appropriate.
  Artist/label/venue updates use their existing post/media authorities. Attached
  photos/video/audio share their containing publication's discussion unless they
  have an independently persisted publication identity. No duplicate media threads.
- One active reaction slot per actor/target, persisted catalog type. Defaults:
  like, heart, fire, clap. Changes/removal are desired-state operations, not unsafe
  retryable toggles. Old toggle endpoints preserve their documented compatibility.
- One comment model: stable UUID, immutable target/parent/root, revision, client
  request identity, tombstone/moderation state, plain text plus stable mention
  spans, extensible attachment relation. Replies stay flat under their root in
  presentation beyond two visual levels. Parent deletion never deletes replies.
- Keyset pages for roots/replies/reactors; separate authorized target-comment
  resolution for deep links. Newest is the default until meaningful relevance
  signals exist. Hidden/blocked rows are filtered before pagination and counts.
- Comment policy is everyone/followers/mentioned/off; composes with entity access,
  current sessions, account state, bilateral blocks and explicit moderation scopes.
  An owner may hide a comment on their content but cannot edit it or gain admin
  removal rights. Author deletion, owner hide, report, block and moderator removal
  are separate audited transitions.
- Existing notification rows remain canonical. Aggregate reactions by recipient,
  target and time bucket; dedupe comment/reply/mention recipients. Revalidate
  current visibility/block/preferences when listing/delivering. No message bodies
  in analytics; no new outbound email/push sender implied by this task.
- Cache entries never grant rights. Reads are authorized at their database
  snapshot; writes serialize with revocation/block changes. Idempotency replay
  checks current authority and rejects reuse with a different payload.
- Additive migration with legacy identity mapping and retained source evidence.
  Rollback after new writes pauses features while preserving data and new privacy
  enforcement; it cannot route users to legacy paths that ignore blocks.

## Delivery checklist

- [x] Isolated root/mobile worktrees; inspect live architecture and schema.
- [ ] Complete source/caller inventory and adapter authorization matrix.
- [ ] Canonical schema, legacy conversion and rollback rehearsal.
- [ ] Backend operations, session/permissions, moderation and abuse controls.
- [ ] Shared web/native components and every eligible existing entry point.
- [ ] Mentions, notification aggregation/preferences, exact deep-link resolution.
- [ ] Unit/property/model/database/concurrency/migration coverage.
- [ ] Desktop/mobile accessibility and end-to-end workflows.
- [ ] Large synthetic discussion EXPLAIN/query-budget evidence.
- [ ] OpenAPI/generated contracts, documentation, full relevant local checks.
- [ ] Independent repository review, protected green CI, merge and immutable build.
- [ ] Backup/guarded migration/recovery drill/deployment/live verification.

## Working verification checkpoint (2026-09-28)

- Additive schema, live entity adapters, desired-state commands, keyset discussion
  reads, typed deep-link resolution, scoped mention search, canonical pair blocks,
  owner/admin moderation boundaries and queued existing-inbox delivery implemented
  behind a disabled runtime gate. Legacy conversion/adapters remain unfinished.
- Fresh local PostgreSQL rehearsal applies the existing 115 production migrations,
  all eight social authority compatibility prerequisites, and the new interactions
  migrations. Policy, command, notification and navigation SQL property scripts
  pass. The rehearsal is synthetic; production has not been migrated.
- Web components and generated OpenAPI contracts implemented. Attached to published
  recordings/sessions/releases, artist releases, directory profiles, classified ads
  and public events. Fan-club and event-moment replacements await compatibility.
- Native components, shared generated model/API, scoped PartySelector, opaque
  discussion routes and notification routing implemented. Native API release host
  corrected to api.tdfrecords.net. Native owner controls, reaction inspection,
  link/mention rendering and comprehensive flow tests still need completion.
- Web and native TypeScript checks passed at intermediate checkpoints (rerun after
  subsequent edits). Web tests: 2,000 generated reaction state transitions, 200
  Unicode mention edits; five rendered flows cover disclosure, failed optimistic
  rollback, same-key retries, deep focus, parent deletion. axe checks on rendered
  flow pass with contrast disabled in jsdom; real-browser contrast/keyboard/mobile
  verification still required.
- 10,000-comment / 2,000-reaction synthetic fixture exposed per-row policy overhead.
  Batched author eligibility plus target/author counters reduced local summary from
  8.59s to 0.642s and root page from 2.60s to 0.108s. Reply page 0.234s. Deep context
  was subsequently converted to batching and needs its final benchmark. These are
  busy local-machine measurements, not production service-level claims.
- Full initial no-code build reached the application but source edits during that
  pass required a repeat. Normal Stack build now running; no complete backend build,
  HTTP E2E, CI, review, merge or deployment result is claimed.

### Subsequent checkpoint

Full Stack build and web/native TypeScript passed intermediate checkpoints.
Legacy conversion preserves nested replies, 4,096-character bodies, titles,
media references, original IDs and historical reaction types. First activation
is transactional, repeat-safe, and permanently fences legacy writers; pausing
never reopens old authorization paths. Canonical erasure also scrubs archived
legacy bodies. Legacy adapters and source deletion are under final review.

SQL properties cover moderation/admin scope, private-event revocation,
legacy self-invitation denial, owner blocking, parent tombstones and conversion.
Real HTTP tests passed publication, reactions, comments/replies, idempotent
retry/conflict, edits, deletion, exact notification context, legacy replies,
keyset pagination, blocks and bearer revocation. Concurrency tests passed 80
competing reaction commands, duplicate comment requests, concurrent replies
and parent deletion, and block/write fencing.

Latest local 10,000-comment/2,000-reactor measurements: summary 154 ms, root
page 44 ms, replies 21 ms, deep context 38 ms. Synthetic PostgreSQL 16 only;
production PostgreSQL 17 rehearsal still required. Web rendered tests: 9 passed.
Mobile event repository tests: 12 passed, including removal of misleading local
fallback after remote authorization or network failures. Actual browser and
native accessibility/E2E remain underway. No PR, production migration,
activation, merge or deployment has occurred.

### Verified mobile association identities

Read from the signed iOS artifact and Google Play Console App signing page on
2026-09-28 (public certificate metadata only):

- iOS application identifier: `83J23NPXG7.com.tdfrecords.app`.
- Android package: `com.tdf.records`.
- Google Play app-signing SHA-256:
  `08:76:1D:24:F5:A8:45:2C:89:11:65:82:C7:C8:5D:0A:E2:B1:A7:24:C1:B4:8B:C0:7B:B1:9F:A2:8A:80:8E:B0`.
- Upload/EAS certificate SHA-256 (distinct from Play distribution):
  `34:E4:2C:EB:CD:7B:CA:3E:6D:F7:10:BD:8B:B1:0A:2C:76:CA:69:E2:1D:D8:E1:24:7A:3D:43:A6:62:CB:24:32`.

Set the existing Cloudflare association-function deployment variables
`APPLE_TEAM_ID` and `ANDROID_APP_LINK_SHA256_CERT_FINGERPRINTS` during release.
Include both Android certificates when supporting EAS and Play builds. Native
associated domains now include both canonical TDF hosts and the existing Pages
host; event routes remain supported alongside discussion links. Association
endpoints and an installed signed build still require deployed verification.

Actual desktop and Pixel 7 browser flows passed against the isolated real API:
pagination/disclosure, reactions, deep focus, authoring/replies, notification
navigation, edit and parent deletion. Browser axe serious/critical findings: zero.
Five native rendered flows passed optimistic rollback, same-key draft retries,
progressive disclosure, linked-reply accessibility announcement/tombstone and
immediate removal of cached bodies after access revocation.
