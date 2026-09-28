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
- [x] Complete source/caller inventory and adapter authorization matrix.
- [x] Canonical schema, legacy conversion and rollback rehearsal.
- [x] Backend operations, session/permissions, moderation and abuse controls.
- [x] Shared web/native components and every eligible existing entry point.
- [x] Mentions, notification aggregation/preferences, exact deep-link resolution.
- [x] Unit/property/model/database/concurrency/migration coverage.
- [ ] Desktop/mobile accessibility and end-to-end workflows.
- [x] Large synthetic discussion EXPLAIN/query-budget evidence.
- [x] OpenAPI/generated contracts, documentation, full relevant local checks.
- [ ] Independent repository review, protected green CI, merge and immutable build.
- [ ] Backup/guarded migration/recovery drill/deployment/live verification.

## Verification and release state (2026-09-28)

Draft pull requests: root [470](https://github.com/diegueins680/tdf-app/pull/470)
and native [116](https://github.com/diegueins680/TDF-mobile/pull/116). Production
schema and activation gate remain unchanged. Independent review, full green CI,
installed native verification and deployment are still release requirements.
The production restore rehearsal has passed (details below).

- Full normal Stack build and the full Stack test suite passed after compatibility
  updates. PostgreSQL 17 hosted property checks and fresh 137-entry migration
  rehearsal pass, including source retirement, legacy conversion, moderation,
  current privacy, session revocation and concurrency. Existing source engagement
  remains archived and mapped; no aggregate provider counts become local reactions.
- Real HTTP checks pass publication, desired reactions, duplicate/conflicting
  request keys, comments/replies, edit, parent tombstone, exact notification
  context, legacy adapters, pagination, blocking, bearer revocation, scoped mention
  search/discoverability, stable mention IDs, notification preferences and owner
  mentioned-only/off policies.
- All ten real-API browser journeys pass across desktop, phone and tablet Chromium,
  Firefox and WebKit, including
  exact notification navigation, focused deep links, editing and parent deletion.
  Browser axe serious/critical findings are zero. Native rendered flows and all
  521 native tests pass. Type/lint/release checks are rerun after follow-up edits.
- Rendered pagination test walks eight pages, verifies only five remain, verifies
  refresh issues five page requests, and navigates backward without a full-tree
  fetch. Both clients share cursor-history semantics; each response stays at 20
  comments. Earlier page controls restore evicted pages.
- Synthetic 10,000-comment / 2,000-reactor PostgreSQL 16 measurements: summary
  154 ms, root page 44 ms, replies 21 ms, deep context 38 ms. Nested auto_explain
  confirms batched author policy and indexed reaction lookup; instrumentation
  overhead raises these timings. These are local measurements, not production SLOs.
- Expo simulator build was rejected by the monthly Free-plan quota. The GitHub
  macOS simulator build passed and its bundled app is installed on a dedicated
  iOS 18.3 simulator; installed-device flows remain under verification.
  Its unsigned simulator artifact is not a store release. Signed release workflows
  now point to the canonical API; checked-in native projects include link
  entitlements/intent filters and the locked ExpoCrypto dependency.
- Root CI uncovered stale test mocks, generated specification inventory and
  reviewed catalog decisions, a setup-node major mismatch, an outdated mobile
  gitlink, and overlap between mocked persona and real-API test discovery. Fixes
  are under verification. The external Datadog monitor was retargeted with unchanged assertions;
  its GitHub rerun passed (details below).

### Content integration and boundaries

| Authority | Web entry points | Native entry points |
| --- | --- | --- |
| club_post | FanClubPage feed and posts | Existing fan-club web destination; native opaque discussion links |
| club_memory | FanClubPage, FanClubMemberProfilePage | Existing web destination; native opaque discussion links |
| recording, recording_session, record_release | RecordsPublicPage | Existing Records web destination; native opaque discussion links |
| artist_release | ArtistPublicPage, ReleaseFeed | Existing artist web destination; native opaque discussion links |
| event | SocialEventDetailPage, DirectoryPublicDetailPage | eventDetail, DirectoryPublicDetailScreen |
| event_moment | SocialEventDetailPage | EventMomentCard for persisted remote moments |
| directory_profile | DirectoryPublicDetailPage | DirectoryPublicDetailScreen |
| classified / opportunities | DirectoryPublicDetailPage | DirectoryPublicDetailScreen |
| artist_update | Trusted adapter for persisted social_sync_post; no existing client renders these external posts | Opaque discussion destination; no standalone native publication feed exists |

Label/venue publication content follows its actual persisted post/media authority.
Operational venue records do not acquire social discussions merely because they
have a directory route. Private device drafts retain device-only editing controls;
remote failures never become successful local engagement. Verified transaction
reviews, operational notes, and external provider metrics retain distinct models.

Compatibility clients receive bounded moment previews (20 comments/100 reaction
identities); exact totals and full traversal are canonical interaction endpoints.
Old array-count-only clients can undercount beyond that preview and need the new
client. Catalog administrative usage_count is a legacy summary; canonical target
counters and reference-protection triggers remain authoritative for this layer.

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

### Production-data and monitoring verification

A fresh 72 MB production database was exported and restored into the isolated
`tdf_interaction_restore_20260928` PostgreSQL 17.8 database. The complete reviewed
137-migration manifest applied without error. First activation, repeated enable,
pause, and resume passed; the conversion ledger remained one row and source counts
matched migrated counts. Exact production legacy post/reply/reaction/moment counts
were zero; all 30 existing notifications survived without any rehearsal delivery.
The clone is paused and has no application workers. Live production schema/gates
remain unchanged. Nonempty migration behavior is covered by synthetic properties.

The full Stack suite passed 3,539 examples with zero failures and six pre-existing
pending examples after preserving the pre-interaction SQLite inbox path. Installed
interaction authority always retains current bearer locks, including while paused.
The isolated HTTP runner now joins the backend CI job and can optionally run the
real-browser suite using `TDF_INTERACTION_BROWSER_E2E=1`.

Datadog API health test `r2d-i82-3jy` now targets
`https://api.tdfrecords.net/health`. Browser form comparison verified all other 85
fields unchanged. Persisted assertions remain HTTP 200, JSON content type,
`$.status == ok`, and `$.db == ok`; location, retries, scheduling state and blocking
CI rule are unchanged. GitHub Datadog rerun `36468621062` passed. No unrelated
monitor or application was changed.

The records thumbnail regression fixture now returns the actual unavailable-target
404 contract for synthetic rows. All ten records tests pass across five browser
configurations. The real HTTP suite also verifies that repeated unauthorized
moderation attempts exhaust the account write budget and return 429.
