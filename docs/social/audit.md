# Capability and domain audit

Access date: 2026-09-14. Source paths below are relative to the repository root at
`17a33eca1`; these are code findings, not observations of production behavior.

## Capabilities and baseline

| Capability | Actual observation |
|---|---|
| Repository | Existing checkout on main has extensive unrelated tracked/untracked edits, including Server.hs, social events, authentication, player and mobile. Preserved untouched. Isolated worktree `tdf-app-social-20260914`. |
| Remote | `git ls-remote origin refs/heads/main` returned baseline SHA. GitHub CLI read works with network permission. |
| Open work | Payment PR chain #331/#332/#334/#340/#343/#347/#350; event chain #336–#342/#345/#346/#348/#349/#351/#352; CI #333. No branch merged or copied into this work. |
| Backend | Haskell2010, Servant/Warp, Persistent/PostgreSQL; stack 3.7.1, lts-24.42/GHC 9.10.3. Use Stack only. SQLite is used by many existing unit/handler fixtures; it cannot establish PostgreSQL locking guarantees. |
| Web | React 18, TypeScript, Vite, MUI, TanStack Query, Jest, Playwright. Node 24.8.0. Reused installed dependencies via ignored symlinks; did not install/modify main dependencies. |
| Mobile | Expo/React Native separate `tdf-mobile` git submodule. Main submodule has unrelated changes. Read-only audit there; isolated worktree initially uninitialized. Do not count root scripts' optional skips as passes. |
| Authentication | `Auth.hs`: API token maps to Party; active credential fallback, catalog roles/modules. AuthedUser contains PartyId, roles, modules, not token identity. Managed-entity authority must be checked independently. |
| Persistence | `Models.hs`, `ModelsExtra.hs`, `Models/SocialEventsModels.hs`, catalog models; raw SQL migrations plus Persistent boot migrations. Indexes and joins are available. No need for graph engine demonstrated. |
| Jobs / integration | `Cron.hs`, `Server/SocialSync.hs`, `Notification`, `EngagementEvent`, event research and reputation workers. Third-party social sync is operations tooling, distinct from consumer Discover. |
| Media / cache | Existing media URL fields, upload handlers and storage integrations; Query client is derived state. Public URLs cannot be made private by graph filtering. No new cache or messaging service justified. |
| DB tests | PostgreSQL 16.10 CLI installed; default local socket had no server. Docker 29.8.0 available after sandbox permission. Disposable containers only. |
| Formal | Found local Temurin Java 17.0.12 runtime and tla2tools-1.7.2.jar. TLC requires sandbox permission for its local RMI listener. |
| Web baseline | Ran existing social API/inbox suites: **3 suites, 44 tests passed**, `evidence/baseline-ui-tests.txt`. |
| Backend baseline | Restored baseline `stack test --fast` failed at optional reactor DTO field; its missing retry helper was repaired separately. Candidate test binary: 2,540 examples passed. Stack copy and complete-schema startup remain separately qualified. |
| CI | Last five main runs returned by gh were messaging-token jobs on another SHA; these are **not baseline build/test evidence**. #333 is an open CI-repair PR. |
| Performance | No representative production degree histogram, query latency, storage cost or useful-outcome dataset was accessed. All new fixtures must be labeled synthetic. |

## Inventory and gap matrix

| Capability / response | Evidence: schema; API/server; UI/tests | Rules, defects, dependencies and acceptance |
|---|---|---|
| Accounts/public profiles — reuse + repair | `Models.hs:UserCredential,ApiToken,Party,FanProfile,ArtistProfile`; `Auth.hs`; `Server.hs:socialGetProfile,loadSocialPartyProfilesDTO`; `SocialPage.tsx`; `ServerSpec.hs` | Profile DTOs already minimize fields. Party is a CRM entity, not proof of an active account. Batch IDs bounded at 100. Add explicit discoverability; never expose legal/contact/financial fields. |
| Artists/identity — reuse | `Models.hs:ArtistProfile,FanFollow`; `Models/SocialEventsModels.hs:ArtistProfile,ArtistFollow`; `Profiles/Artist.hs`; `ArtistPublicPage.tsx`; `Social/FollowSpec.hs,FollowHandlerSpec.hs` | Two artist identity/follow namespaces exist (numeric Party vs social artist IDs/text party references). Preserve both; no ID cast as migration. Canonical mappings need reconciliation. |
| Bands/organizations/team/venues — reuse | `ModelsExtra.hs:Band,BandMember`; `ServerBands.hs`; `Models/SocialEventsModels.hs:Venue`; event handlers; catalog permission tables | Membership and managerial authority are separate from social consent. BandMember role is textual. Acting through an organization needs explicit owner/membership authority; no graph inference. |
| Follows/connections — repair | `Models.hs:PartyFollow,FanFollow`; `Server.hs:socialAddFriend,vcardExchange,fanFollowArtist,socialRemoveFriend`; `SocialPage.tsx`, `api/social.test.ts` | AddFriend and vCard write both directed edges. Fan follow auto-connects club members in both directions. Chat then accepts mutual follows. Historical mutual edges are not evidence of bilateral consent. Require explicit intent from each principal. |
| Blocking/muting/private requests — essential addition | No canonical block/mute schema or enforcement found in audited social handlers | Block is authoritative denial; mute is private presentation preference. Need persisted state, race protection, exclusions before counts/reasons; lifecycle cutover must cover legacy endpoints before activation. |
| Posts/replies/media/reactions — reuse + repair | `Models.hs:FanClubPost,FanClubPostReaction,FanClubMemory`; `ServerFanClub.hs`; fan club pages; handler validation tests | Existing club-scoped posts, replies, hidden flag and reports. Reuse them; no duplicate general post domain. Media remains in existing storage boundary. Hidden/access filtering must precede every feed, including boosted discovery. |
| Feed/discovery — repair | `Server.hs:discoveryFeed,socialListSuggestedFriends`; `ServerFanClub.hs:getFeed,getSpotlight`; `FanHubPage.tsx`, `SocialPage.tsx` | Boosted feed fetches a post by id without applying club access/hidden exclusion first. Suggestions traverse all followed edges with no bounded query. No stable cross-club Following cursor found. Separate chronological Following from explainable Discover. |
| Notifications — repair | `Models.hs:Notification`; `Server.hs:notifList,createArtistFollowerNotification`; `notificationTarget.ts` | Follow notification is created only for new FanFollow, useful dedup reuse. Need authoritative revalidation at delivery and serving, no stale private names/counts. Current payload history cannot be recalled from devices. |
| DM — repair | `ChatThread,ChatMessage`; `Server.hs:ensureCanChatWith,chatGetOrCreateDM,chatListThreads,chatListMessages,chatSendMessage`; `SocialInboxPage.tsx`; `ServerSpec.hs:chatListMessages` | Thread participant check exists. Mutual follows authorize sending; admin bypass exists. Permission check and write use separate DB actions. List threads does N+1 last-message/name queries. Existing thread creation check/insert can race. Consent and block must fence message insert. |
| Communities/communication — reuse | Fan clubs/officers/elections/member profiles/inbox; `ServerFanClub.hs`; `Server/Rooms` or room domain | Fan clubs are existing shared spaces. Room is venue/resource infrastructure, not evidence of a general chatroom product. Do not invent community/chatroom duplication. Preserve officer moderation; social graph does not grant officer powers. |
| Search/recommendations — repair | `Server.hs:partySelector*`; `components/party-selector/PartySelector.tsx`; social suggestions | Reuse accessible selectors and existing minimized profile DTOs. Do not use internal IDs as the primary UI. No private mutual-network explanations. Interests/city only if explicitly published and consented. |
| Reputation/reviews/moderation — reuse | `API/Reviews.hs`, `Server/Reviews.hs`, `Models.hs:FanClubMemoryReport`; reputation SQL/workers; `ModelsExtra.hs:InternalFeedbackReport` | Verified-context reviews and moderation already exist. Following/consent never imply endorsement or reputation. General harassment reporting coverage still requires API/UI audit; do not label internal feedback as a complete abuse system. |
| Events/RSVP/ticketing/virtual venues — reuse | `Server/SocialEventsHandlers.hs`; `Models/SocialEventsModels.hs:EventRsvp,EventInvitation,EventTicket`; `docs/social-events/rsvp-sharing-profile-feed.md`; `SocialEventDetailPage.tsx` | RSVP profile visibility is explicitly opt-in; ticket ownership grants event entitlement only. Event operations PRs overlap this boundary; preserve contracts. No purchase or private attendance recommendation reasons. |
| Releases/EPKs/storefronts/services/bookings/sales — reuse | `ArtistRelease`, `ServiceAd`, `Booking`; marketplace/service/release routes and public pages; `EngagementEvent` | Keep existing conversion flows and entitlement checks. Link to their existing entrypoints; do not duplicate checkout or attribute a sale solely because a social edge exists. Main has active release/player work. |
| vCard sharing — repair | `SocialPage.tsx` sends encoded vCard to `api.qrserver.com`; local qrcode dependency exists | Contact details are sent to a third-party QR service. Generate QR locally; sharing a static card is not recipient consent to connect. |

## Acceptance gate

Inventory identifies concrete source symbols; it is not exhaustive proof of every
route, worker or field. Before activation, complete the endpoint-by-endpoint
policy coverage map, legacy/client adaptation, complete-schema rehearsal and media
privacy qualification. Do not claim that a disabled additive API repairs legacy
production behavior. Legacy edges remain unchanged and explicitly unverified.

## Capability updates after baseline

Native PostgreSQL 16.10 private-cluster HTTP tests passed after the shared Docker API
returned 500. Docker fixture tests and synthetic benchmarks had already succeeded;
the later failure does not invalidate their recorded outputs, and no shared service
was restarted. The mobile submodule was added as a separate git worktree at the
recorded gitlink, preserving the original dirty `app/access-requests/review.tsx`.
Only generated mobile types changed. GitHub branch pushes and draft PR creation
were exercised; CI results and incomplete coverage are recorded in the handoff.

### Session continuation — 2026-09-15

The baseline authentication inventory above remains historical. The new
[session boundary](session-boundary.md) retains an internal API token ID and
revalidates it transactionally for `/social/v2`; legacy handlers and delegated
entity contexts still need integration. The observed old-handler revocation bypass
and generated model-to-PostgreSQL checks are recorded in that packet.

### Legacy chat continuation — 2026-09-15

PR #390 covers all four existing ChatAPI operations without changing their wire
DTOs. `TDF.API.Chat` shares the contract with the HTTP fixture; `TDF.Social.Chat`
centralizes policy selection/session checks/error mapping; the additive chat SQL
checks eligibility before previews, fields and cursor errors. Eleven new bearer
HTTP cases plus the existing 82 session/social cases passed. PR #391 scopes the
existing ChatPage/useChatUnreadCount/read-state helpers by account and withdraws
stale display data on error. No managed-entity, generic profile, notification or
media endpoint is implicitly covered by these chat-specific checks. Full-schema
PostgreSQL 16.10 native and 17.10 Docker fixtures passed; legacy readers remain a
cutover blocker, while the retained #386 trigger protects legacy writers.
