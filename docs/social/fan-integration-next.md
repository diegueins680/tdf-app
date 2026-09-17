# Remaining fan integration boundaries — audit 2026-09-16

Evidence is source inspection at `daf6c5a680b81928c2e2c66df98063328e6d5e9c`,
not runtime qualification. #415 preserves subscriptions and retires implicit effects;
it does not complete membership or notification privacy. No activation is authorized.

| Priority / capability | Observed implementation and client | Repair acceptance |
|---|---|---|
|1: artist-follow GET|`Server.hs:8090` selects all actor FanFollow rows, builds artist name/profile maps without canonical eligibility. Fan/Customer role validation exists; current-token locked serving does not. Existing ServerSpec checks malformed auth, not privacy races.|Reuse FanFollow/DTO; shared session boundary and bulk profile eligibility before returning fields; denied targets absent after block/closure/revocation; retain unsubscription. Preserve order/contract or add explicit paginated version; do not silently truncate legacy list. Connect existing profile model observations to this handler.|
|1: notification list/count|`Server.hs:8487–8509` scopes recipient, selects latest 50 by createdAt, and separately counts all unread. `Models.hs:1157` stores recipient/type/body/target but no originating actor or source version. Artist-follow target is artist, not follower.|Model authoritative eligibility for both list and count. Historical `artist_liked` cannot be safely per-follower reauthorized from current fields: retire unqualified presentation under existing latch, or use reviewed provenance; never infer sender from text. Keep unrelated booking/operations/access-request notifications within their own authority. Add deterministic tie-breaker; do not leak excluded rows in badge count.|
|2: member-profile read/write|`ServerFanClub.hs:756–845`: roster reads each author separately; self-profile PUT updates or creates; `requireArtistKey:1160` only validates a positive path ID. These local handlers do not check current FanFollow/club authority or canonical block/closure. No new runtime exploit claim is made.|Define public/member/owner audiences explicitly before changing contracts. Model publication/withdrawal, artist access, current-token and concurrent unfollow/revocation; apply bulk policy before names/avatar/bio/roster. Explicit creation must not manufacture graph consent. Keep stored identities/joinedAt and authored data.|
|2: web publication and cache|`FanClubMemberProfilePage.tsx:420–456` query keys use artist/auth boolean, not requesting principal; edit form renders only after profile exists. It also loads club memories and supports contact actions.|Use existing session-scoped cache pattern; cancel/clear stale requests on account switch; model/check that previous actor results cannot render. Add explicit accessible publication action with privacy explanation and validation only after server policy qualifies it. Cover empty/loading/403/404/error/retry and keyboard journeys.|
|3: fan memory/content and club roles|`ServerFanClub` memories may create member profile on submission; officers/Admin/Agency and follow-based posting are separate permissions. Profile retirement alone cannot qualify these paths.|Audit inherited audiences, media authorization, officer revocation, counts, reports, leaderboard/reputation and communication before rollout. Preserve existing report/review and adjacent event/commerce authorities; no reputation from follows or club proximity.|

Notification API consumers are in `tdf-hq-ui/src/api/fans.ts:193–214`; missing-endpoint
compatibility returns an empty list/count for selected unsupported statuses, not a
substitute for server authorization. Notification writers also exist in
`ServerInternAudit`, `ServerFeedback`, `Cron` and access-request handlers. Avoid a
blanket suppression that silently removes operational notifications. Read acknowledgements
are recipient-scoped today; qualify token revocation and retries in the same slice.

The pinned mobile source `c1832c5e` search found no direct legacy fan-notification or
member-profile route consumers outside generated contracts. This is source evidence,
not a native runtime test or proof that remote/deep-link clients do not exist.

## Delivery sequence and validation

1. Reuse existing profile/session models for artist-follow list; extend if any new
   policy or pagination state is introduced. Add generated denied/success outcomes,
   actual HTTP fixtures, high-degree query-count/latency measurements and compatibility.
2. Model legacy notification retirement and provenance separately, then qualify list,
   unread count, acknowledgements, pause/reapply and mixed unrelated notification types.
3. Decide club/profile audiences from existing ownership and publication semantics;
   model concurrent publication/withdrawal and revocation before implementing server
   and client changes. Test real account switching and browser access states.

Each is a dependent draft with its own evidence, flags and reversible data plan.
These are **blocked completion criteria**, not completed implementations or grounds
to activate the platform. Product value remains accepted useful connections, leads,
bookings and sales; no conversion result is established by this audit.
