# Listener-scoped local playback state

The final validator correction is committed at
`b6259692c11c7b502b974cf6bee7d4ca189925f7`. A matching-owner saved track
with sources `[validSource, null]` reproduced an App render crash under the
previous validator. Nonempty source arrays must now pass the existing predicate
for every member; the whole corrupt track is discarded and valid explicit B can
still play. This closes the named crash path, not every possible malformed-field
case. Prior `a0120daa` 85-test/27-browser results retain their historical identity;
the corrected source has its own 86-test/27-browser qualification.

Status: implemented at `b6259692c11c7b502b974cf6bee7d4ca189925f7`, locally tested;
locally integrated with the final playback foundation; not deployed.

A local audit reproduced a privacy/UX defect: after logout or account changes,
the next client principal could see the preceding listener's public track title,
queue and resume position. The audit used actual App/session/player composition
with synthetic public tracks and declared media/API doubles. It did not establish
server-private media access or continued sound across accounts.

The selected policy keeps volume, mute and quality as device preferences while
scoping queue, selection, positions, shuffle and repeat to an explicit client
owner. Legacy untagged listening state is discarded; a saved record restores only
for the same resolved valid owner. Anonymous ownership is explicit `null`, not a
fallback for an invalid authenticated descriptor. One device-anonymous context
cannot distinguish separate anonymous people.

Initial pending identity neither exposes saved titles nor overwrites saved state.
When the same owner becomes ready, a matching queue may restore paused only if
the user has made no queue edits. A fresh selection, clear, reorder or other edit
must win even when the current queue is empty. Readiness must not revive old A
after the listener chose B or replace B's decoder. Actual owner changes clear
listening state and stop old work, while current device preferences remain.
Rapid A/B/A or anonymous/account/anonymous transitions must not revive an old
matching envelope before the intermediate owner's delayed save has run.

The engine belongs inside `SessionProvider` and above the keyed
`SessionQueryProvider`. `App` consumes it rather than owning another provider.
This preserves same-owner playback through bootstrap/query scope changes and
supported navigation while allowing actual owner changes to reset it. The
committed composition and guarded hydration replace the initial prototype's
rejected readiness remount.

The implementation branch is `fix/player-listening-identity-20261026`, initially
based on playback head `7de7c292753565ae6ea64c57dd9aa4b97e330b6d`.
The older diagnostic/design assessment binds main `3af768d`; a desired logout
regression also fails on `7de7c2927`. Final qualification records 86/86 tests in
eight suites (23 identity, 39 engine, three queue, 21 session), scoped lint and a
Node 22.14.0 TypeScript/Vite/budget build passing at 336,907 initial gzip bytes.
The installed three-project matrix passes 27/27: 24 native real-media cases plus
three declared mocked smoke cases, zero skips/retries/flaky cases. Chromium
desktop/phone emulation and Firefox each pass the new held-session case: fresh B
keeps the same actual decoder/source when same-owner bootstrap resolves. Session,
catalog and authorization replies are synthetic, media are generated AAC/FLAC
served over loopback; no backend identity/rights or physical-device result is
inferred. WebKit and hosted current-candidate checks remain pending. Source,
receipt, logs and bundle hashes are retained in
[the delivery report](../../reports/player-listening-privacy-2026-10-26.json).

The scoped contract is
[player-listening-privacy.md](../../formal/system/player-listening-privacy.md),
owned by `MEDIA-MUSIC-PRIVACY-001`. This increment appends the requirement to
142 inherited playback requirements, deliberately adding only
`SessionProviders.tsx` to the existing `MEDIA-MUSIC-PLAYER-001` implementation
mapping and correcting that contract's ownership/navigation correspondence. All
other fields of that inherited row and all other inherited requirement objects
remain exact against `7de7c2927`. The bounded player model does not
model identity/preferences, and its verification status will not be upgraded.

The final branch must normally merge the latest root playback foundation with
current main before publication. Queue-owner labels are client isolation aids,
not server authorization, byte revocation, secure erasure or a cross-tab protocol.
Existing server libraries/history, original masters, delivery checks, payments and
media dependencies remain unchanged. Rollback to the older unscoped loader can
restore the known privacy gap; target rollback, physical-device acceptance,
hosted checks, independent review and production deployment remain unverified.

Catalog lineage retains 1,338 unchanged active parent decisions and the exact
superseded eight-action queue review in append-only history. One individually
reviewed nine-action successor adds internal guarded `restore`, leaving 1,339
active decisions. These are reducer dispatch mechanics; no domain catalog or
permissions become client-controlled. Other decisions/reconciliation history
remain unchanged.

Historical integration checkpoint: normal merge of protected main
`78fa351b469c1d2031df3232204dd722ac480e3b`, after playback PR #620 and
ledger-header PR #621. The P0 foundation is already merged; this privacy change
will be reviewed as a focused PR targeting main. Canonical requirements total
146: 144 foreign main objects remain exact, the owned PLAYER object differs only
by its SessionProviders implementation append, and PRIVACY is new. All six
corrected runtime/test fingerprints remain identical to `b6259692`.

The main catalog's new ledger-header reconciliation remains intact. All 1,339
main decision objects are retained: 1,338 unchanged active records and the exact
old queue review in history, plus its individually reviewed successor. The
222-entry migration manifest is byte-identical to main; privacy adds no migration.

The 86-test/27-browser receipt retains its original source identity and does not
qualify unrelated inherited main changes. Exact committed-source admission,
current privacy CI/review and production acceptance remain separate gates.


Historical protected-main reconciliation (2026-10-26): this branch normally merges
`9b7aa4648d4158f6da84fbb5e794267cd4953177`, containing the protected discovery
PR #622 and Stripe PR #623 changes. It does not incorporate pending legacy,
source-availability or credential-cache feature heads. Current requirements total
147: 145 foreign main objects are exact, PLAYER retains its semantic fields with
only the SessionProviders implementation mapping append, and PRIVACY is unchanged.
The catalog retains all 1,339 main objects across 1,338 unchanged active records
and the exact old queue review in history, with one reviewed active successor.
All inherited reconciliation fields and the 222-entry migration manifest remain
exact. All six corrected `b6259692` runtime/test hashes and the package graph/lock
remain unchanged; same-owner fresh-B decoder continuity and corrupt-source
rejection are preserved.

The historical 86-test/27-browser qualification keeps its original identity. This
metadata integration runs a new focused unit and structural-metadata qualification;
it does not relabel historical browser, build or production results. The inherited
API module change is limited to catalog list pagination; asset-access resolution
and the player/session runtime are unchanged. Current-candidate hosted checks,
fresh independent review and production acceptance remain separate gates.


Latest protected-main reconciliation (2026-10-26): the target is now
`2e01bbe1263e8761fa2692bd951f828c01142449`, after legacy-audio PR #624 merged
through the protected process. The earlier main checkpoints above retain their
historical identities. This normal merge preserves all six `b6259692` qualified
runtime/test hashes and the package graph/lock. Canonical requirements total 148:
146 foreign main objects are exact, PLAYER differs only by its authorized
SessionProviders mapping append, and PRIVACY is unchanged. All 1,341 main catalog
objects remain active or in exact append-only history: 1,340 unchanged active
reviews, the exact old queue review archived, and its existing reviewed successor.
The two inherited legacy technical reviews and all main reconciliation fields
are preserved. The 222-entry manifest is byte-identical to main.

The fresh 9b unit qualification remains 86/86 in eight suites; these unchanged
source inputs do not justify relabeling or repeating historical native/browser
results. This refresh checks canonical metadata, strict catalog lineage/freshness
and prepared source mappings. Immutable descendant admission follows the root's
normal merge commit. No review request, feature-head merge, deployment or
production acceptance is implied by this preparation.
