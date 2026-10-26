# Legacy release listening actions

This focused main-targeted increment keeps Spotify/YouTube pages and Google Drive
HTML/share views out of native audio playback. A listener pressing `Escuchar` sees
an explicit provider action for those links. Candidate direct media continues
through the existing global player with stable release identity and the actual
artist name. Artist-profile fallback links state their contextual purpose.

The implementation is source commit
`6f9b192cd6d0b4652b9e3342e4e7db392082fd64` on base
`1c9c833cc1ba62b47822fca3c20e14e86cf28daa`, branch
`fix/legacy-audio-links-20261026`. It is independent of the canonical music player
and catalog-discovery pull requests. Publication, merge and production deployment
are not established by this document.

The provider dialog explains where playback occurs and offers a normal external
link. Opening or canceling it preserves the TDF session. Actual provider-link
activation pauses the global player through its existing takeover event, retaining
track and position. Successful external playback, cross-tab ownership and playback
continuity on another platform are not promised.

The adapter rejects non-HTTP schemes and URL userinfo, avoids provider identity
from spoofed hostname/query text, preserves signed direct references, and refuses
to treat an arbitrary artist-level direct URL as a release recording. A direct
HTTP(S) URL is a candidate only: the adapter cannot prove its MIME, availability,
authorization, rights, redirect behavior or codec compatibility. Private Drive
content remains subject to Drive permissions; no extraction or bypass is added.

The initial regression run produced nine expected new failures and nine existing
passes. The source-bound final receipt records 122 tests in seven suites passed,
lint on six touched files passed, and TypeScript/Vite/initial bundle checks passed
at 335,237 gzip bytes with five preloads. Rendered FanHub/ReleaseFeed actions and
the actual `PlayerProvider` state are exercised using declared media doubles; this is
JSDOM command/state evidence. Real browser media decoding and successful external
provider playback were not run. Artifact hashes and scope are retained in
`reports/legacy-provider-actions-2026-10-26.json`.

The scoped contract is
[legacy-release-playback.md](../../formal/system/legacy-release-playback.md), owned
by `MEDIA-MUSIC-LEGACY-001`. All 142 base requirement objects are retained unchanged;
the new scoped requirement makes 143 total. All 1,330 inherited catalog decisions
are preserved. Two individually reviewed technical decisions cover only the
classifier's exact `spotify.link`/`spoti.fi` host cases and the synthetic session
fixture's `username`/`partyId`/`roles`/`modules` keys, making 1,332 total. The host
cases define parsing/navigation behavior, not a business-provider registry; the
fixture grants no production permissions. There is no blanket fixture exemption.
Mechanical source maps establish freshness and coverage, not correctness.

This increment adds no migration, original-master rewrite, asset backfill, payment
change or infrastructure. Application rollback restores the prior source behavior.
Exact current-candidate hosted checks, independent review and target-environment
acceptance remain required before merge or deployment claims.

## Browser boundary and current-main refresh

Four actual-browser cases passed in desktop Chromium and Pixel 7 on the source
`6f9b192` bundle, with synthetic API and provider destinations. They verify dialog
opening/cancellation, an explicitly contextual artist profile, keyboard link
activation, 44-pixel targets, actual popup navigation to the locally fulfilled
destination, null opener/absent referrer, no native selection and a single takeover
event on provider activation. All external provider traffic is intercepted; this
is neither successful provider playback nor native media-decoder acceptance.
The initial fixture failures are preserved by receipt paths in the report.

The browser test SHA-256 is
`3e34a6eb6dc507c67c48d574a84e02bfbdf0c54157ef47e57d2fbfd8e5696413`;
the results SHA-256 is
`9b2708cf19744e0d9845a42e4c2cc3ccc718eec19f2bacaa70a9a45af1e470e6`.
The loaded entry was `index-elbjHYrn.js`, SHA-256
`827a141b8161cd99f11698d9d90237c110d1de3ddc857cf15fd73a736292334a`.
These identities remain tied to their actual execution, not relabeled as a new
build after metadata or base reconciliation.

Metadata was checkpointed as
`f94a406c53fd2add1183d65d27043c483dfbd7ec`, then normally reconciled with main
`3af768dacd95c1223975c62da3ce8088bed89b47`, including the release-journal
succession work. All 142 current-main requirement objects are retained exactly,
with the original legacy requirement unchanged, for 143 total. Main's 1,330
catalog decision objects plus the two individually reviewed technical cases make
1,332 total. Only generated inventory/traceability conflicted. The actual pinned
Mobile source remains `d832bb3057dd03d44c0f35eea05be9f36320429d`. The main merge
itself changes none of the legacy audio implementation files; the accompanying
Drive correction below changes the URL adapter and its regression test.

The current-main merge introduces no audio source change. Mechanical metadata,
preservation and strict catalog checks are repeated for the refreshed candidate;
the prior unit/build/browser results keep their original scope. No redundant full
UI run, production deployment, migration or actual provider playback is claimed
by this refresh. Root integration still requires current-candidate CI and review.

## Conservative Drive download boundary

Review found that `/uc?export=view&id=...` had been accepted as a native candidate.
The adapter now admits a Drive `/uc` reference to native selection only with
exactly one `export=download` parameter and a nonblank ID, matching the existing
direct-content URL builder. Other Drive references remain explicitly external.
This does not declare that view links are always HTML; their native-audio behavior
has not been established. Even an admitted download reference remains subject to
Drive permissions, actual MIME, availability and browser compatibility.

The four new baseline controls failed against the earlier adapter (four failures,
31 existing passes). The corrected working sources passed 49 tests in three
focused suites, lint on the two changed files, TypeScript/Vite/bundle checks at
335,240 gzip bytes/five preloads, and four refreshed desktop/Pixel 7 browser cases.
The initial source-bound 122-test and four-browser runs above remain historical
evidence for their original inputs; they are not presented as current executions.

The new receipt records dirty-source qualification at actual HEAD
`f94a406c53fd2add1183d65d27043c483dfbd7ec` with merge parent
`3af768dacd95c1223975c62da3ce8088bed89b47`. The current adapter SHA-256 is
`67c0980c3268cc54bda54b7ee871d8345a79d754b1e100e6c98f18340b1f1b68`
and its test SHA-256 is
`c5ad21fcffed6f6d4096788ddfafef44cbdcf3b693dc6c9c37b1012e871c0faf`.
The other four implementation inputs retain their original hashes. The refreshed
browser results SHA-256 is
`cacbe25a5a5a1e8fb6655ceb461e590d94c3783ab9a8e912e80ccc51c8e1f81d`;
the actual new entry `index-CRz9rejh.js` has SHA-256
`cbb185a497ae5aa6302429800f93f5254e970305a74983745572151008bad872`.
The browser test source remains unchanged. Every receipt source/log/result and
bundle hash was checked against local bytes before metadata reconciliation.

This qualification concerns the exact working sources, not a clean release
commit. Provider destinations and API data remain fully synthetic/local; no
provider network, external playback or native decoder acceptance is inferred.
`reports/legacy-provider-actions-2026-10-26.json` retains both historical receipts
and the new correction receipt with their distinct identities and limits.

## Protected-main refresh after fan-club recovery

The candidate was normally reconciled from
`3f6c322b40fdb36e642b8ccb0cb56e8b5bd880d3` with protected main
`ed9eb9a1600f0a827e002bb2ab64a608f8352b2c`, which adds the independently
owned fan-club recovery requirement. All 143 main requirement objects are
retained exactly, followed by the unchanged `MEDIA-MUSIC-LEGACY-001` object,
for 144 total. Main's 1,330 catalog decisions and the two previously reviewed
legacy technical decisions remain exact, for 1,332 total. Generated conflicts
are reconciled from the combined canonical rows and actual pinned Mobile.

All six legacy implementation/test inputs and the browser test retain their
exact bytes from `3f6c322`. The prior 49-unit/four-browser source qualification
and its bundle hashes remain historical evidence for their actual execution;
this merge does not relabel them as a fresh build or browser run. Metadata,
source-admission and strict catalog checks apply to the refreshed candidate.
The previous review is historical and a fresh review is required after the
new head is published. No production deployment, provider playback, migration
or protected-main merge is established by this reconciliation.
