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
JSDOM command/state evidence. Real browser media decoding and successful provider
navigation/playback were not run. Artifact hashes and scope are retained in
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
