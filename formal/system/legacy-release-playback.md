# Legacy release media and external provider actions

Requirement: `MEDIA-MUSIC-LEGACY-001`. Scope: the existing legacy release feed's
`Escuchar` action, source adaptation and FanHub dialog. This contract distinguishes
a native playback command from navigation to a known provider. It does not prove
that an arbitrary URL is audio, authorized, available or continuously playable.

## Inputs and outcomes

Inputs are a persisted legacy release ID/title, its artist and artwork metadata,
the existing per-release uploaded-media map, release streaming fields, and optional
artist-level provider fallback links. The historical Spotify field can contain a
direct media URL; the increment preserves that compatibility rather than migrating
or relabeling stored data.

`resolveReleasePlaybackAction` returns one of:

- `native(url)`: a candidate release-scoped HTTP(S) resource for the global player.
- `provider(url, provider, context)`: explicit navigation to Spotify, YouTube or a
  Google Drive HTML/share view, with release or artist context.
- `null`: no supported action after validation and fallback.

The ordered search is cached/uploaded release URL, release Spotify/direct field,
release YouTube field, then contextual artist Spotify/YouTube fallback. Invalid
candidates may be skipped. An artist-level unknown direct resource cannot stand
in for the selected release recording; a recognized artist provider link is
explicitly labeled as artist navigation.

The adapter trims input, parses relative upload references against a synthetic
base for validation, permits HTTP(S), and rejects explicit non-HTTP schemes and
URL username/password components. Known provider hosts are checked using host
boundaries, not arbitrary query text or a hostname substring. Existing streaming
normalization is reused; host aliases and share/profile pages outside its embed
normalization are classified as provider navigation. A Drive `/uc` resource with
an ID remains a native candidate; a Drive HTML/share page is an external action.

Direct resource URL bytes, including signed query strings, are retained where
already explicitly supplied as HTTP(S) or relative references. Scheme-less
references acquire a parsed HTTPS URL. This is source adaptation, not a MIME,
redirect, codec, ownership, permission or rights check. An unknown HTTP(S)
resource can still be HTML, missing, inaccessible or unsupported by the browser.

## Behavioral obligations

1. `dispatchReleaseToRadio`, despite its compatibility name, dispatches exactly
   one `PLAYER_LOAD_TRACK_EVENT` for a validated native candidate. It rejects a
   known provider page or invalid URL before dispatch. The command retains
   `legacy-release-{id}` identity, release title/artwork and the supplied artist
   name, with the existing fallback artist label when absent.
2. `FanHubPage.handlePlayRelease` requests this adaptation synchronously. Native
   candidates go through the existing global engine. Known provider actions open
   a labeled dialog with a normal `_blank` link and `noopener noreferrer`; they
   do not replace the global selection with a provider HTML document.
3. An artist-profile fallback states that it does not identify a release track.
   The dialog states that playback and controls belong to the external platform.
   Opening or closing the dialog does not pause the current global session.
4. Actual primary or middle-button provider-link activation dispatches
   `RADIO_TAKEOVER_EVENT`; the existing global-player listener pauses while
   retaining its selected track and position. The event does not prove that the
   external tab opened successfully or started playback. Copying a link and
   opening it elsewhere is outside this click bridge.
5. The feed's accessible action feedback announces a request to listen. It does
   not announce successful playback merely because a command or dialog opened.
6. Link-save payload construction validates the same URL boundary before calling
   the existing API and chooses the existing YouTube field from actual provider
   classification, rather than incidental text in an unrelated hostname/query.
   This frontend validation grants no backend publication or ownership authority.

Native selection, provider choice and dialog state are local UI transitions.
No provider SDK, extraction proxy, redirect fetching, migration or new audio
element is introduced. Provider pages remain external and have their own
permissions, autoplay policies and controls. This contract promises no universal
provider playback, gapless handoff or route continuity in the external tab.

## Verification and assumptions

| Obligation | Test boundary | Classification |
| --- | --- | --- |
| Provider/unsafe URLs do not emit native commands | Actual `ReleasePlayerActions` helpers with synthetic URLs and event target | TESTED finite command controls; not all URLs/redirects. |
| Stable release identity, signed direct references and artist attribution | Same helper tests | TESTED command payloads; no decoder or rights acceptance. |
| Feed action opens a contextual provider dialog/link without native selection | Actual `FanHubPage` and `ReleaseFeed` actions with API/UI dependencies declared as doubles | TESTED rendered JSDOM journey; actual external navigation is prevented. |
| Dialog opening/cancel preserves global playback state; link activation pauses it | Actual `PlayerProvider` command/state integration with `play/pause/load` doubles | TESTED model of UI commands, not real media or provider playback. |
| No premature successful-playback announcement | Rendered `ReleaseFeed` regression | TESTED accessible text, not a complete WCAG audit. |
| Real native decoding, successful provider playback, production rights and availability | Separate browser/target-environment acceptance | UNVERIFIED by this increment. |

There is no theorem, extracted state machine or model-checking claim here. The
finite unit and integration controls test the actual helper and event boundaries.
Existing global-engine state, account isolation, media authorization, URL expiry,
legacy direct-URL persistence, embedded streaming components, radio/broadcast and
microphone playback, other tabs and operating-system audio remain independent
obligations. Dispatching a takeover signal cannot establish exclusive ownership
of all audio in the application or device.

The recorded implementation receipt binds source commit
`6f9b192cd6d0b4652b9e3342e4e7db392082fd64`: 122 tests in seven suites passed,
lint on six touched files passed, and TypeScript/Vite/bundle checks passed.
Native browser decoding was not run. These executions retain their original
source fingerprints; later integration requires current-candidate checks.

Reproduce focused checks with the existing frontend tooling:

```sh
npm test --workspace=tdf-hq-ui -- --runTestsByPath \
  src/features/releases/ReleasePlayerActions.test.ts \
  src/features/releases/ReleaseFeed.test.tsx \
  src/pages/FanHubPage.playback.test.tsx --watch=false --reporters=default
```

The broader recorded seven-suite run and artifact hashes are in the versioned
delivery report. Configured CI or a local build is not merge/deployment evidence.

## Release and rollback

The change is a focused independent pull request targeting main, separate from
the canonical music playback/discovery candidates. It adds no schema, backfill,
provider infrastructure, payment activation or production business write.
Application rollback restores the prior three source files; no data reversal is
required. Existing original masters, stored release relationships and provider
permissions retain their prior authority.

`PLAYER-002/003 -> MEDIA-MUSIC-LEGACY-001 -> source classifier + FanHub/ReleaseFeed
actions -> helper/rendered/provider-state tests -> exact-candidate CI -> independent
review -> target release acceptance`.

Universal legacy-audio ingestion, indexing/search completeness, signed-reference
refresh, unavailable source reconciliation and actual native/browser/provider
acceptance remain incomplete. Permission to follow a link or emit a media command
does not establish the rights needed to publish or redistribute its content.
