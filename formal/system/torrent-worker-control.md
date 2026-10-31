# Torrent page control and request ownership — MUSIC-TORRENT-CONTROL-001

An ordinary application document must be controlled by the activated torrent
worker before the real WebTorrent client creates its browser server or attaches
a stream. The previous `/torrent-stream/` scope did not include actual app
routes. In isolated Chromium and Firefox, the published 2.8.5 library returned
51 bytes of HTML rather than the synthetic 16044-byte WAV. Root scope with the
same original worker/library produced the WAV. Root control also exposed a
second defect: the upstream broadcast let a different tab return its 404 before
the owning tab's audio response. These are finite counterexamples, not reports
of production incidents or a general vulnerability proof.

Acceptance criteria:

1. Register the existing `/webtorrent-sw.min.js` URL at `/`. Reuse the browser's
   owned registration and revalidate on every request. Refuse a different worker
   URL in the current controller or root active/installing/waiting slots before
   overwriting anything. Recheck after asynchronous registration lookup.
2. Resolve readiness only when the active worker is activated and is exactly the
   document's controller. Observe controller, worker state and registration
   updates together; clean all listeners and the 15-second timeout on exit.
   An already activated owned worker may be asked once to claim eligible pages;
   success still requires the actual controller identity, not a message reply.
3. Root scope must leave URLs outside `scope + webtorrent/` untouched, including
   query strings containing a stream/control URL. Preserve the library's stream,
   keepalive and cancellation-support protocols inside that prefix.
4. A nonempty FetchEvent.clientId dispatches only to that originating client.
   Missing clients refuse with 404; never fall back to unrelated tabs. The first
   reply is bounded to 15 seconds and losing/finished/cancelled ports are closed.
5. Real sessions still select audio, honor HTTP range requests and support media
   metadata/playback. A foreign root worker stays unchanged on refusal. Legacy
   narrow registrations are preserved. Repeated sessions reuse owned control.
6. Native WebRTC unavailability remains an explicit product refusal before root
   registration; tests label stream branches NOT_APPLICABLE rather than simulate
   a constructor or silently skip import failures.

The worker is a readable MIT derivative; the official browser module is unchanged.
`Clients.claim()` controls eligible documents within scope and changes the
controller asynchronously (MDN Clients.claim and ServiceWorkerContainer.controller;
https://developer.mozilla.org/en-US/docs/Web/API/Clients/claim and
https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerContainer/controller).
The worker activation event retains the claim promise via waitUntil.

Assumptions and limits:

- Trust normal browser service-worker identity/scope semantics and same-origin
  application code. A concurrently executing arbitrary foreign registrar cannot
  be atomically excluded by the browser registration API. A script URL check is
  ownership evidence, not runtime code-integrity attestation.
- The application has one active torrent server per document (RadioWidget
  cancels/replaces the previous session). Multiple independent library servers
  inside one document are outside this contract.
- Empty-client-ID legacy/direct-navigation requests retain upstream broadcast
  compatibility and its first-response limitation. The controlled app-page
  guarantee requires a nonempty initiating client ID; no universal isolation or
  peer-to-peer availability claim follows.
- A page within an existing more-specific legacy scope may remain controlled by
  that registration; readiness refuses after the bound rather than deleting it.
  Ordinary app routes in the checked journeys are outside that old prefix.
- Local verification uses only loopback synthetic content and no external
  BitTorrent/WebRTC peers, trackers, credentials, artist content or production
  state. A library webseed can count as a peer internally. Local WebKit is not
  executed on this unsupported macOS; pinned CI runs all three projects.
- No crash recovery, malicious-message, arbitrary peer, all-browser liveness or
  whole-program refinement proof is claimed. Assurance is TESTED for finite
  unit/protocol/resource and actual browser cases, with deliberate fault controls.

Implementation: torrentWorker.ts, torrentAudio.ts and public/webtorrent-sw.min.js.
Regression controls: torrentWorker.test.ts, Node worker-routing/resource tests,
and e2e/torrent-audio against the actual Vite-compiled helper and byte-exact
browser bundle. quality-repo enforces Node tests; UI quality enforces Jest;
CI persona-web-e2e enforces the browser suite without retries. Reports bind
commands, raw exits, source hashes, counterexamples and limitations to a commit.
