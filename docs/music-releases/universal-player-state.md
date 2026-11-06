# Universal player: state, intent, and source health

One session-owned `PlayerProvider` owns the listening audio element, queue,
source resolution, position, volume and media-session controls. Routes issue
commands; they do not own an audio element. This guarantee covers native TDF
music and radio in one application document. External provider pages, other
browser tabs and operating-system applications are outside that ownership.

Playback intent is separate from media readiness. `explicitPauseRef` is the
user's pause latch; `autoplayRef` is permission for the current attempt to play.
Selecting another artist does not clear the pause latch. Play and explicit track
selection clear it; Next/Previous while paused only change the silent selection.
The same listener's explicit-pause latch is persisted with the queue; a reload restores it silently. Account changes dispose it.

| Model state | Engine/UI representation | Exit |
| --- | --- | --- |
| idle | No selected track, `idle` | Select a context/track |
| resolving | `loading`, current source authorization or metadata pending | Current authorization and native metadata, failure or 15s deadline |
| ready | Prepared media, `paused`, no explicit pause latch | User Play or permitted current autoplay attempt |
| playing | Current unpaused native `playing` event | Pause, waiting, end, error, newer selection |
| paused | `paused` plus explicit pause latch | Explicit Play/track selection |
| recovering | `loading` with bounded retry or source/track advance | Current result, timeout, pause or newer selection |
| exhausted | `unavailable`, no remaining eligible candidate | Explicit retry or another selection; artist UI offers radio |
| error | `error` or diagnosed unavailable/offline state | Explicit retry or another selection |

The transport uses a separate playback-request flag so Pause can cancel authorization or buffering before audio starts. A fulfilled `play()` promise alone never establishes playing. Autoplay rejection
retains the selection, reports paused and requests a Play gesture. Native events
must match current selection, source URL and generation. The old generation is
retired synchronously when recovery advances. Actor changes dispose listeners,
timers, grants, torrent sessions and media resources.

Each candidate has at most the initial request plus two transient retries with
1s/2s backoff. Source preparation and stalls have a 15s watchdog; repeated
waiting or nonadvancing time updates cannot extend it. Requests use cancellable
API deadlines. An attempt therefore terminates or advances; a finite queue is
exhausted without repeat/shuffle cycling. Offline errors retain a useful retry
action and do not condemn catalog media. No timer, background check, prefetch or
artist navigation may clear explicit pause. Artist exhaustion never commands
radio; the listener chooses its link and station.

Protected sources cannot fall back to an old unsigned URL. Canonical assets and
legacy release membership are reauthorized at selection. During visible playback,
a single cancellable check runs every 60s and on foreground return; denial stops
or advances the source. Browser background throttling can delay this check.
Previously delivered bytes and a still-live signed grant cannot be revoked by
the client. The server checks current publication, territory and access rules on
every new grant. Full downloads keep their separate entitlement gates.

The server stores health by kind, source ID and SHA256 source version. Ingestion
of derivatives from the existing decoding pipeline records its result; other
sources enter a bounded queue. The worker inserts at most 100 missing identities
and claims one due source per loop, using a 90s lease. Expired claims can be
reclaimed; an old lease or changed source cannot commit. Repeating the sweep
resumes existing content without a one-shot migration or unbounded transaction.

Checks use one HTTPS request chain with at most three redirects, at most 1MiB
of media, a 25s network deadline, bounded DNS children and 5s local probe/decode
commands. All DNS answers must be public, and TCP uses a checked address while
TLS still verifies the original hostname. Every redirect is checked again;
object-store credentials never follow redirects. ffmpeg/ffprobe receive only the checked sample through a pipe; nested media
cannot open local files or network URLs. Partial/inconclusive containers remain temporary. This is a bounded
sample check, not a guarantee that every byte of an entire recording decodes.

Only two matching permanent failures, separated by at least 30min, suppress
public media. Timeouts, server errors, authentication and transport failures do
not newly suppress valid media. Existing invalid verdicts persist until a
successful check. Valid observations reenable the exact current source.
Diagnostics omit raw URLs, queries, keys and credentials. Owner/team/admin
revalidation coalesces checks without clearing a verdict or granting access.
Public canonical sources and signatures use the same health policy; public legacy
reads remove confirmed-invalid links while owner records remain intact.

Assumptions: the database and worker are trusted, the browser implements native
media events, timers eventually run, and a finite queue is supplied. No guarantee
of uninterrupted playback is possible when all sources or the network fail.
Tests exercise concrete races, native media and database behaviors; they are not
a mathematical proof or a claim that this candidate is deployed. Physical-device,
provider-account and full catalog provenance qualification remain separate.
