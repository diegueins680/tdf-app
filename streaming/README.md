# Historical radio streaming reference

This directory is an unverified local infrastructure example, not the canonical
production deployment or an approved publishing service. Native radio/event
creation returns503 pending a verified provider authorization binding. Configuring
endpoint URLs does not enable it. See the authoritative
[streaming containment contract](../formal/system/streaming-admission.md).

The old example combines an unauthenticated `source: publisher` MediaMTX path,
RTMP, WHIP and public HLS with the same UUID as playback identity and publishing
key. Its HTTP/private-host examples also conflict with the backend's HTTPS and
public-host URL validators. The prior claim that a returned stream key alone
secured ingest is withdrawn. Do not deploy this example as a production fix.

`docker-compose.streaming.yml` and `mediamtx.yml` are retained as historical
implementation evidence. They do not establish current provider availability,
authentication, TLS, image compatibility, revocation, storage durability or media
flow. A replacement requires a provider-specific reviewed contract and executable
positive/negative authorization and media-delivery tests before activation.
