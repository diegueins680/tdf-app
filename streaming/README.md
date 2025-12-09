# Radio Streaming Infrastructure (Reference)

This is a self-hosted stack to enable user broadcasts via the `/radio/transmissions` endpoint. It exposes:
1) RTMP ingest for broadcasters.
2) HLS output for listeners (served over HTTPS).

## Layout
- `docker-compose.streaming.yml`: boots `mediamtx` (RTMP ingest + HLS out) and a lightweight static server for HLS.
- `mediamtx.yml`: RTMP/HLS settings.

## Quick start (local/test)
```bash
cd streaming
docker compose -f docker-compose.streaming.yml up -d
```

Ingest (broadcaster OBS/etc):
- Server: `rtmp://localhost/live`
- Stream key: `<key>` (use the value returned by `/radio/transmissions`)

Listen (HLS):
- `http://localhost:8080/hls/<key>/index.m3u8`

## Environment variables to align backend
Set these in `tdf-hq` env before deploying:
- `RADIO_INGEST_BASE=rtmp://<your-host>/live`
- `RADIO_PUBLIC_BASE=https://<your-cdn-or-host>/hls`

Then redeploy the backend so `/radio/transmissions` returns correct ingest/public URLs.

## Production notes
- Put the HLS output behind a CDN with HTTPS.
- Consider transcoding to AAC for wider browser support.
- Secure ingest with per-stream keys only (already provided by `/radio/transmissions`), and restrict RTMP to authenticated sources if possible.
