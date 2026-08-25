### Artist video sync (Google Drive → artist profile)

Script: `scripts/sync-artist-drive.js`

Env needed:
- `GDRIVE_CLIENT_EMAIL` and `GDRIVE_PRIVATE_KEY` (service account with access to the parent folder)
- `GDRIVE_PARENT_ID` (defaults to `1_ScZjmwmOuBX_325JgFocZ-QlfFEIMax`)
- `API_BASE` (e.g. `https://hq.example.com/api`)
- `API_TOKEN` (admin bearer token)

Runs:
```bash
API_BASE=... API_TOKEN=... \
GDRIVE_CLIENT_EMAIL=... GDRIVE_PRIVATE_KEY="-----BEGIN PRIVATE KEY-----\n...\n-----END PRIVATE KEY-----" \
node scripts/sync-artist-drive.js
```

What it does:
- Ensures one folder per artist under the parent Drive folder.
- Picks the most recent video in each folder, makes it link-readable, and saves the preview URL to the artist profile (`apFeaturedVideoUrl`).
- The Fan Hub shows the preview iframe on artist cards when `apFeaturedVideoUrl` is set.

### Daily artist inventory and enrichment

The reusable pipeline is `scripts/artist-enrichment.mjs`; the complete operating,
backup, deployment, and rollback procedure is in
`docs/artist-enrichment-runbook.md`.

- Fly performs internal discovery at 04:00 `America/Guayaquil` when
  `ARTIST_ENRICHMENT_ENABLED=true`.
- `.github/workflows/artist-enrichment-daily.yml` performs external research at
  10:00 UTC (05:00 Ecuador) and prevents overlapping executions. It only ingests
  media in an explicitly requested run that includes a rights-approved source.
- Manual workflow dispatch defaults to dry-run.
- Disable internal discovery with `ARTIST_ENRICHMENT_ENABLED=false`; disable the
  GitHub workflow to stop external research.
- Every run has a durable backend run ID, redacted checkpoint, counters, and
  error summary. Daily workflow artifacts are retained for 30 days.

Cron example (runs every 6 hours):
```
0 */6 * * * cd /opt/tdf-app && /usr/bin/node scripts/sync-artist-drive.js >> /var/log/tdf-artist-drive.log 2>&1
```
