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

Cron example (runs every 6 hours):
```
0 */6 * * * cd /opt/tdf-app && /usr/bin/node scripts/sync-artist-drive.js >> /var/log/tdf-artist-drive.log 2>&1
```
