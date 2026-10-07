# Canonical public origin

Every public link TDF generates uses `https://www.tdfrecords.net`: event share links (web and Mobile), link-preview canonical/`og:url`, sitemap, legal pages, course landing links and Mobile store URLs.

`tdf-app.pages.dev` is the Cloudflare Pages project host, not a public URL. `functions/_middleware.js` answers it with a `301` to the same path and query on `www.tdfrecords.net`. That way links already shared and OAuth callbacks still registered on the old host keep working. Two exceptions are not redirected:

- Branch previews (`<branch>.tdf-app.pages.dev`), so preview builds stay testable.
- `/.well-known/*`, so app-link verification files remain reachable for old shared links that Mobile still accepts.

Host-recognition code (CORS preview origins, API-base inference, OAuth redirect validation fixtures) still knows the Pages hosts. That recognizes inbound traffic; it does not generate links.

Follow-up configuration outside the repo: register `https://www.tdfrecords.net/...` OAuth callbacks in Google Cloud and switch `GOOGLE_REDIRECT_URI` / `VITE_GOOGLE_DRIVE_REDIRECT_URI` to them.
