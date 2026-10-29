# Official application icons

Status: approved by the product owner on 2026-10-29 for this visual correction.

The web application's browser and installed shortcuts use the existing official
TDF Records symbol from `tdf-hq-ui/src/assets/tdf-isotype.svg`. The symbol paths
and proportions are preserved. The retired text-only TDF monogram with a blue
underline is not an application icon source. The native Mobile release already
uses this official symbol and is unchanged by this correction.

`node scripts/generate-app-icons.mjs` renders the SVG with the repository's
Playwright Chromium. It writes the 1024, 512, 192, 180, 32 and 16 pixel PNGs,
the SVG favicon and a 64 pixel PNG-backed ICO fallback. A dark opaque canvas
and a centered symbol at 60% of its width preserve the Mobile appearance and
place the circular artwork inside the maskable icon safe circle. PNG color
management and antialiasing remain properties of the selected browser renderer.

The manifest declares 192 and 512 pixel PNG icons with `any maskable` purpose.
The page references the actual icon sizes and versions the URLs to refresh
previous browser icon caches. Existing bookmarks or OS-installed shortcuts
can retain their prior icon until the browser/OS refreshes it or the user
reinstalls that shortcut. This source change does not claim an App Store,
Play Store or physical-device icon update.

Validation checks the emitted files, actual HTTP responses, image decoding,
mask previews and rendered samples at their target sizes. These artifact
checks and browser screenshots establish the scoped visual result; they do
not constitute a general browser/OS installation refinement proof. Normal
protected review, integration and publication still apply.
