# Bundled browsers for persona CI

SYS-CI-BROWSER-001 is an inferred test-infrastructure contract. Before persona
journeys start, the official Noble image must be pinned by SHA-256, its version
must equal all three locked and installed Playwright packages, and its bundled
metadata must identify that version and image. Chromium, Firefox and WebKit must
each launch and close successfully. A missing browser or mismatch fails the job.

The full persona journeys, default-off deletion rollout and discovery/navigation
stages remain required, with their original assertions, time limits, artifacts,
retention and aggregate quality dependency. There is no download or APT fallback.
The image replaces installation work; it does not replace any application test.

The observed predecessor installation ran for over 40 minutes before a single
recovery attempt. Its retained log shows slow Ubuntu APT downloads and retries,
not an established application defect or deadlock. A later retrieval candidate
again spent over 30 minutes in browser installation after all backend checks
passed. An immutable image removes this per-job package-installation phase;
registry availability and image-pull time remain external dependencies.

[Official image guidance](https://playwright.dev/docs/docker) explains the bundled
browsers, required version alignment and shared-memory option. The exact
[1.59.1 Dockerfile](https://github.com/microsoft/playwright/blob/v1.59.1/utils/docker/Dockerfile.noble)
and [metadata writer](https://github.com/microsoft/playwright/blob/v1.59.1/packages/playwright-core/src/server/registry/dependencies.ts)
define the browser directory and bundled identity. The manifest and Linux amd64
configuration were fetched from Microsoft's registry with digest verification.
The existing Node 22 setup remains authoritative for repository commands.

Negative controls cover mutable/foreign images, digest syntax, package drift,
stale metadata, browser-path redirection and failed launches. Mock launch controls
do not establish actual Linux browser availability. Fresh complete hosted execution
of the preflight and all three existing browser stages remains required. No browser
assertion or budget is waived, and no production or release authority is granted.
