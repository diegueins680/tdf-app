# WebTorrent browser distribution

TDF loads the official WebTorrent 2.8.5 browser ES module as a Vite URL asset.
`tdf-hq-ui/src/vendor/webtorrent/webtorrent.min.js` is byte-for-byte the published
`dist/webtorrent.min.js`, SHA256
`98f65d27e64f5872427544e53566bba011ace28af867ac44fc8fc08c2f92bcae`.
The production loader uses the existing worker URL `/webtorrent-sw.min.js` at
root scope `/`, with the same library API. The readable MIT worker derivative
waits for actual page control, routes controlled requests to their initiating
document and bounds the initial reply wait. Its separate contract is
`formal/system/torrent-worker-control.md`.
The npm package's unused Node graph is removed; `@types/webtorrent` remains a
development-only type dependency. The browser library module remains byte-exact; the service worker is a TDF derivative.

`browser.json` binds the artifact, public notices and provenance inventory.
`bundled-provenance.json` conservatively inventories all 99 original source-map
entries: 83 files from 58 npm package identities, 11 WebTorrent/polyfill files,
and five webpack runtime entries. Matching published source candidates were
compared after CRLF-to-LF normalization only, using integrity-verified archives.
Matching versions are candidates, not uniquely identified build resolutions.
The source map is upstream publisher evidence, not proof of binary derivation
or runtime reachability. The webpack compiler version remains unknown.

Public notices are served at `/third-party/webtorrent-NOTICES.txt`. They retain
the supplied MIT, ISC and Apache-2.0 notices, embedded attributions and README
license declarations. Nine matching package archives have MIT declarations but
no standalone notice file. No missing copyright holder or year is invented.
This evidence does not certify legal completeness.

Run `node scripts/check-browser-vendor.mjs` for deterministic artifact,
license-preservation, dependency-absence and inventory checks. Run it with
`--audit` to query the npm advisory bulk endpoint for WebTorrent plus every
matching-source candidate version and every previously locked version of each
mapped package. Safe Install runs this live check alongside the unchanged
low-and-above production npm audit. HTTP/network/invalid-response failures and
applicable low-or-higher advisories reject; no browser advisory allowlist exists.
Candidate-version scanning cannot clear unknown exact build resolutions,
compiler versions, unpublished advisories or all runtime inputs. The Node `ip`
package is absent from the source map; its expired npm exception is removed.

For an update:

1. Obtain the exact official npm metadata and tarball over verified TLS; verify
   the archive SRI against the reviewed lock/registry metadata. Do not run its
   lifecycle scripts. Preserve archive URL, SRI, SHA256 and upstream git revision.
2. Copy only the official browser module. Verify its bytes and map from that
   archive. Reconstruct the complete map partition; compare published candidate
   sources and licenses from integrity-verified archives. Retain version
   uncertainty and all supplied notices, including the webpack runtime.
3. Update the manifest, provenance, public notices and reviewed constants in the
   checker together. Do not omit previously queried versions without review.
4. Run the negative controls, unchanged npm audit, browser component live audit,
   UI quality checks and actual browser import/service-worker lifecycle checks.
   Keep the existing worker URL/scope and API behavior unless separately specified.
5. Publish source-bound receipts with limitations, obtain independent review and
   pass required CI before integration. Never restore the expired exception or
   silently substitute a CDN/Node entry to make a gate pass.
