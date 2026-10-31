# Browser dependency boundary — SYS-WEBTORRENT-BROWSER-001

The music playback loader consumes the official WebTorrent browser bundle,
not its Node server/tracker implementation. The previous full npm package added
`webtorrent -> torrent-discovery -> bittorrent-tracker -> ip@2.0.1` to the
production dependency graph. The only production audit exception expired at
2026-10-31 00:00 UTC, causing the existing low-and-above audit to reject. The
upstream advisory has no patched `ip` version:
https://github.com/advisories/GHSA-2p57-rm9w-gvfp.
This establishes an audit/dependency defect, not a demonstrated TDF attack path.

The corrected boundary must satisfy these acceptance criteria:

- Load exactly the original official browser ES module, with its reviewed hash,
  through Vite. Preserve the worker URL, scope, session lifecycle and API calls.
- Do not install the unused Node WebTorrent/discovery/tracker/ip packages,
  including npm aliases, in the root workspace lock.
- Preserve the existing production audit threshold. Remove the obsolete expired
  exception without replacement or extension.
- Bind the browser artifact to official archive provenance, the complete
  conservative source-map inventory and publicly distributed license notices.
- A live advisory check must cover WebTorrent and all mapped package candidate
  versions (including previous app-lock versions), reject applicable low-or-higher
  advisories and fail closed on failed or malformed registry responses.
- Deterministic checks must reject corrupted bytes, omitted source identities,
  candidate versions or notices, old Node imports/dependencies and weaker policy.

The implementation and controls are in `scripts/check-browser-vendor.mjs` and
`scripts/__tests__/browser-vendor.test.mjs`; `quality-repo.sh` runs the static
checks and Safe Install runs the live scan alongside `audit-ci`. The reviewed
artifact/update procedure and qualified provenance limits are in
`third_party/webtorrent/README.md` and its machine-readable manifest/inventory.

Assurance is TESTED for these finite controls, not PROVEN. The broader dependency
security requirement remains PARTIAL. Upstream source maps may describe code
before tree-shaking; matched versions do not uniquely establish build resolution,
and the webpack compiler version is unknown. No complete supply-chain,
reachability, vulnerability-free or legal-completeness claim follows. Real peers,
trackers, paid services, provider state and production playback are outside the
isolated browser lifecycle check. Fresh evidence must bind to the candidate
commit; advisory results expire as the external feed changes.
