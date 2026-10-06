# Dependency security boundary — SYS-DEPENDENCY-001

The canonical version policy is [dependency-security.json](dependency-security.json).
`node scripts/check-dependency-security.mjs` checks root; `--mobile` additionally
requires the Mobile lockfile. Repository and Mobile quality gates invoke the
applicable checks. Regression tests lower every reviewed floor and demonstrate
rejection, including nested packages, missing Mobile and unreviewed series.
These are version checks, not formal models or vulnerability reachability proofs.

The scoped compatible update retains Expo54, React Native0.81.5, React19.1 and the
shipped Mobile lineage. It updates leaf dependencies within their supported
series. `tmp` requires an explicit override because Cucumber pins0.2.3; an old
nested lock entry was re-resolved by npm, not manually assigned new integrity
metadata. The expired React Router RSC audit exception was removed: the current
full report no longer contains it. The existing dated browser WebTorrent
exception remains unchanged.

[dependency-audit-2026-10-05.json](dependency-audit-2026-10-05.json) records actual
post-update lock hashes, full-scan exit1 and source advisory URLs. That historical scan reported 43
root packages (33 high/10 moderate) and 77 Mobile packages (60 high/17 moderate). Counts include affected
parents; they are not independent vulnerability or exploitable endpoint counts.
The production-only audit has a different scope and cannot override this result.

Remaining exposure decisions live in the policy's `remaining` records. In
particular, the URI decoder is reached by React Navigation/Expo query parsing.
Upstream0.5.0 exports an ES module, while installed query-string7 requires a
CommonJS callable. A blind override is not admitted as a compatibility repair.
Metro image-size1.2.1 similarly cannot be blindly replaced with its changed2.x
export API. Forge verification exists in Expo signing tooling; no reachability
clearance follows merely from labeling it a build dependency. Upstream forge1.4.0
and braces3.0.3 remain unpatched as observed2026-10-05.

Do not expand audit allowlists to hide these findings. Reassess upstream fixes
and actual call paths before distribution or accepting untrusted assets/signing
material. Source inspection is scoped evidence, not a proof that every dynamic
import, native build or provider-supplied input is safe. Primary forge report:
https://github.com/digitalbazaar/forge/issues/1149; decoder maintainer advisory:
https://github.com/SamVerschueren/decode-uri-component/security/advisories/GHSA-vcc3-ghjq-m6fr.

The October 6 refresh additionally identified source-map-js indexed-map offset
denial of service and compression premature-close memory retention. The current
compatible patch pins `source-map-js` 1.2.2 in both locks and `compression` 1.8.2 in
Mobile, with matching reviewed version floors. Root source-map-js is marked
development-only by the lock. Mobile installs both transitively through its
Expo/Metro tooling; the lock alone is not a universal runtime-reachability proof.
The existing floor regression tests reject the prior vulnerable versions.

Primary advisories: [source-map-js](https://github.com/advisories/GHSA-68fv-2mgg-jv7q)
and [compression](https://github.com/advisories/GHSA-vc2v-76pw-4v95). New
[sprintf-js](https://github.com/advisories/GHSA-hp3w-g68c-fv3c) and stream-json
findings remain in the explicit exposure register. No audit exception was added
or broadened, and this patch does not upgrade Expo, React Native or Jest.

[The October 6 full scan](dependency-audit-2026-10-06.json) records the patched
lock hashes: 48 root packages (33 high/15 moderate) and 82 Mobile packages
(60 high/22 moderate), with both scans returning exit 1. The fresh pre-patch scans
reported 49 and 84 respectively; the two patched advisories no longer appear.
These counts include affected parents and do not establish release clearance.
