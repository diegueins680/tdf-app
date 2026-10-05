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
post-update lock hashes, full-scan exit1 and source advisory URLs. Root reports43
packages (33high/10moderate), Mobile77 (60high/17moderate). Counts include affected
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
