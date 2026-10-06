# Social boundary integration — 2026-09-17

This candidate integrates the already implemented, inactive compatibility boundaries
from #390, #397, #402, #409 and #415, alongside #391's account-scoped chat client and
#394's verification record. All source commits and attribution are retained through
normal merges. Main at `5c84736ff5727502a77413b24f6a4ccc55a89274` is a parent.
The two documentation conflicts retain both the later implementation decisions and
the independent chronological check evidence. Historical pending statuses in those
packets describe their dated snapshots, not this candidate's current check state.

The wire APIs, directory privacy fixes, worker fixes, registered migration manifest,
feature flags and mobile gitlink from main are retained. Social adapter SQL remains
additive and unregistered for automatic production migration. This integration does
not activate social enforcement; partial adapter installation remains fail closed.

Local validation of the combined candidate: strict catalog audit (1,126 candidates,
zero unreviewed/stale decisions), complete repository quality, 27 focused chat/fan
UI tests, app TypeScript and UI build passed (357,175 gzip initial JS bytes).
Native PostgreSQL 16 full-schema qualification applied main's 103 registered
migrations plus the social adapters and passed reapply/pause and preserved-write
checks. FanEffects TLC passed its positive model and four specific negative controls.
TLC jar 1.7.2, Java 21.0.12.1; this is a separate run from the sources' Java 17 evidence.

The first local catalog run lacked the mobile submodule and reported 97 stale
fingerprints. Initializing main's exact gitlink fixed the setup; no catalog decision
or audit rule was changed. An initial UI command named a nonexistent selector test;
the corrected command ran all three intended files successfully. Sandbox socket
restrictions blocked initial PostgreSQL/TLC runs; their subsequent authorized runs
completed. No failed attempt is counted as a passing check.

Hosted backend, full formal/refinement, browser and integration checks must qualify
the published head before merge, with an independent approval and no unresolved
blocking discussion. Retain the source PRs until the replacement is merged and
verified. The broader notification provenance, member-profile privacy, delegated
identity, moderation/lifecycle, pagination and native cutover work remains open;
see `fan-integration-next.md`. No deployment command was issued.
