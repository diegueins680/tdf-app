Isolated follow response recovery qualification (fixtures only).
Requires Darwin sandbox-exec/clang, PostgreSQL16 binaries, Node22.14.0, exact npm-lock dependencies and Playwright Chromium.
Run from a CLEAN committed source with the exact Mobile gitlink initialized:
python3 reports/activation-follow-response-2026-10-26/run.py --mode rehearsal --source /absolute/checkout --expected-source FULL_GIT_SHA --binary /absolute/qualified/tdf-hq-exe
The native binary hash and fe179 revision are fixed and its entire backend input tree must match the candidate. /version is checked independently; it is never relabeled as a candidate build.
The runner creates its own loopback PostgreSQL221/API/web and refuses occupied ports. Only its own services are stopped and its own disposable DB is dropped. Provider and non-loopback API egress are denied. Browser external requests are blocked.
A successful real POST/DELETE response is discarded only after route.fetch returns success; separate scenarios prevent the write before commit or make the confirmation GET unavailable. This is fault injection, not a production outage or human pilot.
Output contains source identity, command results, current API state, exact SQL assertions and cleanup evidence. Synthetic pairs/events/first-values are not VAR, conversion, retention or user research. No production changes or provider transactions.
