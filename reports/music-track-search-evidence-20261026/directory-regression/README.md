# Existing directory regression — retained environment failure

At b8a, root session74683 exited1 with **11 executions:10 passed,1 failed**,0 skips/retries/flaky cases. Firefox project-level grep replaces the root filter, so actual discovery is11 rather than the initial9 estimate. No filter/assertion/timeout was changed during the run.

Spanish Chromium desktop fails at screenshot line98 with ENOSPC, followed by filesystem write errors and30s timeout. Its later geolocation/map/Axe assertions were not executed. Existing serious/critical Axe assertions passed in the other10 cases. This is PARTIAL_ENVIRONMENT, not a green directory qualification, full WCAG audit or inferred product failure.

[qa-receipt.json](qa-receipt.json) preserves seven unchanged source fingerprints, the private configuration fingerprint, all11 outcomes, and the12 available original artifact hashes. The separate archive includes exact raw results/log/config/pre/post/QA receipt and actual exit1 marker. Large successful PNG/HTML artifacts are hash-listed without copying; no unavailable failed screenshot/trace is invented. No rerun was performed.

This run uses synthetic intercepted directory/session/favorite/music replies, including empty music results. It qualifies neither real search/audio/backend/production behavior nor the earlier native/HTTP results. All41 earlier packet leaves remain exact. The scoped archive and leaf manifests are independent; previous archives are unchanged.
