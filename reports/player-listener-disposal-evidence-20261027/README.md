# Listener disposal qualification evidence

This packet qualifies the privacy-only runtime composed with protected main 1129112f89d3c00d33b8c86043a6bd197031dcff. It imports no universal-search or metadata-readiness code.

The unchanged native regression first fails in Chromium because the detached old element retains its source. Two desired unit tests independently fail on the same retained source. The minimal correction pauses that element, removes src, and calls load once. Corrected results: 87 player/identity/session tests across eight suites; one clean native run of 30 cases across desktop Chromium, phone-viewport Chromium and Firefox; full UI and static results in summary.json. The native run comprises 24 decoder fixture pairs, three denial pairs and three explicit mock smoke pairs. All 491 runtime/fixture inputs remain unchanged across that run. Account/catalog/authorization responses are synthetic; this is not live CDN or production playback evidence.

The archive contains raw before/after results, commands, fingerprints and source captures. Large baseline artifacts remain privately retained with hashes. Launcher/selector failures are recorded separately from actual product tests. Parent reports are unchanged. Existing formal guarantees are not promoted; no new proof is asserted. WebKit, physical devices, OS focus, server revocation and representative SLOs remain unverified.

For offline packaging replay, extract listener-disposal-evidence.tar.gz into an owned empty directory, run python3 build-archive.py --members . --output replay.tar.gz, and compare the SHA256 in summary.json. This starts no service and uses no network. The final merge commit, hosted CI, independent review, FIFO admission and deployment require separate evidence.
