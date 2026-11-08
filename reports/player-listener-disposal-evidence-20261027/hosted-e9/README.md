# Hosted qualification of the E9 listener-disposal candidate

This archive records actual execution for source e9f23483c7d4164a4551e6a90d6b82e6b09fd13a. Native, UI and persona job logs bind checkout aacc4cecb7ddb42ac2869930af6513caa1e5d1d5; its complete tree equals E9. These results do not qualify a later documentation commit as newly executed runtime code.

Primary CI succeeded: 3,578 UI tests in 311 suites, 50 native cases (40 decoder fixtures, five denial cases and five explicit mocks), 594 core persona passes with 22 existing skips, plus 10 rollout and 25 discovery passes. The disposal case passes in all five executing browser profiles, including Linux WebKit. Generated media and synthetic authorization remain explicit boundaries.

The ancillary Safe Install first attempt remains a recorded failure: 3,577 passes and one unchanged non-player workspace test waiting at a lazy-loading fallback. One retry on unchanged code and commands passes 3,578/311. Its exact timing cause remains unverified; no assertion, timeout or retry configuration was changed.

The preview smoke uses anonymous contexts, blocks writes and third-party requests, and proves one idle source-free engine with no page errors at two viewports. Its stable entry embeds E9. It does not prove authorized or production playback.

Original parent reports and the seven local-evidence leaves remain unchanged except the expressly appended current delivery report. Gate observation preserves 19/20 passes, FIFO admission pending PR628, independent review still required, and auto-merge off. Vercel's rate-limit failure is retained separately; no gate is waived.

Extract hosted-e9-evidence.tar.gz into an owned empty directory, run python3 build-archive.py --members . --output replay.tar.gz, and compare summary.json. This packaging replay uses no service or network. Credentials were checked with three limited high-confidence patterns, not an exhaustive security audit.
