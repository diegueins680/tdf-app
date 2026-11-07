# Music source health: backend delivery stage

This candidate delivers server prerequisites for universal playback. Frontend runtime and browser tests match protected main f615c862b; only generated declarations change. The unified engine and its idle/resolving/ready/playing/paused/recovering/exhausted/error model remain in PR #670. This stage does not claim that production has the new player.

The database stores pending, valid, temporary and invalid observations by source kind, identity and version. Ingestion queues new sources; the trusted derivative pipeline records validated output. A resumable worker inserts at most 100 missing identities and claims one due source per loop. Claims use 90-second leases and current-version fencing; expired workers cannot commit over replacement sources or claims.

Checks use public HTTPS, bounded DNS children, pinned validated addresses, rechecked redirects, at most 1 MiB of bytes and bounded probe/decode commands. Object-store credentials never follow redirects; nested containers cannot open local files or network URLs. Known Drive viewer/login HTML is inconclusive. These checks sample media rather than proving whole-recording decoding or live provider access.

Two matching permanent failures at least 30 minutes apart suppress the exact current source. Temporary errors never newly suppress valid content. Successful validation restores eligibility. Legacy featured references obey the same health gate while saved preferences remain available to their owner. Application recovery retains observations and invalidity policy; old SQL is not rewritten.

Canonical assets retain existing publication, territory, preview and download authorization. Legacy source grants recheck current artist visibility and health and return no-store responses. Diagnostic reads and revalidation derive the caller from authentication and check owner/team/admin scope. Revalidation schedules a current identity without clearing invalidity or accepting a caller URL. Diagnostics omit locations and credentials.

Diagnostics use stable source-kind/ID cursors, default 100 and maximum 500 rows per request. Each page rechecks permission; verdict timestamps do not change the ordering. A traversal is not a database snapshot: newly inserted identities before a cursor appear after refresh. Previously delivered bytes or metadata cannot be revoked retroactively.

Assumptions: a trusted database and worker, protected source integration, supported media tooling and eventual timer/process scheduling. No uninterrupted playback guarantee follows from this stage. Full provider, device, real object-store ingestion and production recovery/rollout qualification remain separate. Executed checks are empirical evidence, not mathematical proof.
