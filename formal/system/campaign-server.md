# CAMP-SERVER-001 — Durable musical campaign authority

Authority: Diego's explicit 2026-10-29 implementation instruction, including any registered user, own accounts, TDF-admin approval, all four networks with truthful capabilities, 5 GB Free and proposed paid tiers, server execution, scoped notifications, no spend. This is a new scoped requirement, not a proof of whole-system conformance.

The canonical TDF session supplies actor id and strict administrator status. No campaign data are returned to another actor without explicit membership. OAuth state is single-use, expires in ten minutes and is bound to the current actor and administrative authority. Tokens are encrypted at rest; signed provider media URLs carry only an expiring capability.

Campaign/piece approval is bound to the current versions. Changing material content invalidates approval. A TDF destination needs its own current admin approval; DMs need separate consent. A transaction claims at most one local job. Every final supported provider publication boundary rechecks active state and approvals. Pausing cannot undo an already admitted external effect. A crash or timeout after admission is uncertain and cannot automatically start a second publication. The provider is not assumed transactional with SQLite.

YouTube uploads remain private until HD is confirmed and the authorized reviewer records Studio checks against the exact uploaded video id/version. Studio restrictions are not inferred from a successful API upload. Unsupported native operations remain explicit assisted actions. Provider acknowledgments do not by themselves establish independent public playback; `verified_at` remains unset for those responses.

FFmpeg derivatives preserve musical time and originals. Automated RMS selection is a draft, not semantic recognition of songs or lyrics. Rendered media attach only to the corresponding unapproved draft. Reservations serialize per-user quota and global unfilled reservations; finite host disk capacity can block an otherwise permitted tier. Deleted media retain audit metadata and cannot be removed while referenced. Generating a plan does not schedule publications.

Evidence: actual SQLite concurrency/HTTP boundary tests, OAuth identity readback, isolated real-media server qualification, client-disconnected jobs and restart observations, snapshot restore test. The bounded model explores 1024 abstract states and three counterexamples; it is **not** a refinement proof of HTTP, Python, SQLite, FFmpeg, OAuth, provider behavior, operating system, backups or arbitrary interleavings. Local tests and server qualification retain their actual source manifests; CI and protected frontend integration are separate gates.

External prerequisites remain Google app verification, Meta/TikTok account and app approvals, unsupported native operations, optional notification channels, full-host failover and capacity provision before selling large tiers. Operational details: [operations](../../docs/campaign-platform/operations.md).


## Private off-host campaign backup

The custodian must be a current strict TDF administrator and the original configured owner. Drive authorization uses `drive.file`, verifies the TDF account identity, checks already available capacity before writes, and accepts only an owner-only private folder. Neither a different administrator nor a non-administrator receives backup credentials or recovery controls. Private-key possession is established by a short-lived, one-use RSA-PSS signature computed locally; the private key is never sent to the server.

Each immutable object is encrypted in authenticated blocks under a fresh AES-256 key, wrapped with RSA-3072 OAEP/SHA256. Versioned headers, block index and length are authenticated; truncation, reordering, altered source blocks and incorrect keys are rejected by finite tests. Persisted resumable state is itself encrypted and binds source hash, size, recipient and transfer. Source blocks are rechecked before reuse of a nonce. Completed objects discard local symmetric transfer material. This is a cryptographic implementation tested with the declared library, not an independent cryptographic audit.

An independent resource-limited systemd process uploads a consistent campaign SQLite snapshot, service configuration and every ready asset, then an encrypted manifest as the commit marker. Acknowledged Drive ranges resume without restarting the master; revocation or disablement stops subsequent chunks. Three process interruptions block the request for review. Uploading/partial assets are explicitly excluded. Remote metadata verification and actual independent decryption/restore have separate receipts. A successful campaign restore does not establish core PostgreSQL recovery or production failover; restored publication queues must remain stopped pending reconciliation.

## Facebook publication boundary

Text/link, single/multiple photos, normal video and native Reels have protocol implementations. Video/Reel stages retain durable provider identity, wait for readiness, recheck exact-version and TDF authority, and record a final-publication intent before the write. Readiness alone never means publication. An uncertain final request is quarantined and never replayed; the read-only verifier can inspect it. These stages have controlled protocol/SQLite tests; no connected Facebook Page or live publication qualification is claimed. Known configured TDF identities cannot be relabeled as personal accounts to bypass admin approval.
