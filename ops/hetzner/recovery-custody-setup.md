# Recovery storage and key custody setup

TDF needs a recoverable database, assets, private uploads and deployment
configuration even if the production host is lost. Diego confirmed on
10 November 2026 that independent backup storage and recovery-key custody do
not yet exist. Provisioning and production recovery remain pending; the
requirements below define what the operator must arrange and demonstrate.

## Storage destination

Use persistent storage outside the production host, controlled through an account
whose administrative recovery does not depend on that host. A second directory,
Docker volume or account on the same server cannot satisfy this requirement.
The destination must retain the encrypted coordinated bundle and the trusted
record binding its release identity, plaintext/ciphertext hashes and exact size.
An upload acknowledgement alone is insufficient: an independently authenticated
reader must retrieve the retained bytes and verify them.

The draft coordinator currently supplies a bounded byte-channel protocol. It
does not provision a storage service, authenticate its endpoints or implement a
production storage account. A separately administered SSH destination is a
candidate transport to qualify, not an installed backup service. An object-store
destination would require its own reviewed adapter and acceptance checks.

Give the production transfer identity only the access needed to create a new
release object. Keep storage administration and retention deletion outside that
identity. Protect retained objects against overwrite; demonstrate the selected
service's actual access behavior rather than relying on an account label.
Define retention, deletion, quota and alert handling before production use.
Capacity planning must include measured bundle size and the chosen retained
history. An exhausted destination must reject release preparation safely.

## Recovery key

The existing encryption primitive accepts native X25519 age recipients and one
plaintext native age identity. Use the repository-pinned tooling from
`recovery-tools.json`; other identity/plugin formats need a separate implementation.

Generate the production recovery identity on the custodian's trusted workstation,
outside the production host. Keep its durable copy in a separately recoverable
private vault or offline store. Record only its public recipient and recipient
hash in the release configuration. Never put the private identity in Git,
ChatGPT, PR descriptions, logs, container arguments or environment variables.

The custodian must be able to retrieve the identity when production and its
credentials are unavailable. During an authorized isolated recovery, provide it
through the primitive's private-file/inherited-descriptor boundary, with no
logging or persistent copy in the application deployment. Retain access to each
old identity for as long as its encrypted backups are retained; a rotation must
not make the last usable recovery bundle unreadable.

## Operator decisions to record

Store secret values in the selected private custody system. The operator record
should contain references to those values, not the values themselves.

| Input | Required record |
| --- | --- |
| Storage owner | Account and responsible operator, independent of production access |
| Destination | Provider, region, endpoint and private namespace or path |
| Endpoint trust | Verified SSH host fingerprint or the reviewed transport's equivalent |
| Access | Separate transfer, retrieval and administrative access references; overwrite/deletion controls |
| Key custody | Custodian, private-store reference, public age recipient and recipient hash |
| Service recovery | How to regain storage and key access without the production host |
| Recovery policy | Retention/deletion rules, recovery point and recovery time targets, quota and failure notification owner |

Destination, account, custodian and policy values are unresolved. Preparing this
record does not create an account or incur storage charges.

## Acceptance sequence

1. Authenticate both endpoints independently and exercise their actual access
   restrictions with synthetic data. A production writer must fail to overwrite
   or delete the retained test object.
2. Generate a temporary synthetic identity, encrypt a synthetic complete bundle,
   durably upload it, then close the upload connection. Retrieve through a fresh
   authenticated connection and compare exact ciphertext bytes and size.
3. Remove only the test's original local archive/ciphertext copies. Recover the
   synthetic identity from the proposed custody process and decrypt the retrieved
   copy. Compare the trusted plaintext hash and replay file contents/metadata.
   Exercise wrong-key, corrupt-byte, unavailable-store and interrupted-transfer
   rejection. Synthetic success does not qualify production secrets or data.
4. Establish the real custody record and verify recovery-key usability without
   the production host. Protect both the recovery key and the independently
   trusted bundle record; age encryption alone does not authenticate the sender.
5. Through the reviewed coordinator and continuous writer exclusion, capture the
   actual database, public assets, private uploads and configuration coherently.
   Encrypt, retain and independently retrieve that same bundle. Preserve the
   original deployment and every unresolved journal intent.
6. Replay only into admitted disposable storage. Verify cold PostgreSQL identity,
   complete file metadata, migration apply/replay, usable recovered configuration,
   restricted application behavior and application-before-database cleanup.
   Record fresh source/image/host-bound evidence before release eligibility.

The final production sequence also depends on current-main integration of draft
PR #495, qualified writer fencing and recovery, and review of the observed SMTP
image override. A storage account or decryptable archive alone cannot authorize
a production restart, migration or deployment.

Implementation boundaries:
[encryption](../../formal/system/recovery-envelope.md),
[file recovery](../../formal/system/recovery-files.md),
[physical PostgreSQL recovery](../../formal/system/physical-recovery.md), and
[release journal](../../formal/system/release-journal.md).
