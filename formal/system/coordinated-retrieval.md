# Journaled retrieval: DEPLOY-ENVELOPE-001 / DEPLOY-JOURNAL-001

`ops/hetzner/coordinated-retrieval.py` connects the recorded capture/encryption
chain to the durable ciphertext round-trip primitive. The caller owns the channel
descriptors exclusively, authenticates both endpoints and establishes independent
storage. This library cannot establish those facts from a pipe or socket. Its
receipt therefore leaves endpoint authentication, storage independence, key
custody and database recovery explicitly unverified by the helper. The full
coordinator must supply those additional admissions before release eligibility.

Admission requires the exact completed prefix through encryption, no pending
intent, no new writes, retained physical reservation and unchanged source/host
observations. Read the private canonical capture and encryption receipts through
their completed journal records. Require matching bundle identity, plaintext,
recipient and capture-receipt hash. Hash the actual encrypted source before intent.
The retrieval target hash binds that ciphertext size/hash and encryption receipt.

After durable intent, repeat source admission and receipt checks. Send precisely
the recorded ciphertext and receive the retained bytes using the same nonce and
trusted digest. The transport primitive requires exclusive private creation,
fsync, reopen and full hashing at each receiver. Rehash both original and returned
copies. Reapply source admission, exclusively persist and fsync the context-bound
private retrieval receipt, then recheck admission before journal completion.

Any channel, content, persistence or closing-admission failure leaves the intent
pending and retains private partial artifacts. A round-trip acknowledgement or an
existing receipt alone never completes the journal. The helper cannot retry an
uncertain effect, decrypt, restart a database, remove evidence or restore data.
It offers no cryptographic sender authentication beyond the caller's transport
and trusted encryption evidence. Root/Python/journal integrity, honest filesystem
sync and excluded noncooperating writers remain explicit assumptions.

`python3 scripts/test-coordinated-retrieval.py` runs six Linux-root cases with real
journals, six-tree archives, pinned age, synthetic identities and socket peers.
The passing path decrypts the **returned** ciphertext and compares exact bundle
bytes. Denial cases cover changed source ciphertext, changed encryption receipt,
corrupted returned bytes, a late writer and receipt-sync failure. These local
peers establish byte-chain conformance, not production endpoint independence.
The image workflow runs the real cases; macOS reports explicit skips. Existing
ReleaseJournal bounded models cover ordering, not transport or filesystem
refinement. The underlying recovery-transfer suite separately tests truncation,
deadlines, nonce/framing drift and private-path substitutions.
