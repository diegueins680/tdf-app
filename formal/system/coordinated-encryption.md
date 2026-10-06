# Journaled encryption: DEPLOY-ENVELOPE-001 / DEPLOY-JOURNAL-001

`ops/hetzner/coordinated-encryption.py` connects the recorded six-tree capture to
the pinned age encryption primitive. It is a library without a deployment CLI.
The caller retains the original capture object, physical recovery reservation,
writer fence and trusted journal throughout the operation. Privileged host,
Python source and journal integrity remain environment assumptions.

Admission requires precisely the completed maintenance, writer-stop,
database-stop and capture prefix, no pending intent and no possible new writes.
The recipient hash must match the release plan. The private capture receipt must
be an owned single-link mode0600 regular file, contain canonical JSON and match
the hash recorded by the completed capture observation. Its complete operation
context must correspond to that journal intent. The receipt's bundle binding and
actual archive size/hash must match the current capture. A saved receipt without
a completed journal observation never authorizes encryption.

The encrypt intent binds the exact plaintext digest/size, recipient hash and
capture receipt hash. After durable intent, recheck source admission and the
recorded receipt, execute sealed pinned age bytes, verify the resulting plaintext
binding and rehash the source archive. Reapply source admission, exclusively
persist and fsync a private context-bound encryption receipt, then perform a
closing source admission before the journal can record that receipt's hash.

Failure preserves artifacts and the unresolved intent; no automatic retry,
overwrite or phase advancement is available. Receipt persistence followed by a
crash does not imply a completed journal operation. Encryption success reports
off-host retention, key custody and database recovery as false. This helper does
not generate a production key, transfer data, decrypt, restore, restart services,
resolve interrupted intents or authorize deployment.

Run `python3 scripts/test-coordinated-encryption.py`. Portable checks reject
unobserved and context-mismatched receipts. The image workflow supplies pinned
tools and runs the Linux-root cases with real temporary journals, actual capture
archives and synthetic keys. Those cases check byte-identical authenticated
recovery, recipient/plaintext/receipt substitution, late process rejection and receipt-sync failure with
retained ciphertext and pending intent. They establish implementation behavior
under those fixtures, not production custody or a cryptographic proof. The
existing bounded ReleaseJournal model covers stage ordering only; encryption,
filesystem durability and source exclusion remain outside its abstraction.
