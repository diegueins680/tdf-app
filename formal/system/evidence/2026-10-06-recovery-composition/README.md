# Synthetic recovery composition evidence

These sanitized receipts contain hashes, counts and source provenance only. No
customer data, private manifest paths, synthetic identity or ciphertext is included.

- `stopped-storage-linux.json`: clean revision4f329368b, actual new64MiB Docker
  fixture; retained mount namespace/root across stop, full metadata replay,
  unsupported xattr/running/restarted negatives and owned-container cleanup.
- `bundle-files-linux.json`: sandboxed actual six-tree archive controls. This
  runner recorded source hashes rather than a Git revision; the hashes correspond
  to bundle implementation/tests at5b558a9a4. It does not attest a clean checkout.
- `synthetic-encrypted-ssh.json`: source-hash-qualified working-tree test, expressly
  `sourceWorktreeDirty=true`. Pinned age encryption, durable operator retention,
  retrieval of the same ciphertext and exact six-tree metadata replay; original
  trusted-hash tamper and wrong-release controls pass. Synthetic secret only.

The first stopped-container attempt failed because the fixture inherited the
PostgreSQL image stop signal; explicit inspectedSIGTERM repaired the fixture.
Independent review then found a double-leading-slash source-overlap bypass in the
bundle; canonical-path rejection and an actual filesystem alias regression fixed
it. Removing that new guard reproduces the intended regression failure.

These are empirical scoped results. They do not establish actual production writer
fencing, coherent production backup, real secret/key recoverability, PostgreSQL
recovery from the combined bundle, release eligibility or deployment. The canonical
contracts retain those open obligations. Exact-candidate repository/formal/backend
validation is recorded separately and cannot be inferred from these receipts.
