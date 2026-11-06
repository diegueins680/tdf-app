# Retrieved bundle admission: DEPLOY-ENVELOPE-001 / DEPLOY-JOURNAL-001

`ops/hetzner/coordinated-restoration-inputs.py` binds the output of the completed
capture/encryption/retrieval chain to verified plaintext for later isolated replay.
It is a library inside the pending recovery coordinator, with no production CLI,
shutdown, key provisioning, network, Docker, migration or stage-completion action.

`Inputs.targets()` runs before the caller publishes its `restore-isolate` intent.
It requires the exact completed six-stage prefix, live reservation, stopped source
observations and admitted backup mount topology. It reopens private canonical
capture, encryption and retrieval receipts and verifies their existing journal
observations, contexts and content hashes. Capture binding, encryption recipient,
both predecessor receipt hashes and exact round-trip nonce/content/status must
agree. Boolean lookalikes do not match the canonical transfer record. Only the
fixed retrieved ciphertext path is admitted; the original encrypted copy cannot
stand in for retrieval. The target hash binds all three receipt hashes and both
content digests, rather than an arbitrary caller-provided pathname.

The caller passes that hash to its normal journal `perform('restore-isolate', ...,
effect)`. Only inside that effect may `Inputs.prepare(context)` execute. It checks
the actual last pending intent, including its operation ID, nonce, plan and target
hash. Missing, stale or different contexts reject before decryption. This is not
an unresolved-intent resume API. The one-shot live object rejects retries; existing
output is never adopted or overwritten. The same fixed global journal, physical
reservation and writer-fence assumptions as capture still apply.

The existing sealed pinned age primitive decrypts only the retrieved bytes, using
the caller's native private identity. It requires successful authenticated
decryption and the separately recorded plaintext digest/size. The helper checks
the resulting file again, matches the reviewed encryption/decryption tool hashes,
and repeats source, topology, receipt-chain and intent admission. It exclusively
publishes and fsyncs a private receipt before a final input/plaintext admission.
Errors retain private partial output and the caller's pending intent. Original
plaintext and original ciphertext are not needed once their trusted journal
receipts and the retrieved bytes are available.

The returned private `captureBundle`, `binding` and `operation.output` supply
`recovered-application-content.prepare` inside the same pending effect. Replay
must independently recheck archive content/metadata against that manifest; a
successful earlier hash is not permission to consume later changed bytes.
Plaintext is deliberately **not** a journal observation: the returned nested
context and explicit `isolateStageComplete: false` cannot satisfy `perform`'s
completion protocol. A coordinator must still replay the trees, authenticate
disposable creation, verify cold PG17 state, start and qualify the isolated DB,
apply/replay migrations, run the restricted application and safely remove its
disposables before it may publish a full restore-stage observation.

Caller responsibilities also include the fixed control directory, continuous
writer exclusion, fresh capacity admission before intent and container startup,
endpoint authentication, independent storage, real recovery key custody, secret
usability and stage deadlines. The ten-copy capacity budget already accounts for
decrypted plaintext; this helper adds no extra archive copies. Its receipt leaves
key custody, independent storage and database/application recovery explicitly
unverified. Digests detect substitution under the stated OS/root/fsync trust;
they do not authenticate a privileged writer who can replace journal authority.
The full receipt, paths and capture manifests remain private.

Portable tests use actual journals/private files and an explicit mocked crypto
seam to exercise context, chain, mount, corruption, late-writer, output-adoption
and fsync rejection. A single chain-guard mutation must make the named broken-chain
case fail. Linux root tests additionally compose actual six-tree capture, pinned
age, a synthetic local socket round trip and metadata-preserving replay, including
loss of both original archive copies, a wrong recovery identity and rejection of
plaintext-only stage completion. These are synthetic implementation controls;
they establish neither actual off-host custody nor production database recovery.
The PR repository lane and image lane explicitly require Linux root execution
for those controls; a skipped platform cannot qualify them. Local macOS reports
the three real-age cases as skipped.

The existing bounded ReleaseJournal model covers intent ordering, not receipt
semantics, file replay, cryptography or this Python composition. No new refinement
or whole-system proof is claimed. Both requirements remain PARTIAL. The original
legacy stop policy, production admission and its image identity are unchanged;
this helper supplies no exception to them or permission to deploy.
