# Recovery encryption: DEPLOY-ENVELOPE-001

`ops/hetzner/recovery-envelope.py` encrypts an already captured private bundle and
checks decryption against separately trusted plaintext/ciphertext hashes. It does
not create production identities, select a transfer destination, transfer files,
coordinate a snapshot or restore a database or file tree.

The production primitive supports Linux x86_64 with sealed memfd execution. The
age1.3.2 executable bytes are copied into a private anonymous file, checked against
the reviewed binary hash, sealed against writes/growth/shrinkage and seal changes,
and checked again. The child executes that held object through `/proc/self/fd`.
A hash checked before and after an ordinary pathname execution was insufficient:
a path substitution could run another program in between. A regression test
performs this substitution and requires actual encrypted output and valid recovery.
Other platforms fail closed; the Linux-only tests are visibly skipped on macOS.
Linux must also permit executable memfds; denial fails the operation without
changing kernel or namespace policy.

Only native X25519 age recipients and one plaintext native identity are accepted.
Plugin identities, SSH identities and password prompts are not supported. Private
identity bytes enter age through an inherited descriptor, never a command argument,
environment variable or log. Inputs are bounded, private owned regular files with
one link and no symlink components. Outputs are new exclusive mode0600 files.
Children receive a fixed environment, a120-second deadline and output-size limit.
Encryption rechecks input contents; decryption verifies transferred ciphertext
before execution and requires successful authenticated completion plus the exact
trusted plaintext size/hash before returning success. Partial decrypted bytes from
a rejected stream remain private; consumers must not use them without completion.

Age ciphertext authentication does not identify the sender: anyone with the public
recipient can encrypt another message. Therefore the caller must retain an
independently trusted capture/transfer record binding both full content hashes.
This helper is not a signed bundle protocol and does not protect against a
compromised privileged process or replacement of its own Python/policy source.
Successful records retain `offHost=false` and separate false restore flags.

## Toolchain and executable checks

`recovery-tools.json` pins the official age1.3.2 archive and extracted binaries.
Pins were checked against the publisher's GitHub release metadata. Independent
Sigsum proof verification is not claimed. The installer extracts only the two
expected regular members after archive and member verification and creates a new
private tool directory; it cannot overwrite an installation.

On Linux, create a private parent and run:

```sh
mkdir -m 700 /absolute/private/recovery-tools-parent
python3 scripts/install-recovery-tools.py --destination /absolute/private/recovery-tools-parent/age
export TDF_RECOVERY_TOOLS=/absolute/private/recovery-tools-parent/age
python3 scripts/test-recovery-tools.py
python3 scripts/test-recovery-envelope.py
```

`--archive /absolute/pinned-release.tar.gz` avoids the installer's network read.
The repository CI installs the pinned tools before its checks. Cryptographic
controls use temporary synthetic keys/data and the real age executable: corruption,
truncation even with adjusted transfer hashes, wrong identity, valid substituted
content, private permissions, path links, duplicate outputs, forbidden plugins,
static executable substitution, scheduled path substitution and immutable seals.
No production secret is needed. These are integration/security controls, not a
new cryptographic proof or a production recovery test.

The release coordinator must still capture a coherent bundle, protect a recovery
identity independently of the production host, encrypt, copy off-host, fetch those
same bytes back and restore them into isolated targets. A temporary synthetic
identity and local successful decryption do not establish key custody, retention,
availability, off-host recovery or release eligibility.


## Durable ciphertext transfer and retrieval

`ops/hetzner/recovery-transfer.py` provides the byte-channel portion of the
coordinator. The caller must authenticate both endpoints, own the pipe/socket
descriptors exclusively, and bind the supplied digest/size to its trusted
ciphertext receipt. The module cannot identify a remote machine, encrypt data,
select a destination, establish key custody or authorize production access.
Only ciphertext belongs on this interface; the byte-level tests deliberately
use synthetic opaque bytes and do not claim cryptographic recovery.

Every frame binds the operation nonce, exact integer size, SHA256 and protocol
phase. Canonical byte comparison rejects alternate JSON encodings, duplicate
fields, booleans/floats in place of integers and unexpected fields. Sources must
be private, owned single-link regular files under non-symlink directories. The
sender checks original bytes before sending, then rechecks transmitted bytes,
inode identity and the named path. The receiver creates an exclusive mode0600
file, hashes the entire stream, fsyncs file and directory, then reopens and hashes
that same named inode. Only then can it send the retained bytes back. The
originator verifies that retrieved file against its original trusted receipt;
no peer-supplied replacement hash can authorize completion.

Channel IO is nonblocking under one absolute deadline (at most300seconds).
A supervising process must separately bound potentially stalled filesystem IO;
this library cannot impose a hard deadline on a kernel fsync. Failed or truncated
transfers may retain private partial files. Retrying requires a new admitted
output path; neither an old partial file nor a duplicate nonce is implicitly
resumed. Lost final acknowledgement can leave verified bytes without a completed
receiver receipt. The release journal must retain that uncertainty, never replay
an external effect merely because a connection closed.

`python3 scripts/test-recovery-transfer.py` runs local real-file/socket controls
for full round trip, phase/nonce/digest/count/type rejection, malformed headers,
truncation, corruption, private output, path/hardlink admission, source changes,
fsync/read-back failure, silent peers and backpressure. CI requires these without
an external service. The separate opt-in command is:

```sh
python3 scripts/test-recovery-transfer-ssh.py \
  --identity-file /absolute/private/operator-ssh-key \
  --output-directory /absolute/existing/private/evidence-directory
```

That synthetic4MiB rehearsal pins the existing canonical SSH target and host
trust, and uses a transient Linux DynamicUser with private network/tmp and
protected system/home. It reads no production data or keys. It retains the local
synthetic copy and source-qualified receipt. A passing result proves the tested
transport path, not production backup, key independence, long-term durability,
restored application data or deployment eligibility. Honest fsync/storage, the
trusted Python/policy process and no concurrent privileged inode mutation remain
environment assumptions; there is no formal filesystem or network proof.
