# Atomic rider file publication

`MEDIA-RIDER-001` requires complete, exclusive publication of Live Session rider
bytes. The actor/request/content-derived final path and PostgreSQL request fence
remain in `TDF.ServerLiveSessions`; authorization and identity adoption rules are
unchanged. The canonical requirement row defines the transitions and exclusions.

`TDF.Storage.AtomicPublication` writes to an exclusive mode0600 temporary file in
the destination directory, flushes the Haskell handle, transfers descriptor
ownership and synchronizes file contents. Linux uses `renameat2` with
`RENAME_NOREPLACE`; Darwin uses `renamex_np` with `RENAME_EXCL`. There is no ordinary
rename/overwrite fallback. The containing directory is synchronized before a new
publication is acknowledged. Unsupported filesystems or synchronization errors
propagate as failures. The custom-sync entrypoint exists for fault injection;
production uses the fixed `fileSynchronise` wrapper.

The caller retains recursive directory creation for direct development and isolated
tests and synchronizes both created directory ancestors. Production independently
requires a persistent uploads bind through the entrypoint contract. An existing
final is compared byte-for-byte; differing legacy partial files still return409
and are retained for review. Matching bytes are synchronized along with their
containing directory before replay acknowledgement. This repairs the case where
a prior rename completed but its synchronization or acknowledgement failed.

The directory and its ancestors are trusted and stable. The helper does not
protect against a hostile privileged process replacing directories or files.
SIGKILL during writing can leave a private, single-link staging orphan; it does
not publish that orphan as a final. Cancellation after rename can leave a complete
final without a successful response. Neither case authorizes automatic deletion.
The exclusive rename avoids the hard-link publication window that would violate
the recovery reader's single-link rule. Storage hardware and operating-system
synchronization guarantees remain environmental assumptions; this is not a proof
of power-loss behavior on arbitrary filesystems or devices.

File publication and the PostgreSQL transaction are separate. Complete unreferenced
files can remain after database rollback. Existing malformed finals are not
repaired automatically, and email/provider effects are outside this boundary.

Run `python3 scripts/verify-atomic-publication.py` with the Stack-selected toolchain.
It compiles the actual helper and tests outside the source tree, exercises eleven
real-filesystem cases (including three injected synchronization failures), kills
one owned child during a staged write, and requires two compiled mutants to fail:
overwrite an existing final and expose a final before the writer completes. The
backend quality gate runs this verifier and the standard Hspec suite includes the
same filesystem examples. Native Mac evidence does not establish Linux support;
the Linux backend gate must execute the platform branch before release.
