# Original PR633 CI failures at 7ddf

This is a separate immutable archive of attempt 1 for source
`7ddfbd73bf08d007b4658b700bb9cbe289a6b480`, base
`9b7aa4648d4158f6da84fbb5e794267cd4953177`, tested merge
`9edec4ee222e083bdfd8ebaed2860eb8c80f6542` (tree identity is in the summary).
The original QA receipt, complete persona/social logs, later root-fetched job
annotations and exact source social workflow are compressed without editing.
The annotations are separate later evidence; the original QA receipt's earlier
annotation-access limitation is preserved rather than rewritten.

Persona attempt 1 failed: 565 passed, 22 skipped, 1 failure. WebKit's phone cart
case exceeded its whole-test 30-second budget at the final `Cerrar carrito` click.
The underlying cause remains UNVERIFIED; this archive does not classify it as
fixed or harmless. Social's 63 focused tests passed, but browser journeys were
SKIPPED after the 15-minute job budget expired during dependency installation
after font-mirror delays. The job annotation explicitly confirms 15m0s.

The required composition with protected main 2e and subsequent fresh exact-head
CI are separate release work. Main composition is not a cart fix. No rerun of
7ddf, timeout/skip/assertion/workflow change, browser/SMT/Stack run or production
operation occurred while creating this archive. Existing assurance remains
BLOCKED; 45 compiled cases and 54 HTTP assertions retain their actual 43b source
identity and their separate synthetic-fixture limits.

`archive-member-hashes.json` binds each exact uncompressed member.
`artifact-hashes.json` binds the archive, summary and documentation; it excludes
itself to avoid a hash cycle. Verify member hashes before using the evidence.
The archive was round-tripped and compared byte-for-byte with all original
inputs. It does not download the 70 MB persona browser artifact or certify media
bytes, production playback or CDN behavior.

Original jobs: [persona failure](https://github.com/diegueins680/tdf-app/actions/runs/40395683983/job/121828274802) and [social timeout](https://github.com/diegueins680/tdf-app/actions/runs/40395684105/job/121827958993).
