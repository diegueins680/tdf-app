# Canonical music test execution

`SYS-MUSIC-TEST-ENTRY-001` requires the music domain, content, ERN432 and S3 unit
specifications to run in the canonical `tdf-hq/test/Spec.hs` suite. Merely listing
modules in Cabal or keeping `MusicReleaseSpecMain.hs` does not execute tests.

The aggregate suite registers four explicit groups: `music-unit-domain`,
`music-unit-content`, `music-unit-ddex`, and `music-unit-storage`.
`scripts/quality-backend.sh` reuses the compiled test executable and separately
requires each group with `--fail-on=empty`. Removing a group must fail the gate
even when thousands of other examples pass. The standalone Main is marked as a
development convenience; it imports the same specification modules.

The unchanged previous executable fails each of these four probes as empty.
Positive execution and those negative controls are separate evidence. Group
presence does not prove test adequacy or every repository test's reachability.
PostgreSQL runners, providers, devices and external integration tests retain their
own boundaries; a pending case is never a pass for an external runner.
