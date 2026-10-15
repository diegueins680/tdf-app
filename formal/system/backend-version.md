# Cached backend build identity — DEPLOY-VERSION-001

The canonical backend quality build must refresh compiled Git identity even when
the package's Haskell sources are unchanged and incremental objects were restored.
Runtime metadata overrides are not evidence that the compiled metadata refreshed.
The HTTP fixture retains its independent reported-commit check and executable hash.
These checks do not establish reproducible builds or authenticate source provenance.

Hosted candidate849b exposed this failure: the checkout was synthetic merge
`f3f5b87319f47098b4128f28537403cf30716b01`, but its freshly linked executable reported
`dev` and an October5 build time. The job restored cached objects and did not
recompile `TDF.Version`. The fixture rejected the mismatch before behavioral checks.
This is historical failing evidence, not a current passing result.

There are two caching boundaries. `quality-backend.sh` uses Stack's
`--force-dirty` for the local package so the build cannot be skipped solely because
sources match. `TDF.Version` uses module-local `OPTIONS_GHC -fforce-recomp` so GHC
reevaluates its Git/time splices. Unrelated modules and immutable dependencies can
retain their objects; this is not a global GHC force-rebuild option. A direct
developer Stack build without `--force-dirty` is not the canonical fresh-metadata
gate when only Git HEAD changed.

`scripts/test-backend-version-rebuild.py` copies the actual version module into a
small package using the same Stack snapshot/lock. Only Cabal's package-version
module is stubbed. It first builds without Git, reproducing cached `dev` metadata,
then creates commitA without changing Haskell source. The corrected build must
reportA. A README-only commitB must then reportB with the same Haskell files and
cache. The log must show version recompilation while the unrelated Main object's
bytes and modification time remain unchanged.

Two controls remove the Stack flag or module pragma independently. Each must
retain the known cached `dev` identity instead of the newly created Git HEAD;
unrelated compilation/transport errors cannot count as detection. The executable
receives no inherited metadata environment and `/app/COMMIT` must be absent.
Logs, identities, hashes and results remain in new private directories, including
failures. Deadlines terminate only the test's own process group. These fixtures
contact no application database or provider. They do not prove every arbitrary
Stack entrypoint, cache corruption scenario or privileged host environment safe.

Backend CI gives the probe a new workspace-owned evidence path and always uploads
only its result, synthetic identity JSON and case logs. Git repositories, compiled
binaries and Stack caches are excluded. Failed builds retain the same allowlisted
evidence; source revision and probe/module/snapshot fingerprints bind the receipt.
Probe Git discovery stops at its case parent, so an evidence directory inside the
root checkout cannot substitute the ancestor's HEAD for the initial non-Git cache.
The regression is also exercised beneath an existing checkout with absent parents.

The actual full backend build and HTTP fixture must still pass on the final
candidate, with its exact executable identity. No runtime override, source mutation
or removal of the revision assertion is an acceptable substitute.

Primary toolchain guidance: [Stack's local package dirty flag](https://docs.haskellstack.org/en/stable/commands/build_command/#-no-force-dirty-flag)
and [GHC recompilation checking](https://downloads.haskell.org/ghc/9.10.3/docs/users_guide/separate_compilation.html#recompilation).
Adopted selectively for the two observed cache boundaries; this is an empirical
implementation check, not a formal refinement proof.
