# Backend build authority and package trust

`SYS-BUILD-001`: `tdf-hq/stack.yaml` and its lock select the Haskell toolchain;
`tdf-hq/tdf-hq.cabal` is the active hand-maintained package declaration. Builds
and tests use Stack. The retired Hpack description is preserved under
`docs/archive/backend-package-legacy.yaml` as historical evidence only. Its
version, library and executable declarations do not describe the shipped system.
Do not reintroduce it into the active package or regenerate Cabal from it.

`DEPLOY-BUILD-001`: both backend image recipes must authenticate Debian archive
metadata/packages and enforce metadata expiry. Signature, expiry, proxy and
mirror errors stop image construction. They do not authorize insecure repository
or unauthenticated-package overrides, including in a temporary build stage:
build-time compromise can alter the shipped executable without access to secrets.
`apt-get update --error-on=any` also rejects transient fetch failures instead of
continuing with available indexes. Repair the network, trusted keyring or reviewed base image before rebuilding.

`node scripts/check-build-trust.mjs` checks this narrow source contract. Its test
suite mutates the actual recipes to admit unsigned repositories, unauthenticated
packages, expired metadata, later overrides and missing explicit policy; all must
be rejected. The repository gate runs both checker and controls. This is a
conservative source check, not shell semantics verification: computed commands,
base-image configuration, external tools and registry contents remain outside its
abstraction. A clean CI image build with normal APT verification is required.

No universal supply-chain claim follows. Mutable base-image tags, upstream
vulnerabilities, registry signing/attestation and reproducible Haskell binaries
remain separate audit obligations. An untrusted package must never be accepted
merely to make those image gates pass.
