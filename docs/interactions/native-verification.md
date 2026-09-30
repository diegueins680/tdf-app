# Installed native verification

The mobile repository's `Interaction simulator build` and `Interaction Android
test build` workflows bundle JavaScript with the isolated `127.0.0.1:18128` API.
Their artifacts are test-only: OTA is disabled, Android uses its repository debug
key, and the iOS simulator uses ad hoc signing with a synthetic keychain prefix.
The latter is necessary for SecureStore to retain login across a cold start;
an entirely unsigned simulator app cannot verify session restoration. Xcode embeds
simulator entitlements at link time; applying iOS entitlements to a macOS code
signature after the build is not equivalent. See the [Apple simulator linker specification](https://github.com/swiftlang/swift-build/blob/swift-6.3-RELEASE/Sources/SWBApplePlatform/Specs/Embedded-Simulator.xcspec).
Neither artifact validates App Store/Play signing or production HTTPS association.

Install the artifact on a dedicated simulator/emulator. For Android, run
`adb -s DEVICE reverse tcp:18128 tcp:18128`. Use Java 21 for Maestro and a UTF-8
locale. Keep production credentials out of the test environment.

From the root repository, with the built backend executable and matching mobile
checkout available:

```bash
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 \
TDF_INTERACTION_SERVER_BIN=/absolute/path/to/tdf-hq-exe \
TDF_INTERACTION_SERVER_PORT=18128 \
TDF_INTERACTION_NATIVE_DEVICE=DEVICE \
TDF_INTERACTION_NATIVE_APP_ID=com.tdfrecords.app \
TDF_INTERACTION_MOBILE_ROOT=/absolute/path/to/tdf-mobile \
bash scripts/interactions/test-http.sh
```

Use `com.tdf.records` for Android. `TDF_INTERACTION_MAESTRO` can name an absolute
Maestro executable. The wrapper refuses an occupied API port, creates an owned
database, runs the real HTTP suite, and tears down its server/database on exit.
Fixture credentials and Maestro logs stay in a private temporary directory.
Screenshots contain synthetic discussion content only.

The native runner creates a random local-only password, resets the fixture
owner's reaction, and drives login, reaction and comment creation. It verifies the
comment through the real API, creates a reply as another fixture account, dispatches
the existing notification worker, and cold-opens the exact notification. The
second journey edits the parent, deletes its body, and collapses/expands replies.
Final API assertions verify the tombstone and retained reply relationship.
The `--resume` option is solely for diagnosing a creation journey that has already
persisted `nativeBody`; routine verification should run the complete wrapper.

An installed test-artifact pass is separate from release verification. Before
activation, verify the signed store build's HTTPS comment links, account
restoration and association responses against the deployed canonical hosts.

For a resource-constrained local machine, dispatch the existing CI workflow with
`force_backend=true`, `publish_backend_artifact=true`, and `native_android_run`
set to a successful Android test-build run. The run must match the root checkout's
mobile application source and the designated test-build workflow. A later gitlink
may reuse an artifact only when every changed path is a Maestro YAML file under
`e2e/interactions/`; any application, native, dependency or build-script change
requires a new artifact. CI downloads the backend
from its own successful build, boots a dedicated Android 35 device on a Linux
runner with hardware virtualization, and runs the same isolated HTTP/native
journeys. It retains only synthetic screenshots; credentials remain ephemeral.
The aggregate quality check includes this job when explicitly selected.

The `Universal interaction verification` workflow also supports an explicit
hosted iOS run. Supply `native_ios_run` with a successful mobile simulator-build
run and `application_sha` with the full immutable root commit to qualify. The
macOS runner checks out that exact commit, verifies that the artifact's source
tree equals its pinned mobile tree, verifies the original app signature, builds
the backend with the project's Stack resolver, and starts an isolated PostgreSQL
17 database. It runs the same complete HTTP/native runner on a dedicated iOS 18
simulator. No rebundling, resigning, production credentials or live services are
used. Only synthetic screenshots are retained; fixture credentials and database
contents remain on the disposable runner. This optional job is not selected by
ordinary pull requests, and must pass explicitly before claiming iOS qualification.
