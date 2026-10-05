# Shipped Mobile CI evidence

`SYS-CI-001` owns the exact pinned Mobile validation workflows. Main and shipped release branches run release checks, unit tests, signing/artifact guards and Expo doctor. A cancelled run is not a pass. Preserve the source revision and workflow attempt for each result.

Datadog is conditional on configured repository secrets and excludes fork pull requests. Its job can complete after explicitly reporting missing credentials; that is **not evidence that synthetic tests executed**. When configured, critical errors and missing tests fail the provider step. Read the step result before claiming external behavior. Do not make deterministic local conformance depend on this external service.

The root CI workflow regression suite parses these files from the initialized, exact Mobile gitlink. This checks workflow selection and failure policy, not the correctness of the hosted executor or provider.
