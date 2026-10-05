# Mobile session cache isolation — PRIV-CACHE-002

The shipped Mobile lineage uses a new QueryClient for each authentication
occurrence. Explicit login/logout, even batched with equal tokens, advances the
occurrence. Changed token, Party or normalized role/module/flag scope resets the
query subtree before children commit. Equivalent session refreshes preserve it.
`AuthQueryProviders` is the production composition used by `AppProviders`.
Authentication survives the reset; analytics, settings, theme, onboarding,
experiments and route query consumers are beneath it. Persisted settings retain
their existing ownership and lifecycle; this contract does not audit those stores.

Clearing a shared cache is insufficient: at Mobile eac261c, an actual navigation
favorite response completed after screen unmount, logout and another Party login.
The callback repopulated the shared actor-independent navigation key, so the new
Party saw the prior favorite and received prior visit timestamp/count metadata
without issuing its own GET. This synthetic actual-screen reproduction establishes
a P2 privacy defect, not a demonstrated booking/message leak or production exploit.

Retired callbacks can complete and write their captured old client; a fresh client
prevents those writes reaching the new account. Regression tests require both
favorite and visit callbacks actually to execute, inspect the old cache, then
assert the new screen/cache remain scoped to the current Party. They also cover
batched same-token relogin, A-B-A, changed roles from the real session refresh and
equivalent refresh without remount. Independent StrictMode checks exercise both
delayed mutation cases. Storage, transport, routing and analytics are synthetic;
this is not a physical-device or complete native startup test.

`SessionCacheIsolation.tla` and its shared-client/actor-reuse negative controls
apply to the `PrivateProjection` abstraction. The web contract documents the three
occurrence/four slot bound, atomic scope selection and correctly scoped response
assumptions, and exclusions. Mobile does not claim refinement of the model's
`CurrentExpiry` property. No fairness/liveness, universal concurrency proof,
cross-device revocation or cancellation of accepted backend effects is asserted.
The React state-identity and TanStack QueryClient primary-source decisions in
`research.json` apply here too; no new persistence, offline queue or retry is added.
