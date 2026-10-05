# Web session cache isolation — PRIV-CACHE-001

Every authenticated security-scope occurrence owns a new QueryClient and mounted
application subtree. Scope changes include login/logout, token replacement,
server bootstrap and identity/role/module/feature changes. Returning to the same
Party is a new occurrence, including batched logout/login before a render. Theme,
toast and query consumers are beneath that boundary in the production
`SessionProviders` composition. Existing query retry/staleness defaults remain.

Old query completion and old mutation callbacks may finish, but their captured
client cannot populate the next session's cache. Retired clients are cleared;
actual client replacement and remounting provide isolation, not a claim that all
HTTP requests or external effects can be canceled. Component state is reset at
the boundary. Tokens are not used as React keys or persisted by this cache.

The API client captures a request-occurrence epoch before sending. A later401/403
that denotes invalid session authentication may expire only that same occurrence.
An old A request must not expire B, or a later A session with equal identity/token.
The original caller still receives its error. SessionProvider already fences late
bootstrap/onboarding results separately; this repair retains those checks.

Two local tests at9fca2c091 using the actual SessionProvider and TanStack Query
reproduced forbidden A-data-under-B rendering: a fresh cached response and an
in-flight response completed after account switching. Both used an actor-independent
internal-feedback key and synthetic data; neither queried production or proved a
full-browser exploit. The corrected tests require B's own query, different client
objects, and isolation even when an actual late mutation callback writes into its
captured old client. Additional tests cover A->B->A, batched same-actor relogin,
stale401 delivery, and actual production theme/provider composition in StrictMode.

## Bounded model

`SessionCacheIsolation.tla` abstracts three ordered session occurrences A,B,A,
one asynchronous operation per occurrence, four cache slots, and arbitrary
operation completions/authentication failures interleaved with switches. A
completion also represents a mutation callback's cached response. All provider
responses are assumed correctly scoped at the backend. A session switch and
client selection are atomic at the abstraction boundary, justified separately by
the synchronous render selection and component tests; no compiler-level
refinement proof is supplied.

`PrivateProjection` requires the visible value to belong to the current occurrence
or be absent. `CurrentExpiry` rejects stale-request expiration. No fairness or
liveness property is asserted. Bounds exclude unbounded sessions, cross-tab or
cross-device propagation, backend revocation latency, storage outside QueryClient,
manual global stores, malicious scripts, cryptographic properties and external
mutation cancellation. Three negative configurations intentionally share one
client, reuse clients by actor, or authorize expiration by actor instead of epoch;
each must violate its named safety invariant, not merely fail to execute.

This fixes an observed client privacy bug; frontend hiding still never replaces
backend authorization. Mobile has a separate token-change cache clear mechanism
and is not covered by this web model. Its full late-mutation parity remains open.
