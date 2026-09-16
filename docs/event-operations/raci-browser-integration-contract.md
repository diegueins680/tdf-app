# Real RACI browser integration contract

Test-only increment, dependent on PR 388 (`061553d085d3106a9c8877ca08ccb3e2420cdd09`).
No new domain transitions, permissions, schema migrations or production configuration.
The previously checked `RaciWebEditor`, `RaciEditorContext`, RACI command and session-fence
models remain applicable; this harness tests their implementation boundary, not a new proof.

- BI01: Only an owned disposable PostgreSQL container is used. The entrypoint replaces any
  inherited DSN, rejects remote Docker endpoints, freezes its local Unix socket, binds
  database/API/UI to IPv4 loopback, and cleans up owned resources.
- BI02: Browser cookies authenticate through the production `/session` getter and event
  authentication context. Production event handlers execute the existing migration chain.
  Synthetic database identities are fixtures, not mocked HTTP success responses.
- BI03: An explicitly reviewed command updates the real assignment, revision, receipt and
  audit exactly once. Dropping the response **after** a successful real request leaves the
  UI uncertain. Explicit retry sends the identical key/body and retrieves `replayed: true`.
- BI04: A reader cannot prepare a write; an assignment alone cannot grant task visibility.
  Revoking a token after review prevents mutation. A stale revision returns an explicit
  conflict instead of overwriting newer responsibility data.
  The canonical API client's recognized session-expiry event must also discard private
  task/review data after a real inactive-token response; this is not an uncertain-retry UI.
- BI05: Browser traffic outside the owned UI origin is blocked. Only session and event API
  paths proxy to the owned server. Other APIs are unavailable, not simulated successes.
  No worker, login, provider, email, production configuration file or external API starts.

Fixtures intentionally cover only the tables used by these routes. This is a real integrated
RACI subflow, not a complete production-schema/deployment, login, CORS, native-mobile,
notifications or payment rehearsal. Existing full-schema migration tests remain separate.
Fault injection may discard a genuine response but must never manufacture a receipt.
Final evidence and exact commands belong in the increment report after execution.
