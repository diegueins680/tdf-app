# Meta webhook admission — AUTH-WEBHOOK-001

All four public POST routes share one signature boundary:
`/facebook/webhook`, `/instagram/webhook`, `/webhooks/whatsapp` and its supported
compatibility alias `/hooks/whatsapp`. GET verification challenges are a distinct
verify-token protocol and are not authenticated with a payload HMAC.

POST ingestion requires a configured nonblank Meta app secret and a valid
`X-Hub-Signature-256` HMAC-SHA256 over the exact raw body bytes. Missing/blank secret
returns503 before decoding or mutation, even when a caller supplies a signature.
A configured secret with a missing, malformed or incorrect signature returns401.
Only valid signatures reach envelope validation and existing ingestion behavior.
Development uses an explicit synthetic secret; absence is not an authentication
bypass. No new provider activation or production secret change is required.

The old shared helper returned success for absent configuration, explicitly
supported by a unit test and described as useful for development. No environment
guard limited that behavior. Optional environment normalization can produce absent
configuration in any deployment. AUTHORITY-030 rejects the bypass as a security
bug, rather than converting it into intended anonymous ingestion.

The isolated HTTP fixture starts additional servers with absent and blank secret
configuration, checks all four routes with and without supplied signatures, and
checks that deletion payloads create no Facebook/Instagram tombstones. Correctly
signed deletion fixtures against the configured server establish that these are
real mutating payloads; a signature over different bytes must reject without a
row. These fixtures do not send provider requests or automatic replies.

`APITypesSpec` tests the shared helper, absent/blank keys and supplied headers.
QuickCheck generates raw byte bodies, computes a valid HMAC and rejects that HMAC
after a byte is appended. This is empirical property testing over generated
inputs, not a proof of cryptographic collision resistance. The cryptographic
library, key secrecy and the transport boundary remain assumptions. Provider
replay, event ordering, token rotation, delivery idempotency and downstream worker
semantics are separate obligations.

A read-only environment-presence observation on 2026-10-05 found the canonical
production API configured with `FACEBOOK_APP_SECRET`. No value was emitted. This
limits the finding to unsafe missing-key behavior; it does not show that current
production accepted unsigned requests or that the provider key is correct.
