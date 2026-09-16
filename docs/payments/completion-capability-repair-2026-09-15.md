# Complete-checkout capability repair

Datafast status verification records a historical `capture` attempt even though
its hosted debit is captured automatically. The canonical boundary now requires
verified `one_time` and `server_verification` for that provider/operation pair.
It keeps existing operation names, attempt identities, idempotency keys, receipt
checks and marketplace restrictions. PayPal and other capture operations still
require their real `capture` capability. No provider or method is enabled.

Public runtime availability now adds complete-checkout requirements before
routing, including PayPal capture and card server verification, even when API
callers omit required capabilities. Extra caller restrictions and all marketplace
connected-account/split/payout requirements remain in force. Missing credentials,
contracts, method-specific verification or environment agreement still deny a
route. The pure routing engine remains usable for operation-specific decisions.

Regression tests exercise the real route engine with method-scoped capability
evidence: Datafast confirmation is routable only with both supported capabilities;
PayPal remains absent until capture is verified; missing marketplace or caller
requirements deny the route. No schema, generated API or provider credential
change is needed. The provider sandbox and native-stack qualification gates remain.

Validation results are recorded after execution in the audit report. No test
result or production qualification is inferred merely from this code change.
