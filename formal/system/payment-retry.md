# Provider retry admission: PAY-RETRY-001

An uncertain provider outcome must retain its original attempt/intent and block a
fresh charge on every rail for the checkout. A confirmed no-charge outcome permits
a new attempt and provider reference, subject to current checkout authorization
and payable state. A failed UI/request response alone is not no-charge evidence.
Exact authenticated replays recover the original attempt; changed immutable
parameters conflict. Closed checkouts cannot initiate a new payment. Captured
history cannot be downgraded by a later decline/cancellation callback.

The idempotency domain is `(provider, merchant_account_ref, operation, key)` in
`CheckoutStore.beginPaymentAttempt`; replay must also bind checkout, environment,
amount and currency. `ProviderExecutionStore` additionally binds operation request
hash, method/resource and lookup capability. Authentication on a replay remains
mandatory: possession of a key alone is not authority. The checkout-level gate
serializes admission across different providers and keys.

## Specification conflict and authority

The old checkout table in `docs/revenue-platform/formal-model.yaml` treated
`failed` as terminal, required `draft` as the initial state and omitted direct
verified payment from `awaiting_payment`. It is now retained under
`historical_state_machines`, outside active transition discovery. The pure
`TDF.Commerce.StateMachine.transitionCheckout` is an independently tested reference
helper with **no production caller** at this audit revision. It also differs from
that table and cannot establish runtime conformance.

Current runtime creation starts at `holding` for an unaccepted quote or
`awaiting_payment` for an accepted payable snapshot. `CheckoutStore`,
`ProviderExecutionStore` and `PaymentRuntimeStore`, together with database guards,
implement the live boundary. Existing PostgreSQL retry tests explicitly require a
fresh intent only after confirmed no-charge, reject alternate rails during
ambiguity and preserve closed-checkout/replay evidence. These corroborate the
payment reliability requirement; the conflicting old table must not forbid safe
recovery or authorize a new charge on a timeout. This scoped decision does not
approve every SQL transition or declare the complete checkout lifecycle reconciled.

## Bounded model

`ProviderRetryAdmission.tla` abstracts one authorized checkout, two canonical
idempotency scopes (which can represent different rails), two immutable payload
classes, and at most two attempt IDs. States per attempt are unused, active,
ambiguous, confirmed no-charge and captured. Checkout creation closure, immutable
receipts, replay responses and historical captures are modeled separately.

Each action represents an atomic committed transaction, so interleavings model
concurrent requests only under the implementation's actual locking/rollback
assumption. A crash before commit is stuttering; a lost response after commit can
replay the retained receipt. Provider verification, monetary equality, current
session/capability validation and operation-specific key normalization are assumed
preconditions, not proven by this model. A confirmed no-charge outcome is assumed
final and authentic. Network/provider truthfulness and external duplicate effects
are outside this local-state abstraction.

Safety invariants require one unresolved-or-captured attempt, immutable key
binding, matching replay payload, no creation after closure, retained capture
evidence and at most one captured attempt. All actions may stutter; no fairness is
assumed and **no liveness claim** is made. The model does not prove eventual
provider response, deadlock freedom, multi-checkout independence, partial captures,
refunds, actual authorization, unbounded history, or SQL refinement.

Five controlled mutations independently remove ambiguous-attempt exclusion,
key uniqueness, replay payload binding, closure admission, or terminal evidence
protection. Each configuration must fail its named invariant; parser/tool failures
are not accepted as counterexamples. The positive configuration must pass.

## Implementation evidence

`ProviderRetrySpec` exercises actual PostgreSQL transactions through the runtime:
concurrent different-key exclusion, concurrent same-key replay, alternate-rail
blocking during ambiguity, confirmed-no-charge recovery, closed-checkout denial,
immutable bindings and conflicting late outcomes. Execute through
`scripts/test-provider-retry-runtime.sh` against its owned empty local/CI database.
Its environment-gated tests are **not executed** by an ordinary Hspec run without
`TDF_PROVIDER_RETRY_DATABASE_URL`. Record the integration command and candidate
SHA separately; a passing bounded model never substitutes for that execution.

The formal runner invokes the positive and all five negative configurations.
Traceability fingerprints connect the requirement to the model, runtime and
PostgreSQL tests. Review of that correspondence remains an explicit obligation.

Primary research supports, but does not prove, these choices: [PayPal's idempotency
contract](https://developer.paypal.com/api/rest/reference/idempotency/) requires
operation-scoped IDs, permits reuse within provider-specific retention, and warns
that simultaneous same-ID calls need not both succeed. TDF therefore retains local
receipts and serializes admission; it does not assume unlimited provider retention.
[PostgreSQL17 row locks](https://www.postgresql.org/docs/17/explicit-locking.html)
provide transaction-duration exclusion. Consistent lock order and actual concurrent
integration tests remain necessary; adding a lock is not a whole-system proof.
