# Music purchase creation replay

`PAY-MUSIC-PURCHASE-REPLAY-001` governs `POST /music/purchases` at the
`createMusicPurchase` boundary. It refines the order-creation portion of
`PAY-MUSIC-DOWNLOAD-001`. Provider attempt keys and payment outcomes have separate
contracts; this repair does not make music commerce eligible for activation.

## Contract

The idempotency domain is `(authenticated buyer Party, Idempotency-Key)`. The
first accepted request creates one purchase, checkout, immutable line-item
snapshot and terms evidence in one database transaction. A later request must
pass current feature enablement, input validation and offer eligibility again.
The authenticated HTTP boundary remains responsible for session validity.

A successful replay returns that buyer's original order with its current state.
It does not create another order, checkout or terms acceptance. It must match the
original availability-rule UUID, checkout environment, declared country after
uppercasing/trimming, effective trusted country (`ZZ` when unknown), and trimmed
terms version. The immutable line-item snapshot is the binding authority. The
published offer supplies price/currency; these are not client-selected replay
parameters. Published offer edits are already rejected by the release triggers.

An eligible request using the same buyer/key with different bound inputs returns
HTTP 409 with no purchase effects. Missing/incomplete original binding evidence
also returns 409; it is never inferred from the newest request. An identical
request with the same key by another buyer is a separate purchase. Malformed,
disabled or currently ineligible requests can fail earlier with 400, 503 or 404;
an old receipt does not bypass those checks.

The existing unique constraint reserves the buyer/key. A request that loses
`INSERT ... ON CONFLICT DO NOTHING` reads the winner in a subsequent statement
and applies the same binding check as a request that found an existing order.
This assumes the application's default PostgreSQL READ COMMITTED transactions,
an immutable checkout line item, and successful commit of the winner. Database
errors propagate as failures and roll back the transaction. Higher isolation
levels may require an explicit retry of a serialization failure.

## State machine and recovery

Each request moves `new -> looked -> accepted | conflict | failed`. Between lookup
and insertion, another transaction can reserve the key. Acceptance either creates
the one original binding or returns a matching binding; a conflict cannot change
it. Lost responses can be retried under the same rules without another purchase.
This does not retain an exact HTTP response body: the order's state may advance.
Clients must choose a new key for a different purchase or terms version; they
must not automatically charge an existing order after a conflicting response.
No migration or destructive rollback is needed. Reverting the application repair
would restore unsafe replay acceptance, so it is not a safe conformance rollback.
Existing immutable financial/terms history is preserved.

## Traceability and evidence

- Implementation: `tdf-hq/src/TDF/Server/MusicReleaseCommerce.hs`, the existing
  buyer/key uniqueness constraint and immutable checkout line items.
- PostgreSQL: `tdf-hq/test/TDF/MusicPurchaseSpec.hs`, executed by the existing
  mandatory `scripts/test-music-purchase.sh` backend CI step. Tests cover unchanged
  replay, normalized inputs, changed terms/rule/countries, incomplete evidence,
  distinct buyers, repeated concurrent conflicting requests, and a schedule
  where a test-only advisory barrier proves both handlers reached INSERT after
  observing an absent key before either can reserve it. The barrier's private
  function/trigger are removed; no provider is contacted.
- Baseline negative control: the unchanged main handler at
  `2781ddc2a3c9fd661684742550918d653afdc0cf` returns success for changed terms;
  the new regression expects 409 and fails at that assertion.
- Formal: `MusicPurchaseReplay.tla` models two buyers, one key and two distinct
  payloads, with separate lookup and commit actions and repeated retries. TLC's
  intended configuration checks type safety, one creation per buyer/key, and
  buyer/payload preservation. Three configurations separately remove payload
  comparison, buyer scoping or atomic reservation and must fail the respective
  named invariant. The canonical formal runner requires all four results.

The model abstracts normalized request fields into a payload value and the
PostgreSQL unique-index wait/visibility into atomic reservation. It does not
prove the SQL translation, HTTP session revocation, arbitrary isolation levels,
provider effects or global payment safety. No fairness or environment progress
assumption is needed for these safety checks; no liveness property is claimed.
Request crashes, transaction rollback, finite database resources and expiry are
outside this model. Handler tests connect the payload abstraction to actual
stored fields; they are conformance evidence, not a refinement proof.

## Authority and research

The previous interpretation, "same buyer/key means replay regardless of body,"
is rejected: it can answer a different requested offer or terms with a successful
old purchase. The user's audit explicitly requires parameter-bound replay and
conflicting replay behavior. [RFC 9110 §9.2.2](https://www.rfc-editor.org/rfc/rfc9110.html#section-9.2.2)
defines idempotence in terms of repeated identical requests.
[Stripe's idempotency contract](https://docs.stripe.com/api/idempotent_requests)
provides a concrete parameter-mismatch rejection precedent. TDF adopts that
comparison rule, not Stripe's response caching, retention or provider architecture.

The existing OpenAPI inventory still records undocumented music operations.
This scoped handler contract does not close that separate API coverage debt.
