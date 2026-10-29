# Current promo terms for new legacy ticket orders

Parent requirement: `EVT-TICKET-001`.

`EVT-LEGACY-PROMO-SNAPSHOT-001`: before a new legacy checkout reserves
inventory or creates order/redemption/ticket effects, its quoted promo must
match the current, locked promo's event binding, discount type and value,
uppercase currency, validity endpoints, tier restriction, minimum purchase and
active state. A mismatch returns409 with no such effects. The lock covers the
check through order commit. Stored orders and idempotent replays retain their
original financial snapshots after later promo edits.

Authority: the existing legacy promo validators, `updatePromoCode` and discount
calculation, plus canonical `EventTicketCheckout.promoSnapshotUnchanged` and its
locked pre-reservation check. This is quote identity, not a new discount policy.
Unlike canonical snapshot comparison, the current redemption count and maximum
are deliberately excluded: `EVT-LEGACY-PROMO-LIMIT-001` permits changed caps
when the new claim remains admissible. The existing atomic current-column cap
predicate stays authoritative. Description and timestamps do not affect quote
identity; currency case alone is harmless.

The baseline actual-handler counterexample uses an unchanged Event lock as a
barrier. A buyer quotes a paid100/unit tier and10000bps promo, then waits. The
normal organizer handler commits a changed promo before that buyer is admitted.
Seven cases return200, paid-zero order, sold3, tickets3 and redemptions1 despite
changed discount value/type, currency, future start, past end, disallowed tier
or minimum301 above the300 base. Inactive already refuses; description-only
and currency-case edits are compatible. No test-side financial write, provider
request or email operation is needed.

The correction acquires Event -> optional terms -> Tier -> Promo, checks terms,
then reserves using the existing shared inventory authority. Inventory's later
Event/Tier accesses are already held by this transaction; ordinary promo edits
read Event without a lock and update Promo. Existing compensation, atomic cap
claim and order creation remain in the same transaction. SQLite fallback is
retained for existing local compatibility and is not a concurrency guarantee.

The actual Promo FIFO regression holds an unchanged promo, queues the organizer
first and buyer second, then releases. Removing only the new financial read's
FOR UPDATE must fail despite Inventory and the later atomic claim still locking:
the unlocked comparison can read the old promo before the edit commits. A
separate completed-order/replay regression preserves history after financial
terms change; the previous eight current-cap regressions remain applicable.

The bounded design enumerates one editor, one new claimant, nine independent
term dimensions with two values each and one changed dimension per input:
18 inputs,324 states,324 transitions and18 valid later historical edits. Nine
omitted-field controls, missing-lock and refusal-effect controls must fail their
named properties. This is BOUNDED_VERIFIED design evidence, not SQL/Haskell
refinement or proof of all cross-field combinations. Runtime behavior is TESTED
only when source-bound execution receipts pass.

Assumptions: valid initial rows, participating PostgreSQL READ COMMITTED writers
and an initially valid quote. Matching date endpoints does not recheck elapsed
wall-clock time after waits; current database-time expiry remains separate.
Alternate-isolation availability, old binaries, direct out-of-protocol writes,
HTTP authentication, provider execution and mixed-version rollout remain
separate obligations. No historical financial rows are rewritten. Hosted CI,
current-head approval, current-main composition and protected FIFO are required
before integration; no deployment or whole-platform proof is claimed.
