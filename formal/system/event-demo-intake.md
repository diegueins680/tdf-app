# Quito event demo intake (EVT-DEMO-INTAKE-001)

A disabled or unreachable availability service gives no form or CTA. Public requests require a name, contact email and event description, with bounded input and optional normalized attribution. The API rejects invalid requests or missing owner configuration. Durable acceptance does not imply a confirmed appointment. Replays use the same idempotency key; changing its payload is refused. A distinct inquiry type keeps demo leads out of the school workflow and sends no email or WhatsApp. The private inbox requires both the configured owner and existing Admin permissions.

## Verification and admission

UI tests distinguish disabled intake, successful request and ambiguous retry. Generated API client and route registration must correspond to the supported contracts. Compiled PostgreSQL rehearsal, owner verification and the abuse-control review remain release blockers. No owner is inferred and no public launch is implied.

## Receipt namespace

The existing Ads endpoint still validates customer keys as ASCII letters, digits, underscore or hyphen, 16–128 characters. The Demo endpoint validates first, then adds the storage-only prefix `demo:`. The forward migration changes only the receipt CHECK constraint so it also accepts that disjoint internal namespace followed by a full valid key. It neither modifies existing receipts nor makes the colon valid at the Ads API. Rollback refuses to tighten the constraint while demo receipts exist; operators must not delete them to force rollback.

Reproduce the PostgreSQL constraint and concurrent-admission rehearsal with `python scripts/test-event-demo-request-namespace-postgres.py --pg-bin <postgres-bin> --pg-share <postgres-share>`. This applies the real migrations to a minimal disposable fixture; the four concurrent SQL transactions implement the receipt lock/lookup protocol and are not a full Servant HTTP test. The Hspec case separately invokes the existing public Ads key validator.
