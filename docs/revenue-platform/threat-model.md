# Revenue and distribution threat model

Scope: public storefronts, provider callbacks, guest tracking, private assets, DDEX delivery,
royalties, statements, and settlements. This is an engineering threat model, not legal advice.

## Trust boundaries

Browsers/mobile clients and uploaded files are untrusted. Provider browser returns are untrusted.
Only authenticated provider server responses or signature-verified events can establish payment.
Partner delivery status requires recipient evidence tied to the exact environment/profile/package.
Staff actions require least privilege and audit; a staff login does not make financial evidence true.

| Threat | Control implemented in this branch | Residual action before production |
|---|---|---|
| Price, quantity, tax, or fee tampering | Server package lookup, integer minor-unit snapshots, song bounds, immutable checkout lines | Wire all domains to approved product/quote versions and tax adapter |
| Fake browser callback | Return cannot mark paid; Datafast/PayPal bindings verify provider data; PayPal webhook uses remote verification over exact raw bytes | Verify the PayPal contract with secret-backed sandbox tests; keep Datafast callback disabled until contracted |
| Callback replay/reordering | Immutable encrypted PayPal inbox, event/hash dedupe, four-day/five-minute timestamp bounds, claim/retry/dead-letter state | Add operator replay endpoint/worker scheduling, alerting and credentialed delayed-event tests |
| Duplicate order/capture/refund | Client idempotency, unique provider bindings, PayPal request IDs, checkout-locked refund reservation and immutable allocation | Apply orchestration to every domain and add provider concurrency E2E |
| Guessing guest orders | Random lookup token stored as hash, constant-shape not-found response | Add verified-email recovery, rate limiter and abuse telemetry at ingress |
| Raw card/secret leakage | Hosted provider model; secret values not documented; redaction boundary specified | Structured redaction tests, secret-manager rotation and log sampling in staging |
| Privileged token in browser | Records demo/admin bearer removed | Bundle scan as a blocking CI check across all build variants |
| Asset theft | Private staging paths and checksum-verified download | Object storage, short signed URLs, per-object ACL, access audit, encryption/backup drills |
| Malicious/oversized XML | 50 MiB gate, strict decode, DOCTYPE/entity/XInclude rejection and parser tests | Malware scanner, decompression bomb controls and licensed XSD/profile validation |
| Rights/identity fraud | Versioned declarations, evidence references, accepted 100% splits and staff-review state | KYC/sanctions/tax/legal workflows, policy review, complaint/counter-notice and guardian gates |
| False DSP status | Distinct states; environment-bound evidence; production rejects mock/sandbox evidence | Contracted signed/verified acknowledgement adapter and live-store evidence rules |
| Delivery substitution | Immutable release/package/profile/message/checksum linkage | Private package persistence, transport receipt verification and key rotation |
| Royalty report tampering | Raw checksum/version/correction lineage; normalized lines and allocations immutable | Signed partner receipt where offered, total reconciliation and dual-control report acceptance |
| Rounding/split theft | Basis points must total exactly 100%; immutable correcting events | Currency/FX/rounding property tests on real partner fixtures |
| Payout-account takeover | Versioned beneficiary profile, verification status, dual-control payout approval | Step-up auth, cooling period, out-of-band change notice and bank ownership verification |
| Insider refund/price/release abuse | Strict-admin refund endpoints, immutable economics and allocations, requester/reviewer separation, compensating ledger and credit note | Step-up auth, configurable limits, anomaly alerts and periodic access review |
| Enumeration/rate abuse | Token capability and customer-safe response | Edge/IP/account/device throttles, CAPTCHA escalation and alert thresholds |
| Provider/partner outage | Honest processing/unavailable states and kill flags | Circuit breaker, bounded worker retries, DLQ dashboard and rehearsed failover messaging |

## Security invariants

1. No client payload can set `paid`, `captured`, `delivered`, `accepted`, `live`, or `settled`.
2. Environment, merchant, currency, amount, internal order and exact provider resource must match.
3. Provider/recipient event identity is unique and processing is replay-safe.
4. Fulfillment is separate from payment and consumes a hold exactly once.
5. Refunds, disputes, corrections and write-offs add compensating events; history is not rewritten.
6. A production status cannot cite mock/sandbox evidence.
7. Protected assets are private, immutable once delivered, checksum-bound and access-audited.
8. Prices and rights/splits become immutable when approved/accepted.
9. Requesting and approving a sensitive financial action must be different principals.
10. Automatic payout remains impossible while its production flag, verification, reconciliation, and
    explicit authorization gates are incomplete.

## Abuse-test gates

Before production, add an automated bundle/secret scan, credentialed webhook signature/retry suite,
concurrent HTTP capture/refund/hold tests, signed-URL authorization tests, malware/quarantine fixtures,
role-matrix tests, beneficiary-change/cooling-period tests, and log-redaction assertions. Perform a
staging tabletop exercise for provider compromise, account takeover, false live evidence, emergency
takedown, reconciliation variance, and insider approval abuse.
