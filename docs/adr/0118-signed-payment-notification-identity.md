# ADR 0118: Signed PlaceToPay notification identity

Date: 2026-09-14 Ecuador / 2026-09-15 UTC. Status: implemented in a dependent
draft after #350; provider activation remains blocked pending real qualification.

## Context and official evidence

The previous PlaceToPay callback used `ptp-<SHA256(raw JSON)>`. Authentication
covered the provider's signed fields, but identity covered unsigned text too:

```text
One valid signed notification
  → whitespace / unsigned reference / message / signature-hex case variants
  → different raw-body IDs
  → repeated durable query triggers for the same evidence
```

This is storage and query amplification, not permission to charge, fulfill or
switch providers. The existing reconciliation layer still requires authenticated
provider queries and immutable transaction bindings.

Official sources accessed 2026-09-15 UTC; confidence high for the documented
session callback contract, unverified for any TDF merchant account:

- [Notification](https://docs.placetopay.dev/en/checkout/notification/): the
  session is identified by `requestId`; duplicate detection may compare
  signatures. The documented webhook is sent once, without provider retries,
  and expects a prompt 2xx response. Recurring notifications instead use
  `internalReference`; they are **not covered by this session implementation**.
- [SHA-256 migration](https://docs.placetopay.dev/en/checkout/migration-sha256/):
  the digest covers concatenated request ID, status, date and merchant secret.
  Its `sha256:` prefix and 64 hexadecimal digits are documented. Account-level
  SHA-256 activation requires provider coordination. Existing SHA-256-only
  validation remains; this patch does not introduce SHA-1 compatibility.

The plural `/en/checkout/notifications/` URL could not be opened. The singular
source above was reached through the official migration page's documentation
link. No provider callback or sandbox transaction was received during this work.

## Decision

After original-body authentication, derive:

```text
ptp-v2- + SHA256(UTF8(JSON array[
  "tdf:placetopay:notification:v2", integer requestId,
  exact status.status, exact status.date, lowercase signature
]))
```

The versioned array encoding is pinned by an independently computed test golden.
It is independent of object ordering, whitespace, equivalent JSON number/key
spelling, unsigned fields and signature hex casing. Exact signed status/date
strings remain unchanged. Distinct signed tuples or signing-key digests remain
distinct; this does not merge statuses, normalize instants or infer a financial
transition from a callback. The signature bytes alone are not used as the ID,
avoiding ambiguity from the provider's undelimited input concatenation.

`ProviderEventStore` enforces this identity for verified PlaceToPay inserts,
regardless of a caller-supplied event ID. It rejects the untrusted-store path for
this provider. Identity derivation itself does **not** authenticate anything:
the Servant handler still verifies the original message with the configured
secret before any inbox persistence. Other providers keep their existing IDs.

New retained payloads normalize only the signature's hexadecimal casing. The
existing unique `(provider, environment, merchant_account_ref, provider_event_id)`
constraint serializes concurrent inserts. Duplicate checks still require matching
immutable metadata, trust, resource, timestamp and an allowed payload checksum.
Replay never requeues a terminal row. No advisory locks or new schema are needed.

## Historical compatibility and explicit limit

Before inserting a canonical row, use the existing unique index to check for the
exact legacy `ptp-<SHA256(original bytes)>` in the same environment and merchant.
If present, reuse that ID and the original inbox UUID, subject to the unchanged
metadata checks. Checksums can match the original bytes, the current minimal
projection or the validated pre-v2 projection (which retained signature casing).
No historical row, ciphertext, provider reference or processing state is rewritten.

A differently formatted pre-upgrade notification cannot be mapped to an unknown
raw-body ID without historical payload processing. It may create **one additional
canonical query-trigger row per signed tuple**, after which variations converge.
Existing historical duplicates are not collapsed. This is a bounded forward fix,
not a retroactive deduplication/backfill claim. Authorized reconciliation remains
necessary; do not claim that historical evidence was purged or normalized.

The guarantee assumes all active callback writers use this version. An old
writer can still create raw-body variants during a mixed-version rollout.

## Threat controls and remaining risks

- Forged signatures fail before persistence. Tests explicitly distinguish an
  identity hash from signature verification.
- Valid unsigned-field/formatting replay cannot multiply **new** canonical rows;
  duplicates do not reset the worker's attempts or processed state.
- Environment/merchant scopes remain separate; immutable evidence conflicts fail
  closed, including conflicts with legacy rows.
- No callback status becomes payment evidence. Separate signed states are stored
  for authenticated reconciliation, including out-of-order arrival.
- Key rotation can produce a new signed tuple and therefore a new query trigger.
  Unknown future callback schemas fail closed. Mandates/recurring callbacks need
  a separately implemented, qualified contract.
- Rate limiting and historical amplification cleanup are not implemented here.
  Lost callbacks still require scheduled reconciliation because provider retry
  delivery is not documented. No timestamp-age rule is invented in this patch.

## Rollout and rollback

No SQL migration, generated contract or web/mobile change. Preserve existing
encrypted inbox data and references; the existing worker understands both old
and normalized minimal payloads. Review the dependent payment stack through
#350 before this increment. Keep accounts disabled until sandbox qualification.

For a future authorized rollout, inventory active writers and prevent mixed
legacy/new callback processing; reconcile outstanding sessions across the change
window. Do not rely on the provider redelivering notifications rejected during
maintenance. Compare counts by scoped resource and processing state, not by raw
payload; do not export ciphertext keys or decrypted notification bodies.

Rollback is code-only, with provider entry points disabled while the old writer
is restored. Preserve all evidence; do not rename/delete v2 rows. The older
worker can read the payload, but the older callback writer no longer supplies
the deduplication guarantee. Keep activation blocked until the forward fix and
reconciliation are restored. This document authorizes no deployment or transaction.

See [verification evidence](../payments/notification-identity-2026-09-14.md).
