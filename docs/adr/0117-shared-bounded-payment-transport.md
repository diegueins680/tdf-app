# ADR 0117 — Shared, bounded payment HTTP transport

Date: 2026-09-14 Ecuador / 2026-09-15 UTC. Status: proposed, implemented in a draft
branch; no deployment or provider activation. Depends on payment notification
minimization [#347](https://github.com/diegueins680/tdf-app/pull/347).

## Problem

The PlaceToPay/PayPhone adapter executor had redirect and header-timeout controls,
but `httpLbs` consumed the complete response before enforcing its one-MiB limit.
It also inherited a general-purpose connection manager's stale-connection retry
policy. Datafast/PayPal remote helpers bypassed that executor: default redirects,
unbounded response consumption and some parser errors containing provider input.
Service requests repeatedly created managers instead of reusing a payment pool.

Three uncalled PayPal remote helpers in `TDF.Server` duplicated the
active implementation, lacked stable mutation request IDs, and included a
floating-point amount formatter. Repository-wide call-site inspection found that
the active PayPal product paths already use `ServiceStorefront` remote helpers.
`TDF.Server` has implicit Haskell exports, but these helpers have no repository
callers or HTTP routes, and this package does not publish a Haskell library.

## Decision

```text
Bookings / tickets / courses / Domo / services / marketplace
  -> existing Datafast / PayPal remote helpers -----------+
Provider sessions / reconciliation                      |
  -> PlaceToPay / PayPhone adapter requests --------------+
                                                        v
                         ProviderAdapter.Http (shared boundary)
                         -> provider destination validation
                         -> payment TLS pool, no implicit retries
                         -> no redirects; 15-second total deadline
                         -> streamed, decompressed response <= 1 MiB
                         -> typed JSON or redacted uncertain error
```

- Preserve the existing JSON/form encodings, immutable merchant references,
  integer/decimal-string amounts and PayPal request IDs in the active helpers.
- Centralize safe URL parsing, validation after headers are attached, HTTP
  execution, typed JSON decoding and static error messages. Reject HTTP, URL
  credentials, explicit URL ports, fragments, cross-provider hosts and explicit
  `Host` header overrides. Malformed UTF-8 host bytes fail closed.
- PayPal, PlaceToPay and PayPhone retain exact configured provider host choices.
  Datafast retains the existing valid-DNS `oppwa.com` origin family and the
  existing sandbox/production environment check, rather than asserting an
  unverified universal regional endpoint. Its configured origin is normalized
  before use. Merchant-specific Datafast endpoints still require qualification.
- Use a separate shared TLS manager with normal certificate verification and
  `managerRetryableException = const False`. All active call sites changed here
  use it; the unrelated general-purpose manager is untouched. Injected managers
  are trusted application/test dependencies, not user configuration.
- Disable redirects and automatic cookie handling. Enforce both a 15-second
  response-header timeout and a 15-second total execution deadline, including
  body reads. Use exception-safe `withResponse` cleanup. Synchronous transport
  and decompression exceptions are redacted; external asynchronous cancellation
  is not deliberately swallowed.
- Consume successful responses incrementally. Stop on the first chunk exceeding
  the one-MiB accumulated limit. Non-2xx responses are errors without consuming
  their bodies. Decode inside the deadline and omit JSON diagnostics from errors.
- Validate PayPal access-token length/visible ASCII and Bearer token type before
  creating a financial request. Never print requests, tokens or response bodies.
- Remove only the three unused PayPal HTTP helpers. Keep the active helpers,
  pure legacy validators, existing public DTOs, endpoints and financial records.

## Compatibility and uncertainty

Transport errors do not prove a decline, cancellation, refund or lack of charge,
including HTTP 4xx/5xx, timeouts and invalid JSON. The existing canonical retry
isolation and reconciliation rules still apply. No provider fallback, retry loop,
new idempotency key, authorization, capture, refund or payout is introduced.

The legacy HTTP boundary consistently returns a static HTTP 502 error for
transport failures. A subset of PayPal verification/reconciliation/refund
transport failures previously returned 503. Both remain retryable *availability*
signals to callers, not permission to repeat financial writes. Client recovery
must continue inspecting the original attempt's authoritative state. Existing
disabled feature flags, underwriting and sandbox-evidence gates are unchanged.

No database migration, backfill or historical rewrite is needed. This is transport
consolidation, not a claim that Datafast/PayPal now implement every operation in
the newer `ProviderAdapter` record or that all requested business flows are done.
Old hosted widgets/redirects still collect payment information; this patch never
adds raw card-data collection or persistence.

## Validation and remaining limits

See [execution evidence](../payments/http-boundary-2026-09-14.md). Tests use an
in-memory HTTP connection under the real HTTP client plus synthetic JSON and
credentials. They do not exercise DNS, real TLS handshakes, merchant sandboxes,
staging payments, provider uptime or production latency.

Provider sandboxes must verify authentication, create/capture/status/refund and
webhook verification using the exact approved account, environment and feature
flags. A 15-second timeout may increase pending/reconciliation outcomes for slow
providers; do not lengthen it or enable retries without measured sandbox/staging
evidence and review. Review any provider response exceeding one MiB through a
redacted metadata-only incident report, not by logging its body.

Roll back by reverting this code only after stopping new payment writes through
an approved operational procedure. Preserve attempts, events and external IDs;
query original provider references before retrying. Reverting restores weaker
transport protections, so prefer a reviewed forward fix. No data rollback or
destructive cleanup is part of this change.

## Primary sources checked

Access date for all: **2026-09-15 UTC**.

| Source | Finding used | Confidence / qualification |
|---|---|---|
| [PayPal API requests](https://developer.paypal.com/api/rest/requests/) | Official sandbox/live API origins and OAuth/Bearer request conventions. | High for protocol documentation; not merchant availability. |
| [PayPal idempotency](https://developer.paypal.com/api/rest/reference/idempotency/) | Preserve operation-specific `PayPal-Request-Id`; support/retention depend on the endpoint, and a timeout can follow a completed operation. | High; no account-specific retry window assumed. |
| [Datafast official FAQ](https://www.datafast.com.ec/Soporte/PreguntasFrecuentes) | Hosted Dataweb integration, server status lookup and `test.oppwa.com` example. | High for published FAQ; regional production origin unverified. |
| [http-client maintainer API documentation](https://hackage-content.haskell.org/package/http-client-0.7.19/docs/Network-HTTP-Client.html) | `httpLbs` consumes the entire body; `withResponse`/`brRead` stream with cleanup; BodyReader bytes are decompressed; header timeout and manager retry settings are distinct controls. | High; compilation/tests must still use the repository's actual Stack dependency version. |

The Datafast-specific OPP documentation URLs
`https://datafast.docs.oppwa.com/` and
`https://datafast.docs.oppwa.com/tutorials/integration-guide` could not be opened by
the web tool on this access date. No claim about their current contents is made.
The FAQ and existing repository policy do not establish merchant credentials,
contract approval, production activation or a successful provider sandbox test.
