# Public route admission: AUTH-PUBLIC-001

Every operation in [compiled-api-surface.json](compiled-api-surface.json) that is
served without a Servant `AuthProtect` combinator, and every opaque Raw mount, must
appear exactly once in [public-routes.json](public-routes.json) with a reviewed
boundary. `scripts/check-public-routes.py` enforces this in CI; the snapshot itself
is proven to match the built executable by the compiled-API gate (SYS-API-003).

## Boundaries

| Boundary | Admits | Structural check |
|---|---|---|
| `public-read` | Content intended for public display | Not allowed on POST/PUT/PATCH/DELETE |
| `public-diagnostic` | Version, health, OpenAPI, docs, health-only `/mcp` | Mutation only for `/mcp` |
| `capability-header` | Follow-up access with a server-issued lookup secret | Named header is a route input |
| `capability-path` | Random UUIDv4 bearer identifier in the path | Reviewed; sequential IDs inadmissible |
| `signed-provider-callback` | Provider webhooks and notifications | Mutations receive the exact raw body; declared signature header present |
| `provider-verification-challenge` | Meta subscription challenge | `hub.verify_token` query present |
| `handler-session` | Handler resolves `Authorization`/`Cookie` itself | One of those headers is a route input |
| `credential-intake` | Login, signup, password reset | Reviewed |
| `public-intake` | Anonymous creation of the submitter's own new record | Reviewed |
| `operator-secret` | Seed operations | Named secret header is a route input |
| `public-static-files` | Raw static file mounts from the public media root | Only Raw mounts may use it, and Raw mounts may use nothing else |
| `public-withdrawal` | Anonymous withdrawal of a permission (WhatsApp opt-out) | Reviewed: may only reduce permissions, never disclose prior state |
| `known-debt` | Reviewed and found insufficient | Must reference a registered debt |

The gate rejects unregistered routes, stale or duplicate entries, unknown
boundaries and structural inconsistencies. Fourteen mutation controls in
`scripts/test-public-routes.py` exercise each rejection. Passing establishes that
the declaration is reviewed and structurally consistent. It does **not** prove that
a handler verifies its capability; handler enforcement needs its own HTTP evidence.

## Repairs from the 2026-10-06 audit

The audit of main `1f99d143900a67aa9b82f4138630684afbd7db2f` found 89 mutating and
90 read routes without `AuthProtect`. Four groups were repaired:

- `POST /public/courses/{slug}/registrations/{registrationId}/payment-intent` and
  `/checkout-session` were retired. They served only legacy registrations, which
  have no lookup token, keyed by a sequential ID. For a party-linked registration
  the mobile branch returned a Stripe ephemeral key for that party's Stripe
  customer, and any caller could consume the registration's single PaymentIntent
  slot. No web or Mobile client called them and OpenAPI did not document them.
  Canonical course checkout (ADR-0111) uses lookup-token Datafast/PayPal routes.
- `POST /ads/assist` now requires authentication and `hasSocialInboxAccess`
  (product-owner decision, AUTHORITY-049). Retrieval already rendered only current
  public course data (PRIV-RAG-001), but anonymous callers received the six latest
  staff-authored ad conversation examples, scopable by ad or campaign ID, and could
  invoke the paid model without metering. The staff Social Inbox page is its only
  caller; a future public assistant needs its own reviewed endpoint.
- `GET /input-list/sessions` and `/input-list/sessions/pdf` were retired. They
  enumerated studio sessions by index and disclosed client names. Scheduling staff
  keep `GET /sessions/{id}/input-list` and `/sessions/{id}/input-list.pdf`.
- `GET /input-list/inventory` now rejects `sessionId` and `channel`. Those filters
  derived availability from a session's private input rows; no client used them.

`tdf-hq/test/TDF/ServerSpec.hs` ("served authorization boundary") exercises the
served application: anonymous assistance returns 401, a signed-in user without
Social Inbox access receives 403, and the retired course routes return 404.
`scripts/test-booking-conformance.py` repeats the 401/403 checks against a real
PostgreSQL-backed server and exercises retrieval privacy as authenticated staff.

## Open debt

No registered debt remains.

`DEBT-AUTH-ACADEMY-001` was repaired on 2026-10-07 (AUTHORITY-051): academy enroll,
progress and referral claim require a session and bind to the signed-in account's
email; a different email is refused with 403.

`DEBT-PRIV-WHATSAPP-001` was repaired on 2026-10-07 (AUTHORITY-052, PRIV-WHATSAPP-001):
public consent is a double opt-in request activated only by a timely SI reply from
the number, the public lookup is retired (410), and public opt-out is a non-disclosing
withdrawal. Existing consent rows are unchanged by the additive migration.
