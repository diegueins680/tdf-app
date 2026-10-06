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
| `known-debt` | Reviewed and found insufficient | Must reference a registered debt |

The gate rejects unregistered routes, stale or duplicate entries, unknown
boundaries and structural inconsistencies. Twelve mutation controls in
`scripts/test-public-routes.py` exercise each rejection. Passing establishes that
the declaration is reviewed and structurally consistent. It does **not** prove that
a handler verifies its capability; handler enforcement needs its own HTTP evidence.

## Repairs from the 2026-10-06 audit

The audit of main `1f99d143900a67aa9b82f4138630684afbd7db2f` found 89 mutating and
90 read routes without `AuthProtect`. Three groups were repaired:

- `POST /public/courses/{slug}/registrations/{registrationId}/payment-intent` and
  `/checkout-session` were retired. They served only legacy registrations, which
  have no lookup token, keyed by a sequential ID. For a party-linked registration
  the mobile branch returned a Stripe ephemeral key for that party's Stripe
  customer, and any caller could consume the registration's single PaymentIntent
  slot. No web or Mobile client called them and OpenAPI did not document them.
  Canonical course checkout (ADR-0111) uses lookup-token Datafast/PayPal routes.
- `POST /ads/assist` now requires authentication and `hasSocialInboxAccess`. It
  returned retrieved RAG context containing campaign budgets, ad notes, internal
  studio-knowledge entries and booking/teacher schedules, and invoked a paid model
  without rate limiting. The staff Social Inbox page is its only caller.
- `GET /input-list/sessions` and `/input-list/sessions/pdf` were retired. They
  enumerated studio sessions by index and disclosed client names. Scheduling staff
  keep `GET /sessions/{id}/input-list` and `/sessions/{id}/input-list.pdf`.

`tdf-hq/test/TDF/ServerSpec.hs` ("served authorization boundary") exercises the
served application: anonymous assistance returns 401, a signed-in user without
Social Inbox access receives 403, and the retired course routes return 404.

## Open debt

- `DEBT-AUTH-ACADEMY-001` (P2): academy enroll/progress and referral claim trust a
  submitted email.
- `DEBT-PRIV-WHATSAPP-001` (P2): public WhatsApp consent lacks proof of number
  control, messages the submitted number, and discloses consent state.

Both need a product decision on identity proof (for example a one-time code) before
repair; until then they stay visible as registered debt.
