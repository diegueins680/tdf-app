# Credentialed browser origin boundary

Requirement: AUTH-CORS-001. This boundary concerns browser access to HTTP
responses and early middleware rejection. CORS is not identity authentication,
resource authorization, or complete CSRF protection.

## Intended behavior

A production runtime must never reflect arbitrary browser origins while allowing
credentials. `APP_ENV`, `ENVIRONMENT`, `NODE_ENV`, or `RUNTIME_ENV` with the
case-insensitive trimmed value `prod`, `production`, or `live` classifies the
runtime as production. Any production marker wins over development markers.
The same classifier protects the seed-trigger configuration.

Production startup rejects an effective allow-all flag or a configured `*`.
Origin aliases retain their existing first-nonempty precedence: `ALLOWED_ORIGINS`,
`ALLOW_ORIGINS`, `ALLOW_ORIGIN`, `CORS_ALLOW_ORIGINS`, `CORS_ALLOW_ORIGIN`.
Boolean flag validation and origin syntax validation remain enforced.

Production admits only explicitly configured origins and, when defaults are
enabled, the origin derived from configured `HQ_APP_URL`. Local development and
Pages preview origins are not implicit production grants. Explicitly configured
preview origins may be admitted. Native clients and probes without an Origin
header retain their existing behavior and still require endpoint authorization.
An untrusted Origin is rejected before the application handler executes.

## Evidence and limitations

On 2026-10-04, an unauthenticated `/health` request with
`Origin: https://conformance.invalid` received that origin in
`Access-Control-Allow-Origin` and `Access-Control-Allow-Credentials: true`.
The inspected backend was `645f56fcc44f81609fbfd0e03d683b40376ce77a`.
Production had `APP_ENV=production`, `ALLOW_ALL_ORIGINS=true`, HTTPS
`HQ_APP_URL`, and no explicit origin list or cookie overrides. Current Config
defaults in that configuration select Secure, SameSite=None cookies. This is
evidence of a credentialed origin-policy defect; no authenticated data access
or exploit was attempted.

`test/TDF/CorsSpec.hs` executes the real middleware. The full backend suite imports
it. `scripts/verify-cors-boundary.py` runs the same tests against current source
and two isolated mutations: disabled production classification and restored
implicit preview trust. A compiler failure or incomplete Hspec run cannot count
as a detected mutation. The receipt binds tests and implementation to source
hashes. These are finite runtime checks, not a universal proof, browser test,
or deployment verification. Endpoint CSRF behavior remains a separate obligation.

## Deployment and recovery

Deploy reviewed configuration and image together. An image-only rollout against
the observed `ALLOW_ALL_ORIGINS=true` must fail startup. The production Compose
services explicitly set `ALLOW_ALL_ORIGINS=false`, `CORS_DISABLE_DEFAULTS=true`,
and the highest-precedence `ALLOWED_ORIGINS` to the two canonical web origins.
This overrides stale aliases in `api.env` without printing its contents.

Production-backed previews are denied by default. Supporting a specific preview
requires a reviewed Compose override of `ALLOWED_ORIGINS` in the affected service,
listing all approved canonical and preview origins exactly; an `api.env` edit
does not override Compose's explicit setting. Review the override with the same
release and retain its nonsecret effective value in the deployment receipt.

Before admitting the candidate, test canonical origins, an untrusted origin,
preflight, no-Origin health, and exact version in the quarantined runtime. Repeat
safe unauthenticated origin checks after rollout. Rollback must preserve the
explicit allowlist even if recovering to an older image; re-enabling arbitrary
credentialed origins is not an acceptable recovery strategy. No migration or
provider activation is required for this change.


## Account-deletion route aliases

The authenticated deletion intake requires a non-simple proof header and a
trusted browser Origin even in a permissive development CORS environment.
Servant accepts `/feedback/account-deletion/` as the same endpoint; the guard
normalizes empty path segments before matching. Tests use Servant's actual route
matcher and verify both canonical and trailing-slash URLs. A controlled mutation
restores the old exact-path comparison and must fail the two trailing-slash
proof/origin checks. This establishes the middleware/routing boundary only;
identity, persistence, manual fulfilment and deployed behavior have separate tests
and requirements in `PRIV-INTAKE-001` and `PRIV-DELETE-001`.
Public checkout uses `X-Order-Lookup-Token` as a guest order capability. Explicitly
trusted origins must be able to send this existing header on GET/POST requests.
The CORS preflight must advertise that header without admitting an untrusted
origin or widening the origin allowlist. Endpoint token validation and order
binding remain mandatory; CORS header permission grants no order authority.
`TDF.CorsSpec` executes both methods through actual WAI middleware and denies
the same preflight from an untrusted origin.
