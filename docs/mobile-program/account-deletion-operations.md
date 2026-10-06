# Account deletion: authenticated initiation and manual fulfilment

The authenticated form and administrator queue use separate rollout controls, both disabled by default until their backend/web qualification. Pausing new intake leaves an enabled operator queue available for existing requests. The public page retains the verified email contact while disabled. See [account-deletion-rollout.md](account-deletion-rollout.md) for the build switch, acceptance and recovery order.


The owner confirmed that `info@tdfrecords.net` handles account/data-deletion
requests. `/cuenta/eliminar` gives the account holder a dedicated, bilingual
initiation flow without composing an email or explaining a reason. The mobile
About → Support and legal → Data Deletion link reaches this action through the
existing legal page. Both canonical and old legal URLs remain supported.

The form shows the current account, requires an explicit confirmation, reloads
the live cookie session before submission and refuses a missing/different
account. It sends the request to `/feedback/account-deletion?accountId=…` using that
cookie, without a potentially stale bearer-token override. The non-simple
`X-Requested-With: TDF-Account-Deletion` header is required and any browser Origin
is checked against configured TDF origins even when general CORS is permissive.
A cross-site multipart form with the session cookie is rejected. The guard
normalizes empty path segments so the trailing-slash URL accepted by Servant
requires the same origin/header proof as the canonical endpoint. The POST itself
normalizes browser multipart CRLF to a canonical newline before validating the
request marker, requires a live matching account before insertion or notification, and reuses
`withCurrentAuthSession` to hold the existing token-row lock through insertion.
A revocation that wins the lock prevents acceptance. The client
validates the returned `adrCreatedBy` and `adrRequestId` before acknowledging
receipt; an expired session or an old backend cannot produce a false success. If preflight or submission reports lost authentication, the form is removed and offers sign-in with the deletion destination preserved; network errors remain retryable. No credentials,
attachments, diagnostic logs or analytics events are added. A successful
response means **request received**, never **account deleted**. Processing is
manual with the already stated target of 30 days and a completion confirmation.

## Operator authorization

The backend requires an Admin, Manager or StudioManager role **and** the canonical `internships` module grant for both legacy-feedback reads and deletion resolution, matching the UI boundary. A role alone, or an unrelated admin-module grant, cannot read the privacy queue or mark a request terminal. Request authentication alone is insufficient. Within the same database transaction as the private read or terminal audit, `withCurrentAuthorization` locks and rechecks the current session, active role assignments/roles and module permission chain. It evaluates both the operator-role and internships-module predicate on those current grants. The lock order is session, role assignments/roles, module permission chain, then (for resolution) owner advisory mutex and feedback row. A role or module revocation committed before that admission denies the operation. A revocation that loses to the held grant locks waits until the admitted transaction completes. An Intern grant alone remains insufficient after loss of the operator role. Queue projection and its history reads share that admitted transaction.

## Operator procedure

1. Find the `account_deletion_request` in the existing internal feedback queue
   (the normal feedback notification also reaches the confirmed inbox).
2. Verify the authoritative database/queue `feedbackCreatedBy` is present and
   matches `requested_account_party_id`. The administrator view shows the
   server-recorded account and request IDs beside these requests; the protected
   `GET /feedback/internal/legacy?accountDeletionOnly=true&offset=0` response exposes them as `lfdCreatedBy` and
   `lfdId`. The dedicated administrator queue filters requests before pagination
   and exposes pages of 20 with next/previous controls; ordinary feedback does
   not evict privacy requests. The general feedback endpoint rejects the reserved
account-deletion marker, and resolution checks the single claimed owner against
the stored authenticated creator, including old records. It still accepts
   anonymous feedback: a body, email, title or claimed ID alone is **not**
   authority to delete an account. Reject mismatched requests for fulfilment;
   ask the account holder to use the authenticated flow again if needed.
3. Open the case in the existing [privacy operations ledger](../../ops/privacy/README.md)
   using the original `feedbackCreatedAt` as receipt time. Keep the mapping from
   the opaque ledger case to the feedback ID/account and evidence in the private
   proof store, not in analytics or the ledger's public metadata. Preserve the
   earliest receipt and 30-day deadline; use the ledger's identity, full-scope,
   effect-verification and delivered-notice stages before closing fulfilment.
   The web queue is authenticated intake and a resolution receipt, not a second
   erasure engine or a substitute for this evidence lifecycle.
4. Verify the account contact against TDF's account records before sending any
   personal information. Handle legal/fiscal/security/dispute retention
   separately and explain the actual retained records to the account holder.
5. Process the entire account and associated personal data, including
   user-generated content. Revocation/deactivation alone is not completion.
   Revoke sessions and linked service access as part of the existing owner-run
   deletion process. Do not delete shared financial records indiscriminately.
6. Confirm completion through the verified account contact, and record the
   processing result and verified-contact confirmation in the internal queue. The
   completed/rejected actions require a note and append the operator identity and
   timestamp to the existing audit table. Requests begin pending; concurrent or
   repeated resolution returns 409 without overwriting the first result. These
   actions record work already performed and never erase data. The UI retains the
   authoritative terminal receipt even if reloading the queue fails. Do not expose the request, identity
   or result to other users or product analytics.

This change does not automate erasure or prove a production account has been
deleted. No real account-deletion request is submitted for QA. Tests use
synthetic identities, intercepted browser requests and a disposable PostgreSQL/API. Actual fulfilment remains the
owner's confirmed operational responsibility.

Apple allows manual processing with a clear timeframe and completion notice,
but requires initiation without a mandatory support email for this kind of app.
See [Apple's account-deletion guidance](https://developer.apple.com/help/app-review/guideline-reference/5-1-1-account-deletion).
The separate App Store 2.1/4.8 rejection and physical-iPhone authentication gate
remain open; this implementation is not a claim of App Review approval.

Deploy the backend receipt route and paginated queue before considering the web
form operational. A missing route fails closed; no request is acknowledged by
an old API. No database migration is required.

Verification covers arbitrary positive/mismatched/missing identity inputs with
QuickCheck, rejected/mismatched/missing receipts in the client, a cookie revoked
between preflight and POST in the browser fixture, and a disposable API harness
with 21 privacy requests followed by ordinary feedback. The harness checks
unauthenticated/foreign-owner rejection, authoritative request IDs and both
queue pages. These tests do not establish manual fulfilment or physical-device
behaviour.

## Intake retries and notification audience

The authenticated owner has one unresolved receipt. Intake holds a transaction-scoped,
owner-specific PostgreSQL advisory lock across lookup and insertion, including requests
from different live sessions. A retry returns the oldest pending receipt whose owner claim passes the same validator
as strict intake and resolution. Missing, duplicated or mismatched legacy owner claims
are ignored for reuse and preserved for operator reconciliation. The lookup pages through
100 candidates at a time under the owner mutex, so invalid rows cannot hide a later valid
receipt or cause unbounded row materialization. A retry does not change
its original timestamp/content or send another notification. Existing duplicate legacy
records are retained for operator reconciliation, not deleted. After terminal resolution,
a new request may create a new receipt; there is no permanent client-key idempotency
promise across completed cases. Resolution takes the same owner mutex before locking
the feedback row. An intake ordered after resolution waits for its commit and creates a
fresh pending receipt; intake ordered first can legitimately reuse the existing receipt.
All writers acquire locks in session → owner → feedback order; the stored creator is
immutable after insertion. Anonymous legacy records retain row-only resolution.

New deletion notices go only to the owner-confirmed `info@tdfrecords.net` inbox. Ordinary
feedback retains its separate audience. The server sends a notice only for a newly inserted
receipt; transport delivery is still best effort and requires operator queue monitoring.
No test or abstract model proves actual inbox delivery or fulfilment.

`AccountDeletionIntake.tla` checks three bounded concurrent attempts for one already
authenticated owner, with an abstract owner mutex and no terminal resolution or crashes.
It checks one pending receipt, notices only for new receipts, and the privacy audience.
Three negative configurations remove the mutex, notify replays, or use the general audience;
each must violate its named invariant. These finite abstractions are not a proof of SQL,
SMTP, session validation, crash recovery or whole-system refinement. The actual disposable
HTTP harness separately races eight cookie/bearer submissions, checks receipt/time reuse,
then verifies new requests after terminal resolution, pagination and concurrent resolution.

`AccountDeletionResolution.tla` separately models one intake racing one resolution
of an existing request. Resolution that acquires the owner mutex first must yield a
fresh receipt to subsequent intake; removing that mutex produces the expected
counterexample. This finite safety model assumes committed transactions and does
not establish fairness, crash recovery or SQL refinement. The actual HTTP regression
holds a feedback row in a separate PostgreSQL transaction, observes resolution
waiting on that row and intake waiting on the owner advisory lock, then releases
the barrier and checks one fresh pending receipt plus one immutable resolution.
The previous binary must fail that same test. No timing-only sleep determines
which request wins.

The intake model includes an abstract invalid legacy receipt; an additional negative
configuration disables owner validation and must violate `OnlyValidReceipt`. Actual
HTTP/PostgreSQL checks seed 105 older invalid rows (missing, duplicated and foreign
claims), accept a fresh valid request, and race eight retries that must reuse that
valid receipt beyond the first page while preserving every legacy row. This does
not automatically reconcile or erase historical records.

Legacy marker lookups accept LF, CRLF and bare CR consistently in the operator
queue, receipt reuse and terminal resolution. The SQL prefix admits both newline
forms before the normalized owner validator runs; reading or resolving a record
does not rewrite its original content or timestamp. Actual HTTP checks convert
synthetic existing records to each legacy encoding, verify visibility, race eight
retries against the same receipt and record exactly one terminal outcome.

## Concurrent operator authority model

`AccountDeletionAuthority.tla` bounds one request and session/operator/module
revocation, with three Boolean grants and request phases new/captured/admitted/done.
The operator Boolean abstracts membership in Admin/Manager/StudioManager; module
access without that membership is insufficient. Admission atomically abstracts
successful current-row validation and all retained locks. Actual PostgreSQL
acquires those locks in the order above; no effect occurs until all checks pass.
The model ends at the effect, so a later legitimate revocation does not retroactively
invalidate a committed operation. There is no fairness or liveness claim.
Three controlled mutations admit captured stale grants, omit the session lock,
or omit grant locks; each must violate `CurrentAuthorityAtEffect`.
The model excludes crashes, SQL query planning, new grants, multiple requests,
external deletion effects and response transport. It is bounded safety analysis,
not a Python/Haskell/SQL refinement proof. Six real HTTP/PostgreSQL barriers cover
operator-role and module revocation during a witnessed token-row wait for queue
reads, legacy reads and resolution, including retained Intern access. Rejected
resolution must leave no terminal audit. Previous binaries are failing controls
only when actually run; source inspection is not runtime evidence.
