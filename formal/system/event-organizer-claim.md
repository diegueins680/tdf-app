# Claiming an event without an organizer — EVT-ORGANIZER-CLAIM-001

Owner instruction of 2026-11-13: an event that has no administrator can be
claimed, in the same way an artist profile without an account can be claimed.

The artist claim (ID-ARTIST-CLAIM-001, ID-CLAIM-REVIEW-001) is a reviewed
request: a signed-in account submits evidence, the request grants nothing, and
an administrator other than the claimant approves or rejects it. This contract
applies the same shape to `social_event.organizer_party_id`. It is derived from
`tdf-hq/src/TDF/Server/EventOrganizerClaim.hs` and the migration
`tdf-hq/sql/2026-11-13_event_organizer_claim.sql`. It describes the shipped
behaviour with finite tests; it is not a proof.

It does not bring back the behaviour removed under EVT-OWNER-CLAIM-001
(AUTHORITY-057), where the first caller of a route became the organizer. No
route assigns an organizer to its own caller on request: the only new writer of
the organizer field is a strict administrator's approval of another account's
claim, and it uses the same single conditional assignment.

## Actors

- **Claimant**: any signed-in account that can see the event.
- **Reviewer**: a strict administrator (`hasStrictAdminAccess`) other than the
  claimant of the claim being decided.
- **Organizer**: the party stored in `social_event.organizer_party_id`; "no
  organizer" is judged by the rule every organizer check uses (absent, or only
  whitespace).

## Routes

- `GET /social-events/events/{eventId}/organizer-claim` returns whether the
  event can be claimed (it has no organizer) and the caller's most recent claim
  on it (identifier, status, submission and review times). It never returns
  another account's claim, evidence or review notes.
- `POST /social-events/events/{eventId}/organizer-claims` records a claim with
  evidence (trimmed, 20 to 2000 characters, no control characters other than
  line breaks and tabs).
- `GET /social-events/organizer-claims` (reviewer) lists up to 200 open claims,
  oldest first, then the 50 most recent decisions, with event title and start,
  whether the event now has an organizer, claimant party and display name,
  evidence, and the decision data.
- `POST /social-events/organizer-claims/{claimId}/decision` (reviewer) records
  `approved` or `rejected` with review notes (trimmed, 10 to 2000 characters).

Every route requires an authenticated session. The two event routes apply the
event visibility check (a hidden imported event answers 404 to everyone but a
strict administrator).

## Claim lifecycle

Statuses: `submitted`, `approved`, `rejected`, `superseded`.

1. **Submit.** Under the event row lock: the event must exist and have no
   organizer (409 otherwise). If the caller already has an open claim on the
   event, that claim is returned unchanged, with the evidence first recorded.
   Otherwise a `submitted` row is inserted, unless the caller already holds 5
   open claims or the event already holds 20 (409). Submitting changes nothing
   else: the organizer field, roles, modules and delegated administrators are
   untouched, and every organizer-only route keeps refusing the claimant.
2. **Reject.** Under the event row lock the claim becomes `rejected` with the
   reviewer, notes and time. The event stays without an organizer and the same
   account may submit a new claim.
3. **Approve.** Under the event row lock: the event must still have no
   organizer (409) and the claimant's party must still exist (409). The
   claimant is assigned through `assignMissingEventOrganizer`, the one
   conditional update of EVT-OWNER-CLAIM-001, whose reread decides the outcome.
   The claim becomes `approved`, and every other open claim on the event
   becomes `superseded` in the same transaction. A refused approval writes
   nothing.
4. **Replay.** Repeating the recorded decision on a decided claim returns it
   unchanged and writes nothing, whoever the reviewer is. The opposite
   decision, or any decision on a `superseded` claim, answers 409.

A claimant never decides their own claim, even as a strict administrator (403).
The table also refuses, by constraint, an `approved` or `rejected` row whose
reviewer is missing or equals the claimant, and a second open claim for the
same account and event.

## What an approval grants

The approved claimant is the stored organizer, with everything that implies:
editing the event and its image, ticket tiers, orders, refunds, manual payment
review, tax documents, waitlist, promo codes, budget, finance, logistics
ownership, and delegating administrators under EVT-ADMIN-DELEGATION-001. Buyer
and order data of the event become readable by that account. The reviewer is
shown this on the review screen.

## Web client

- The event page (`/social/eventos/{id}`) shows a claim card to a signed-in
  account when the event has no organizer: the evidence form, or the state of
  the account's own claim (under review; not approved, with the form again).
  It shows nothing once the event has an organizer.
- The directory administration page (`/admin/directorio`) has a fourth tab,
  "Eventos sin organizador", with the review queue: evidence rendered as text,
  required notes, approve and reject. Approval is disabled when the event
  already has an organizer.

## Evidence

Local run on 2026-11-13 (PostgreSQL, a migrated disposable database with this
migration applied): 7 examples, 0 failures; the full Hspec suite passed on the
same build (4418 examples, 0 failures, 25 pending). No hosted run has been
observed yet.

- `tdf-hq/test/TDF/EventOrganizerClaimSpec.hs`
  (`scripts/test-event-organizer-claim.sh`, PostgreSQL, fully migrated schema,
  real handlers): a claim is refused on an organized event, recorded once on an
  event without an organizer, repeated without a second row, and assigns
  nobody; an ordinary account cannot list or decide and a strict administrator
  cannot decide their own claim; approval assigns the claimant, records the
  reviewer and supersedes the other open claims; the decision replays without
  change and cannot be reversed; rejection leaves the event without an
  organizer and admits a new claim; approval is refused once the event has an
  organizer or when the claimant's party does not exist; six administrators
  approving six different claims of one event at once produce exactly one
  approval and five 409 refusals; the sixth pending claim of one account is
  refused.
- Jest: `EventOrganizerClaimCard.test.tsx`, `EventOrganizerClaimsQueue.test.tsx`,
  `SocialEventDetailPage.test.tsx`, `DirectoryAdminPage.test.tsx` and
  `socialEvents.test.ts` cover the client calls, the claim form and its states,
  the review queue, and that the card appears only for a signed-in account on
  an event without an organizer.

## Limits

- No model. The single assignment relies on the model and tests of
  EVT-OWNER-CLAIM-001; the claim state machine itself is covered by the
  PostgreSQL examples only.
- The limits (20 to 2000 characters of evidence, 5 open claims per account, 20
  per event, 200 and 50 rows in the queue) are implementation choices pending
  owner confirmation. The per-account limit is counted under the lock of the
  event being claimed, so simultaneous submissions for different events can
  exceed it by the number of requests in flight.
- An approval cannot be revoked through the API. Unlike a directory grant, the
  organizer field has no route that clears it; correcting a wrong approval is a
  reviewed database change, as before.
- A claimant cannot withdraw a claim or add evidence to an open one, and is not
  notified of the decision: the event page shows the state on the next visit.
  Review notes are not shown to the claimant.
- A strict administrator's edit of an event without an organizer still assigns
  that administrator (EVT-OWNER-CLAIM-001) and leaves the open claims
  `submitted`; they can then only be rejected. The queue marks them.
- `event_operation_relationship` is not updated: an event assigned by approval
  still shows `owner_missing` in the event-operations migration issues.
- The public event page and its unauthenticated data do not say whether an
  event has an organizer, so the claim is offered only on the signed-in event
  page. Mobile has no claim surface. The routes are absent from the OpenAPI
  document.
- Evidence is free text judged by a person. Nothing verifies it automatically,
  and the reviewer's judgement is the only control against a false claim.
- No production data was inspected: how many events lack an organizer is
  unknown (the read-only count is in `event-organizer-assignment.md`).
