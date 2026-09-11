# RSVP, sharing, and profile activity

## Domain contract

An RSVP is the authenticated person's current answer for one event. Its only persisted
states are `accepted` (Voy / Going), `maybe` (Me interesa / Interested), and `declined`
(No iré / Not going). No row means that the person has no RSVP; `NONE` is a client-only
display value and is never sent to the API.

Allowed transitions are absence to any persisted state, any persisted state to any other
persisted state, and any persisted state to absence by deletion. Repeating the same upsert
or delete is idempotent. The database serializes concurrent writes through the unique
`(event_id, party_id)` key and an atomic upsert.

The authenticated session is the sole RSVP identity authority. The event comes from the
validated path capture. Canonical requests contain only `status` and `showOnProfile`.
Legacy `rsvpPartyId`, `rsvpEventId`, and other server-managed fields are rejected as
unknown input. Compatibility is provided at the route level, never by accepting caller
identity.

## Eligibility and privacy

- New or updated RSVPs require an existing, visible event whose active workflow state has
  the `rsvp` capability. Cancellation, deletion, loss of visibility, or an ineligible
  workflow state rejects an upsert. Deletion of the caller's RSVP remains idempotently
  available and does not reveal whether a guessed private event exists.
- Public sharing requires an event marked public whose active workflow state is
  public-listable. A cancelled event may retain a public detail only when the event remains
  explicitly public; it never accepts a new RSVP. Private, hidden, malformed, or deleted
  events fail closed with the same not-found boundary.
- Anonymous reads expose event-safe fields and aggregate `accepted`/`maybe` counts only.
  Attendee identity is not part of the public contract. Organizer RSVP lists require
  organizer ownership or strict administrative permission.
- Each RSVP stores an explicit `show_on_profile` decision. Historical rows are migrated to
  `false`. New choices default in the UI from the account preference, are visibly editable,
  and are returned authoritatively by the server.
- Feed projection is derived from the current RSVP and current event. Only `accepted` and
  `maybe` with explicit profile visibility may appear. `declined`, deletion, private or
  hidden events, deleted events, and blocked viewers remove the item. A cancelled event is
  shown only if its public detail remains eligible. Past events say that the person *marked
  they would attend*; actual attendance is never claimed without a verified check-in.
- The public-directory surface requests activity through an authenticated slug endpoint.
  The server resolves only a published public `person` profile with an explicit subject
  Party and then applies the same block/visibility rules; organization, unlinked, ambiguous,
  and blocked profiles fail closed without putting a Party identifier in the browser URL.

## Authentication continuation

An anonymous RSVP action stores an expiring, single-use local intent containing only the
event identifier, selected state, profile-display choice, allowed public return path,
origin, an allowlisted shared-campaign boolean, nonce, and timestamps. Web uses session
storage; mobile uses application storage.
The URL carries only the existing `events` onboarding intent and a sanitized local return
path. Opening a shared link alone never creates an account or an RSVP.

Signup is shown first with an explicit existing-account alternative. Successful signup or
login returns to the public/native event. That screen reloads the event, revalidates RSVP
eligibility, performs the atomic upsert, and clears the intent only after server success or
explicit dismissal. Failure, cancellation, offline state, or a changed/deleted event keeps
the unexpired intent and explains the recovery action. Expired or malformed intents are
discarded. A consumed intent cannot be replayed; repeated delivery remains safe because the
upsert is idempotent.

## Sharing and observability

All shares use the canonical public `/eventos/:eventId` URL. Only allow-listed attribution
parameters may be appended; no party identifier, PII, session material, or protected route
is permitted. The native browser/device share sheet is preferred, copying is always
available, and WhatsApp is an explicit user action. Share-sheet destination is recorded
only when the platform reports it.

Web and mobile use the same event analytics vocabulary without PII: shared-event view,
RSVP start, auth redirect/completion, RSVP create/update/delete, share prompt, share start,
share completion/cancellation when knowable, link copy, and attributed conversion. Backend
logs record outcome, event, operation, and conflict/error class without names, email,
tokens, or raw party identifiers.

## Concurrency and failure behavior

Controls disable duplicate submissions and optimistic UI is allowed only with a stored
rollback snapshot. The server response always replaces optimistic state. Two concurrent
upserts produce one row; a delete racing an upsert has normal last-committed-writer
semantics, and the next read is authoritative. Rate limits constrain abusive mutation
bursts without changing idempotency. Cache keys for RSVP, RSVP summary, profile activity,
and invitations are distinct and user-scoped where identity affects the response.
