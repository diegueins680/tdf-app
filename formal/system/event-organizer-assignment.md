# Events without an organizer — EVT-OWNER-CLAIM-001

`social_event.organizer_party_id` names the party that manages an event: its details,
image, ticket tiers, orders, refunds, invoices, budget and logistics. Every creation
path sets it. A row can still lack one (legacy rows, an organizer cleared by hand),
and the event-operations migration of 2026-09-14 records such events as
`owner_missing`, "must remain disabled until reviewed".

## Rule

- An event with an organizer is managed by that organizer. No request path replaces
  a stored organizer.
- An event without an organizer has no organizer-level manager. The routes that
  require the organizer (event edit, image upload, ticket tiers, order status,
  manual payments list, tax-document list, waitlist, promo codes, budget, finance)
  answer `403 This event has no organizer; a strict administrator must assign one by
  editing the event` and write nothing. Routes with their own organizer comparison
  refuse with their own messages.
- A strict administrator whose edit of the event (`PUT /social-events/events/{id}`)
  is accepted becomes its organizer in the same transaction, under the event row
  lock, through one `UPDATE … WHERE organizer_party_id IS NULL OR
  trim(organizer_party_id) = ''` followed by a reread; the edit proceeds only if the
  stored organizer is then the caller. A refused edit assigns nobody.
- Logistics access to such an event follows the recorded logistics team only
  (viewer or editor); nobody holds the owner role.
- Unchanged: strict administrators act on any event, with or without an organizer,
  through the routes that already admitted them without assignment — refund and
  manual-payment review, tax-document resend, bank-transfer refund completion,
  event deletion, invitations and the RSVP list.

## Defect repaired (AUTHORITY-057)

Until this change `claimOrRequireEventManager` made the caller the organizer of any
event without one. It was reached from seven call sites: event edit, tier creation
and update, order status change, the shared organizer check behind manual payments,
tax documents, waitlist, promo codes, budget and finance (reads included), the
refund-management check for non-administrators, and the logistics access check used
by every logistics read. The image upload had its own copy. Any signed-in account
that could see the event therefore took control of its ticket sales, payments review
and buyer data by opening a page. The write was an unconditional `UPDATE` after an
earlier read in another transaction, so two simultaneous callers were both told they
managed the event while the row kept only the later one.

The repository audit of 2026-09-14 listed this as high severity ("Backfill reviewed
owners, log claims, restrict compatibility window, then remove"). The backfill of
resolvable owners ran in the event-operations foundation migration; this change
removes the behavior.

## Evidence

- `formal/event-operations/EventOrganizer.tla`: callers read, decide, write and
  conclude in separate steps, which is weaker than the implementation (it now holds
  the row lock from the reread to the write). Checked for two allowed callers and
  one ordinary caller, from every initial organizer: 795 distinct states, three
  invariants (`OrganizerStable`, `OnlyAllowedAssigned`, `ManagersAreOrganizer`).
  Three controls each remove one guard and each violate one invariant: anyone may
  be assigned, the write is unconditional, the outcome trusts the earlier read.
- `tdf-hq/test/TDF/EventOrganizerSpec.hs` (`scripts/test-event-organizer.sh`,
  PostgreSQL, fully migrated schema): the organizer check refuses and leaves the row
  untouched; a blank organizer counts as missing; eight simultaneous assignments
  admit exactly one; a late caller waits behind an uncommitted assignment and is
  then refused; an assigned organizer is kept.
- `tdf-hq/test/TDF/Social/FollowHandlerSpec.hs` (SQLite, real handlers): the edit
  and image-upload routes refuse an ordinary user on an event without an organizer
  and change nothing; a strict administrator passes the gate, and an edit that is
  then rejected leaves the event without an organizer.

## Limits

- No test drives a successful administrator edit through the handler, and the other
  call sites (tiers, order status, the shared organizer check, logistics) have no
  per-route test for this case; they were verified by reading.
- No shipped client offers the administrator edit: the web app shows the edit tools
  only to the organizer and Mobile has no event edit. Today the recovery is an API
  call or a reviewed database assignment. An event without an organizer therefore
  loses organizer-level management on deploy until one of the two is done.
- The administrator who assigns the event becomes and stays its organizer; there is
  no route to hand it to another party.
- The model is bounded and covers assignment only. It does not cover the routes
  that compare the organizer themselves, administrator role changes during a
  request, or direct database edits.
- The assignment is not written to an audit table; `updated_at` is the only trace.
- `event_operation_relationship` is not updated by the assignment, so an event
  assigned this way still shows `owner_missing` in the event-operations migration
  issues until reviewed there.
- No production data was inspected: how many events lack an organizer is unknown.
  Current code never creates one. Before deploy, count them with
  `SELECT count(*) FROM social_event WHERE NULLIF(btrim(organizer_party_id), '') IS NULL`.
