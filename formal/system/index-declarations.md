# A lookup index never refuses a legitimate row

Requirement `DATA-INDEX-DECLARATION-001`, decision `AUTHORITY-063`.

## What was wrong

The models declared twenty-three lookup indexes with lines such as

    IndexChatMessageThread threadId
    IndexWhatsAppMessagePhone phoneE164 createdAt !force

The model language has no index declaration. It reads any capitalised line of
this shape as a uniqueness constraint, whatever its name. Production does not
take its schema from the models (`RUN_MIGRATIONS=false`), but nine of these were
created there as `UNIQUE` constraints, and are in the production schema snapshot
of 2026-08-14 that every migration since builds on:

| Table | Constraint | Columns | What it forbids |
| --- | --- | --- | --- |
| `chat_message` | `index_chat_message_thread` | `thread_id` | a second message in a conversation |
| `whats_app_message` | `index_whats_app_message_phone` | `phone_e164, created_at` | two rows for one number at one instant |
| `whats_app_message` | `index_whats_app_message_party` | `party_id, created_at` | two rows for one contact at one instant |
| `course_registration` | `index_course_registration_party` | `party_id, created_at` | two registrations by one person at one instant |
| `course_registration_receipt` | `index_course_registration_receipt_registration` | `registration_id, created_at` | two receipts for a registration at one instant |
| `course_registration_receipt` | `index_course_registration_receipt_party` | `party_id, created_at` | two receipts for a person at one instant |
| `course_registration_follow_up` | `index_course_registration_follow_up_registration` | `registration_id, created_at` | two follow-ups for a registration at one instant |
| `course_registration_follow_up` | `index_course_registration_follow_up_party` | `party_id, created_at` | two follow-ups for a person at one instant |
| `course_registration_follow_up` | `index_course_registration_follow_up_next` | `next_follow_up_at` | two follow-ups, of anyone, with the same reminder time |

The application writes rows these forbid:

- `chatSendMessage` inserts a `chat_message` for every message. The second one
  in a conversation fails with a unique violation and the request returns 500.
- The WhatsApp webhook reads the clock once per delivery. It stores the inbound
  message with that time and, for an enrolment request, records its reply with
  the same time and telephone number. The reply has already been sent when the
  record fails; the webhook answers 500 and the provider delivers the message
  again. Two messages from one sender in one delivery fail the same way.
- Staff who set the same reminder time on two follow-ups have the second refused.

None of the nine is used to look a row up, and no statement names them in an
`ON CONFLICT` clause.

The other fourteen lines have no constraint in the snapshot. They made a
difference only to a database built from the models (local development).

## What changed

- `tdf-hq/sql/2026-10-11_index_declarations_not_unique.sql` drops each of the
  nine constraints and creates a plain index with the same name and columns. It
  changes no row and can be applied more than once.
- `tdf-hq/sql/2026-10-11_index_declarations_not_unique_rollback.sql` restores
  the constraints. It fails, changing nothing, once any table holds rows they
  forbid.
- The twenty-three lines are removed from `TDF.Models` and `TDF.ModelsExtra`, so
  a database built from the models no longer creates the constraints. No code
  referred to them.

The identity constraints are untouched: a WhatsApp provider message id, a
conversation's pair of participants, and a registration's payment identifiers
remain unique.

## What changes when this is applied in production

- Conversations accept a second and later message. Until now each conversation
  could hold one.
- Requests that failed with a unique violation on one of the nine names succeed.
- Each dropped constraint and each index build takes a brief exclusive lock on
  its table. The tables are small.
- Nothing is switched on, and no stored row is modified.

Whether production still carries the nine constraints was not checked against
the live database. This query answers it:

    SELECT conrelid::regclass, conname FROM pg_constraint
    WHERE contype = 'u' AND conname LIKE 'index\_%';

## Model

`formal/event-operations/IndexDeclaration.tla` describes a table of rows written
under a key at a time, each with its own identity, and three switches for the
constraints. Bounds: two keys, two times, three identities.

| Configuration | Constraint switched | Result |
| --- | --- | --- |
| `IndexDeclaration.cfg` | identity only (the repaired schema) | 493 distinct states, no violation |
| `IndexDeclarationKeyUnique.cfg` | adds unique key (`chat_message`) | `NothingNewRefused` violated |
| `IndexDeclarationKeyTimeUnique.cfg` | adds unique key and time (the other eight) | `NothingNewRefused` violated |
| `IndexDeclarationIdentityNotUnique.cfg` | removes unique identity | `IdentityStoredOnce` violated |

This is bounded model checking of an abstraction. It does not model the
application's code, time precision, or concurrent transactions.

## Tests

`tdf-hq/test/TDF/IndexDeclarationSpec.hs`, run by
`scripts/test-index-declarations.sh` against a database built from the
production snapshot and the migration manifest. Each case runs in a transaction
that is undone.

RESULTS

## Not covered

- No test sends chat messages or webhook deliveries over HTTP; the cases write
  through the storage functions and the model types.
- The WhatsApp webhook still reads the clock once per delivery, so rows of one
  delivery share a timestamp and their order within it is by id.
