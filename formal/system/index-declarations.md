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

- Sending a conversation message inserts a `chat_message`: the database
  function `social_v2_chat_send` where the social functions are installed, and
  `chatSendMessage` otherwise. The second message in a conversation fails with
  a unique violation, which the server answers with 500.
- A contact started from a directory profile (`contactProfile`) reuses the
  conversation between the two accounts and inserts a message, so a second
  contact between the same two accounts fails the same way.
- The WhatsApp webhook reads the clock once per delivery. It stores the inbound
  message with that time and records what it sends in answer (the enrolment
  reply, the consent confirmation) with the same time and telephone number. The
  answer has already been sent when the record is refused, and the error is not
  caught, so the webhook request fails. If the provider then delivers the
  message again, the enrolment branch runs again. Two messages from one sender
  in one delivery fail the same way.
- A follow-up whose reminder time equals that of any other follow-up is refused
  by the constraint. The staff screen that sets it was not traced.

None of the nine is used as a unique key to look a row up, no statement names
them in an `ON CONFLICT` clause, no foreign key refers to them and no code
treats a violation of them as a duplicate guard.

The other fourteen lines have no constraint in the snapshot. They made a
difference only to a database built from the models (local development).

## What changed

- `tdf-hq/sql/2026-10-11_index_declarations_not_unique.sql` drops each of the
  nine constraints and creates a plain index with the same name and columns,
  with a ten-second lock timeout. It ends by failing if any of the nine names
  is still unique. It changes no row and can be applied more than once.
- `tdf-hq/sql/2026-10-11_index_declarations_not_unique_rollback.sql` restores
  the constraints. It fails, changing nothing, once any table holds rows they
  forbid, and can be applied more than once.
- The twenty-three lines are removed from `TDF.Models` and `TDF.ModelsExtra`, so
  a database built from the models no longer creates the constraints. No code
  referred to them.

The identity constraints are untouched: a WhatsApp provider message id, a
conversation's pair of participants, and a registration's payment identifiers
remain unique.

## What changes when this is applied in production

- Conversations accept a second and later message, from the conversation
  screen and from a directory contact. Until now each conversation could hold
  one.
- The WhatsApp webhook records its enrolment reply and consent confirmation
  instead of failing after sending them.
- Requests that failed with a unique violation on one of the nine names succeed.
- The migration runs in one transaction, so the exclusive locks on the five
  tables are held until all nine indexes are built. The size of the production
  tables was not checked; if a lock cannot be taken within ten seconds the
  migration fails and changes nothing.
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
| `IndexDeclarationKeyUnique.cfg` | adds unique key (`chat_message`, the reminder time) | `NothingNewRefused` violated |
| `IndexDeclarationKeyTimeUnique.cfg` | adds unique key and time (seven of the nine) | `NothingNewRefused` violated |
| `IndexDeclarationIdentityNotUnique.cfg` | removes unique identity | `IdentityStoredOnce` violated |

This is bounded model checking of an abstraction. It restates what each kind
of constraint permits; it does not model the application's code, time
precision, or concurrent transactions, and is not evidence about them.

## Tests

`tdf-hq/test/TDF/IndexDeclarationSpec.hs`, run by
`scripts/test-index-declarations.sh` against a database built from the
production snapshot and the migration manifest. Each case runs in a transaction
that is undone.

Run locally on 2026-10-11 against PostgreSQL 16:

- With the migration applied: 11 cases, 0 failures.
- On the same database without it: 10 of the 11 fail, each on a unique
  violation of one of `index_chat_message_thread`,
  `index_whats_app_message_phone`, `index_whats_app_message_party`,
  `index_course_registration_party`, `index_course_registration_receipt_party`,
  `index_course_registration_follow_up_party` and
  `index_course_registration_follow_up_next`, or on the schema check. The
  eleventh (a message delivered twice is stored once) holds either way.

The run without the migration was made by hand; CI runs only the migrated
schema. The two `registration_id, created_at` constraints are covered by the
schema check, not by a case of their own, because a row that violates them
also violates the matching `party_id` constraint first.

The migration was also applied twice, rolled back twice and applied twice more
on a copy of that database, and its final check was seen to fail when one of
the names was made a bare unique index.

## Not covered

- No test sends chat messages or webhook deliveries over HTTP. WhatsApp rows,
  follow-ups and one conversation case go through the functions the application
  calls; the other conversation case, registrations and receipts are inserted
  directly.
- The conversation send function was exercised with the social runtime not
  activated (two people who follow each other); the activated mode was not.
- The WhatsApp webhook still reads the clock once per delivery, so rows of one
  delivery share a timestamp and their order within it is by id.
