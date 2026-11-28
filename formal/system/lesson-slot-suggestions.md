# Slot suggestions for one lesson booking (SCHOOL-SLOT-SUGGESTION-001)

`GET /trials/v1/class-sessions/suggestions` answers the best feasible slots for one
new lesson (teacher + student + subject, room given or chosen). It only suggests: it
never inserts, moves or cancels a `class_session`, `trial_assignment` or `booking`.
It is off unless `LESSON_SLOT_SUGGESTIONS_ENABLED=true`.

The product owner approved the rule, the schema route and every constant below on
2026-11-21 (America/Guayaquil), before any implementation was written.

## Constraints that existed before this change (main 985117b3d)

| What | Where | Enforced by |
|---|---|---|
| A resource holds one active allocation at a time, half-open `[starts_at, ends_at)`, for `allocation_status IN ('holding','reserved')` | `service_booking_resource_allocation`, `tdf-hq/sql/2026-08-16_service_booking_checkout_runtime.sql` | PostgreSQL `EXCLUDE USING gist` |
| `booking` + `booking_resource` rows project into that table in the same transaction | `tdf-hq/sql/2026-10-05_booking_calendar_projection.sql` | Triggers |
| Active allocation statuses, half-open interval, 409 on conflict | `formal/system/booking-policy.json`, `formal/system/bookings.md` | Handler + database |
| Lesson end after start | `createClassH`, `tdf-hq/src/TDF/Trials/Server.hs` | Handler only |
| Teacher has no overlapping `class_session` or `trial_assignment` | `teacherAvailable` | Handler only, unlocked check-then-insert |
| Room has no overlapping `class_session`, nor a `booking` on that resource that is not Cancelled/NoShow | `roomAvailable` | Handler only, unlocked |
| Room is a Room resource, allowed for the subject when `subject_room_preference` rows exist | `ensureSchedulableRoom`, `ensureRoomAllowed` | Handler only |
| Teacher holds the Teacher role; a non-staff teacher teaches the subject and owns the student | `ensureTeacherSelection`, `ensureTeacherSubject`, `teacherOwnsStudent` | Handler only |

What that meant: lessons had **no** exclusion constraint (`SCHOOL-CLASS-SESSIONS-001`
recorded "Double booking is possible between the conflict check and the write"), a
lesson did not occupy the room calendar that studio bookings use, and a student could
hold two lessons at once. The owner chose to close this in the database (decision D1).

## Schema: one calendar (`school_schedule_occupancy`)

`tdf-hq/sql/2026-11-21_school_schedule_occupancy.sql` adds one row per occupied
teacher, student or resource and one constraint,

    EXCLUDE USING gist (subject_kind WITH =, subject_id WITH =,
                        tstzrange(starts_at, ends_at, '[)') WITH &&)

fed by triggers in the writer's transaction:

- a `class_session` occupies its teacher, its student and its room;
- a `trial_assignment` occupies its teacher and its room (not its student: anonymous
  public requests share one fallback party);
- a `holding`/`reserved` row of `service_booking_resource_allocation` occupies its
  resource; any other status frees it.

So the database rejects (SQLSTATE 23P01) a second lesson for a teacher or a student, a
lesson over a trial, and a lesson over a studio booking of the same room, in either
order. The trials server answers that refusal with 409 instead of 500. The backfill
projects only rows that end in the future; a pre-existing future overlap or an empty
interval aborts the migration and nothing is chosen for the operator. Writers must be
drained first. Not enforced by the database: a legacy `booking` that never received an
allocation row, and a trial student's overlap.

## Rule

Inputs: `teacherId`, `studentId`, `subjectId` (required), `roomId`, `durationMinutes`
(default 60; 30..180 in steps of 30), `from` (default now), `days` (default 14, 1..28),
`limit` (default 5, 1..20), up to 5 preferred windows (`preferredStart`/`preferredEnd`
pairs). Out-of-range values are refused with 400, never clamped.

Horizon: starts at the later of `from` and now, lasts `days` × 24 h. Candidates: every
start on the 30-minute grid whose interval fits the horizon, for each candidate room.

Hard constraints (a candidate is feasible when all hold on the snapshot):

- H1 teacher free: overlaps no teacher row of the calendar.
- H2 room free: overlaps no resource row of the calendar for that room, and no
  `booking` on that room that is not Cancelled/NoShow (what `roomAvailable` refuses).
- H3 student free: overlaps no student row of the calendar.
- H4 room is an active Room resource; allowed for the subject when the subject has
  room preferences; equal to `roomId` when given.
- H5 opening hours: the whole interval lies in 08:00–22:00 `America/Guayaquil` on one
  day (UTC−05:00 all year; Ecuador has no daylight saving).
- H6 notice: starts at least 12 hours after the request.
- H7 the teacher teaches the subject (`teacher_subject`).
- H8 availability: the interval lies inside the union of the teacher's
  `teacher_availability` windows for that subject **and that room**.
- H9 package: the student has a `class_package_purchase` with status `Open` for a
  package of the subject, and the lesson ends by `purchased_at + expires_days`.

Objective, lexicographic, smaller is better:

1. outside every preferred window (0 inside or when none is given, 1 outside);
2. idle minutes added to the teacher's day: whole minutes to the teacher's nearest
   commitment on the slot's studio day, 0 when adjacent, 840 (the length of the opening
   day) when that day has no commitment;
3. room priority (`subject_room_preference.priority`, lower first; 1 when unset);
4. start instant;
5. room id.

Output: `computedAt` and up to `limit` slots `{startAt, endAt, roomId, roomName,
insidePreferred, teacherGapMinutes}` in objective order, **one per start instant** (its
best room). Empty exactly when no candidate is feasible. Revenue is not an objective:
the price of a lesson comes from the package, not from the slot.

## Boundaries

- **Authorization.** `createClassH`'s: school access (Scheduling module and Teacher or
  staff role); a non-staff caller must be the teacher, teach the subject and own the
  student. 403 otherwise. Students cannot call it.
- **Privacy.** The answer holds free slots only. It never names who occupies a busy
  slot and does not say whether the teacher, the student or the room was busy.
- **Personal data.** No behavioural history is used. Preferred windows are typed into
  the request, never stored or inferred. Invariant (tested): preferred windows never
  change which slots are feasible, and a request without them is ranked by criteria
  2–5 alone — that is the non-personalised fallback. There are no stored preferences,
  so there is nothing to opt out of yet.
- **Follow/contact policy.** The route offers no party to follow or contact;
  `social_v2_profile_eligible` does not apply.
- **Concurrency.** The whole snapshot is one `SELECT` (`slotSnapshotSql`), so it is read
  at one instant. Existence and authorization checks precede it in the same
  transaction. A suggestion is advice: creating the lesson re-validates and the
  exclusion constraint decides; a slot taken meanwhile answers 409 and nothing is
  double-booked.
- **Determinism.** The answer is a function of the request, the clock reading
  (`computedAt`) and the snapshot; the order is total.
- **Failure.** 400 malformed, missing or out-of-range parameter; 401; 403; 404 unknown
  teacher, student, subject or room, and 404 while the flag is off; 422 a room that is
  not a room or not allowed for the subject; 500 if the snapshot cannot be decoded.
- **Rollout.** Web only. `LESSON_SLOT_SUGGESTIONS_ENABLED` (default false) gates the
  route; the session reports it in `featureFlags` and the "Nueva clase" dialog shows the
  panel only then. The migration is independent of the flag: it protects lessons as
  soon as it is applied.

## Implementation

| Part | Where |
|---|---|
| Search, cost, SQL (pure core + one query) | `tdf-hq/src/TDF/Trials/SlotSuggestions.hs` |
| Handler, 409 mapping | `slotSuggestionsH`, `runTrialsAction` in `tdf-hq/src/TDF/Trials/Server.hs` |
| Calendar | `tdf-hq/sql/2026-11-21_school_schedule_occupancy.sql` (+ `_rollback.sql`) |
| Verification oracle (not in the runtime image) | `tdf-hq/app/SlotSuggestionsOracle.hs` |
| Contract | `suggestLessonSlots` in `tdf-hq/docs/openapi/api.yaml` |
| Web | `tdf-hq-ui/src/features/school/LessonSlotSuggestions.tsx`, `ClassesPage.tsx` |

## Evidence, by strength

Proofs, bounded models and tests are different things; none below is stronger than
its row says.

| Property | Evidence | Strength | Trusted / not covered |
|---|---|---|---|
| `overlaps` ⇔ two non-empty `[)` ranges share an instant (what `&&` tests) | Z3, `scripts/verify-lesson-slot-suggestions.py`, expression read from the Haskell source; negative control with closed intervals | **Proved** for all integer instants | The reading of `tstzrange && tstzrange`; the 60-line translator |
| Soundness: a feasible candidate inserts without an exclusion violation on its snapshot | `scripts/test-lesson-slot-suggestions-sql.py` with the oracle: every candidate of a fixture day is inserted on PostgreSQL 16 and compared with the compiled core; infeasible-by-overlap candidates are rejected (negative control) | **Tested** on one fixture; rests on the proved predicate | Not a proof over all database states; legacy bookings without allocation are covered by H2 but not by the database |
| A stale suggestion is rejected and never double-books | `formal/school/LessonSlotAdvice.tla`: TLC (3 clients, 2 slots); Apalache inductive invariant for every set of ≤4 clients and ≤3 slots, any number of steps; negative controls: without exclusion two clients book one slot, and advice does go stale | **Proved for the model** (inductive); bounded in clients and slots | The model, not PostgreSQL: one cell per slot, no interval geometry, no cancellation; that the triggers run in the writer's transaction is tested, not proved |
| Optimality of the first suggestion | Z3 against the compiled core on **every** instance of a stated family (one day, 2 rooms, 14 grid starts, ≤2 teacher, ≤1 student and ≤1 per-room commitments, 3 availability options per room, ≤1 preferred window, 2 package expiries, 2 priority orders, 30/60 min: 48 384 instances); negative control: the runner-up is refuted | **Proved per enumerated instance** against an SMT restatement of this contract | Larger calendars are covered only by the QuickCheck property |
| Completeness, order, determinism, fallback invariant, gap, validation | Hspec + QuickCheck against a brute-force reference, `tdf-hq/test/TDF/Trials/SlotSuggestionsSpec.hs` | **Tested** | Random small calendars |
| No writes | The query is one `SELECT` checked for write keywords (Hspec) and run inside `BEGIN TRANSACTION READ ONLY` on PostgreSQL | **Tested** | The handler's other statements are reads by inspection |
| Migration: backfill, repeatability, abort on overlap, triggers on insert/update/delete, rollback | `scripts/test-lesson-slot-suggestions-sql.py` | **Tested** on table shapes extracted from the production schema fixture | Not run against production data |
| Web panel hidden and silent while the flag is off | Jest | **Tested** | |

CI runs the overlap proof, the TLA+/Apalache obligations, the SQL test and the
oracle-against-PostgreSQL correspondence. The bounded optimality family needs the compiled
oracle and Z3 in one job and is run by hand (`python3 scripts/verify-lesson-slot-suggestions.py
--oracle <binary>`), not in CI.

Not verified: the route over HTTP against a fully migrated database; the migration
against production data (existing future overlaps would abort it); behaviour under
PostgreSQL versions other than 16.

## Decisions recorded

- D1 one occupancy table (approved). D2 objective order above (approved). D3 hard
  constraints H5, H6, H8, H9 (approved). D6 constants (approved). D7 callers are staff
  and the teacher (approved). Stored student preferences are not in version 1.

## Open for the owner

- H8 reads an availability window as "this teacher, this subject, **this room**". If a
  window should allow any permitted room, H8 changes.
- H9 checks that a package is open and unexpired, not that hours remain: attendance
  does not consume purchased minutes today (`SCHOOL-CLASS-SESSIONS-001`).
- `teacher_availability` is filled for trial lessons today; a teacher without windows
  gets no suggestions.
- Lesson creation itself does not enforce H5, H6, H8 or H9; only suggestions do.
- A trial student's overlap is not enforced by the database (shared fallback party).
- Production must be checked for existing future overlaps before the migration:
  the dry-run query is in the pull request.
