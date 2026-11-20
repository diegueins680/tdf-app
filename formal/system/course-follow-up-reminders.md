# Course follow-up reminders

Requirement: `COURSE-FOLLOWUP-REMINDER-001`.

An admin schedules a reminder on a course registration follow-up by setting
`nextFollowUpAt` (the "Recordatorio (próximo seguimiento)" field or the
"Programar recordatorio" action in the registration dossier).

## Bell reminder

- `startCourseFollowUpReminderJob` runs every five minutes on every replica;
  advisory lock `8401320250714` lets one replica do the work per tick.
- A follow-up is due when `nextFollowUpAt <= now` and
  `nextFollowUpAt >= now - 2 days`; older reminders are never sent, so a long
  outage does not flood the bell.
- The recipient is the follow-up's `createdBy`. Rows without an author and
  cancelled registrations are skipped.
- `CourseFollowUpReminderDelivery (followUpId, dueAt, recipientPartyId)` is
  unique. The notification is inserted only after the delivery claim
  succeeds, so each scheduled time reminds once; rescheduling to a new time
  reminds again.
- The notification has type `course_follow_up_reminder`, target type
  `course_registration`, target id the registration id and target key the
  course slug. The shared web/Mobile notification destination registry does
  not list `course_registration` yet, so the bell opens the generic
  notification page. The dossier itself accepts
  `/configuracion/inscripciones-curso?registration=<id>&slug=<slug>`, which
  the calendar entries use.

## Calendar entries

Each follow-up with a reminder shows "Google Calendar" (a
`calendar.google.com/calendar/render?action=TEMPLATE` link) and
"Apple / Outlook (.ics)" (an RFC 5545 file built in the browser with a
`VALARM` ten minutes before). Both carry the registration name, subject,
contact data, the note and the dossier link. Nothing is written to a shared
calendar by the server.

## Schema

Production creates `course_follow_up_reminder_delivery` through the reviewed
migration `tdf-hq/sql/2026-11-20_course_follow_up_reminder_delivery.sql`
(rollback alongside); schema verification requires the table and its unique
claim constraint. Deleting a follow-up deletes its delivery claims.
