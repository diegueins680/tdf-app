# Public studio booking timezone

`BOOK-STUDIO-TIMEZONE-001` records the product owner's 26 October 2026
confirmation: the physical Quito studio operates 08:00–22:00 in
`America/Guayaquil`. A general display preference such as UTC does not move
that physical opening window. The visitor's local date/time remains the input
zone; its offset label uses the selected date, including daylight saving, and availability and submission retain the selected instant in UTC.

The public form accepts opening at 08:00 and an interval ending exactly at
22:00 Quito. Existing input normalization brings a too-early start to opening and a too-late
start back to the latest interval that fits. Validation refuses any remaining
interval before opening or after closing.
The first step names Quito and its opening hours. The schedule step presents
the conversion into the visitor's zone. Existing persisted UTC booking instants,
API contracts, resource assignment, payment rules and idempotency are unchanged.

The new browser regression uses a UTC visitor in January and a Europe/Madrid
visitor in July to exercise daylight saving and cross-midnight conversion.
All API calls are intercepted. The booking POST intentionally receives a
synthetic 409; its UTC payload is inspected and no success is displayed.
These checks establish scoped frontend behavior, not backend opening-hours
enforcement, actual availability, payment success or completed human bookings.

A production anonymous GET observation on source 63c56aaf showed UTC despite
11 selectable catalog offerings. That baseline and the owner's confirmation
motivated this correction; it is separate from catalog recovery PR636.
Protected CI, independent review, queue admission and deployed-source verification
are still required. No production environment change or timestamp migration is
part of this correction.

## Quick suggestions preserve the selected Quito date

Review comment4384399010 on source75b2cd1 identified a real regression:
January15 at13:00UTC means January15 at08:00Quito, but flooring the visitor
date to midnight before conversion produced January14 suggestions. Repeated
selection could move the booking back another day. Convert the selected instant
to the studio zone before taking the start of day. Quick selections must preserve
the Quito date; a late Quito slot may fall on the following visitor-calendar day.

The original rendered component fails with expectedJanuary15 and actualJanuary14.
The corrected form and strengthened UTC/Madrid browser cases click suggestions
repeatedly at opening and after a local-midnight crossing. Original75 full-CI
success remains historical; this correction requires its own hosted validation.
