# Booking time helpers stay inside the content viewport

Parent requirement: `BOOK-SERVICE-PICKER-001`. Related measurement contract:
`BOOK-VIEWPORT-BOUNDS-001` in `booking-viewport-bounds.md`.

`BOOK-TOOLTIP-VIEWPORT-001`: an open suggested-time helper must not enlarge the
document beyond the requested mobile content viewport or cause automatic page
zoom when the booking form reflows. Its helper text and accessible name remain
available to keyboard users. At the first atomic measurement after the requested
320px client viewport is established, root scrollWidth must be <= clientWidth,
visual scale must remain1. Root overflow itself has zero tolerance. A later fit
cannot replace that initial result. Helper placement is checked once after the
requested native window resize event and its queued update have run: the helper
must fit within one physical pixel of the client bounds, retain its text/name
and keyboard focus, with root bounds and scale checked again.

Authority is the existing public booking narrow-screen/no-overflow check and
the established keyboard-accessible catalog-recovery workflow. The helper is
an overlay, not additional booking content. No tooltip dismissal, CSS clipping,
disabled hover/touch/focus behavior or wait-for-overflow-to-disappear is a repair.

## Concrete counterexamples and correction

On the unchanged application UI, three controlled interactions compare a helper
open at390px before shrinking to320 (A), absent before resize (B), and opened
after already reaching320 (C). Chrome desktop and Firefox fit in all variants;
Chromium with the iPhone13 profile fails only A. Its numeric first result is
innerWidth362/clientWidth320/scrollWidth362, visual width361 and scale0.8864.
The helper retains x99/right361.05. B and C retain320/320/320 and scale1. This
isolates a local resize-with-open-helper path; it is not a proof of every browser.

The only product correction selects Popper's existing fixed positioning strategy
for these suggested-time tooltips. It retains content, ordinary interactions,
portal and scroll/resize listeners. Installed Popper uses visualViewport width
when computing its viewport rectangle for both strategies. The observed
absolute-overlay extent and mobile zoom are consistent with a feedback path;
the measurements alone do not formally establish the browser's full layout
algorithm. Fixed positioning must be accepted on observable bounds and behavior,
not merely on the presence of that configuration option.

The permanent regression uses real Tab navigation from the date input to the
third suggestion, with the pointer away. A fixed synthetic Date keeps that slot
available regardless of CI execution time; retry/transition timers continue.
The focused helper remains open across resize, preserving its name and text.
The same original first overflow assertion is retained. Removing only the fixed
strategy makes this actual regression fail:362px document width in a320px client,
scale0.8864, and the same overflow in all four later samples while focus remains.
This is a persistent keyboard case, separately from the earlier transient hover
observation. Exact correct source is restored after the controlled mutant build.

After the immutable initial assertion, the test places the still-focused anchor
fully in view. A separate40px scroll then checks actual scroll and anchor
movement, helper bounds/content/focus and scale. With unchanged Popper
placement, helper and anchor displacement must agree within the documented
device-pixel tolerance plus0.05px arithmetic allowance. A legitimate placement
flip does not imply equal displacement. Centering uses one animation-frame
observation with a250ms fallback. The tracking scroll registers a listener before
scrolling, requires the actual scroll event within8seconds, then measures after
the following task so Popper's event-driven microtask can update. A frame alone
can precede delivery of the scroll event in Firefox; it is not a convergence
contract. No product overflow is polled until it becomes acceptable.

## Assurance and limitations

Implementation is TESTED only by passing source-bound execution receipts.
Controlled A/B/C and actual option-removal results are empirical evidence, not
CSSOM or React/Popper refinement proofs. Existing conditional integer-width
theorems remain scoped to their arithmetic assumptions; no new whole-platform
proof is claimed. The former shared-driver attempt that timed out between
projects remains PARTIAL; isolated profile results must be reported separately.

The earlier PR638535 Linux WebKit run measured initial340/320 overflow and later
fit; its tooltip attribution was incomplete. The original84d run has no numeric
widths. Neither cause is retroactively proven by this local Chromium experiment.
Current Linux WebKit, hosted CI, current-head independent review, current-main
composition and protected FIFO integration remain necessary. Gallery contrast
is separate: retain approved648/651 for their slots without a duplicate patch.
No API, database, backend, dependency or production financial state is changed.

## Historical hosted failure and observation scope

Source21628's Linux WebKit iPhone run passes the original initial root-width
and scale guards but fails both new helper-edge checks at339.640625px in a320px
client. The literal initial-overlay contract at that source remains FAILED. The
observed old fixed transform persists75–92ms through final trace snapshots;
the trace contains no DOM resize-delivery or Popper-update milestone. At that stage event timing, a missing visualViewport signal and other update
causes were unresolved hypotheses.

Source6252 / public a909 diagnostic records bounded window/visualViewport resize events
and positioner style/placement mutations before the viewport request. Event
callbacks read no layout and do not update Popper, scroll or mutate application
state. Every initial atomic result is attached; ANY initial root, helper or scale
failure collects the same bounded later geometry plus event timeline. Observer
and listeners are disconnected before assertions, with page teardown covering
exceptions. The original initial measurement still supplies every unchanged
assertion: a later fit can never turn a failure into a pass. Observation can
perturb scheduling and is not itself a product repair or proof of causality.
That observation-only source preserves the literal initial-overlay assertion;
it does not claim a product repair. Its evidence is historical and not replaced
by the specification correction below.

A saturation flag makes the128-event cap visible; no event-absence conclusion is
supported after truncation. Mutation records retain the prior attribute value
and current inline style, with batching still possible. The initial snapshot
identifies whether the current helper is the observed node and whether that
node remains connected; the observer does not follow a remount automatically.


## Native resize phase correction

The a909 hosted run40833156449 provides the missing milestone. Both original
first root measurements remain320px with scale1, while helper right339.640625
is outside. In the error case the first native window.resize and
visualViewport.resize arrive at11518ms,138ms after the11380ms measurement;
in the empty case they arrive at4316ms,220ms after the4096ms measurement.
At each delivery the same connected Popper node changes from the old transform
to the corrected transform; later helper right319.97396 fits. The bounded
observers are not truncated. These cases do not support a missing
visualViewport-listener diagnosis or establish painted clipping.

This is a specification/harness defect in the added helper phase: observing
clientWidth does not acknowledge delivery of resize notification. CSSOM View
[13.1, published draft16September2025](https://www.w3.org/TR/2025/WD-cssom-view-1-20250916/#resizing-viewports)
defines window/visual viewport resize notification in resize steps. The HTML
[rendering task](https://html.spec.whatwg.org/multipage/webappapis.html#update-the-rendering)
runs these steps during rendering; a viewport protocol response or layout read
is not the event milestone. These specifications explain the phase boundary;
they do not prove Popper correctness or prescribe an8second product SLA.

The corrected oracle preserves the original immutable first root/scale result.
Before requesting320x740 it registers a listener requiring a trusted window
resize at a positive client width<=320 and height740. It accepts no fit predicate,
dispatched event, forceUpdate, tooltip dismissal, scroll, clipping, or extra
product listener. Missing notification fails closed within the existing8second
assertion budget. The first following task acknowledges the installed Popper
resize handler's Promise microtask; one subsequent helper measurement supplies
all strict bounds/content assertions. No fit/convergence polling is introduced.
The same visible-anchor scroll check follows. Timers/listeners/observers are
cleaned in finally, including an earlier immutable-root failure.

Pre-notification helper geometry remains attached as diagnostic evidence;
the earlier literal helper assertion remains FAILED in the old receipts.
Corrected-phase evidence is TESTED only when the exact new body executes.
Both instrumentation-free execution (retaining only the required native-event
acknowledgment) and independent fault controls must qualify: removing the sole
fixed strategy still fails the first root guard, a persistently misplaced helper
still fails after notification with a fitting root, and missing native resize
still fails. Historical21628's invalid pre-notification helper phase is not a
success criterion for the corrected oracle. Hosted Linux WebKit qualification,
independent exact-head approval, gallery/FIFO sequencing and all protected
checks remain required. No product code is changed by this phase correction.
