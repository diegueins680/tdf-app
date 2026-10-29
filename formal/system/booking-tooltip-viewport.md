# Booking time helpers stay inside the content viewport

Parent requirement: `BOOK-SERVICE-PICKER-001`. Related measurement contract:
`BOOK-VIEWPORT-BOUNDS-001` in `booking-viewport-bounds.md`.

`BOOK-TOOLTIP-VIEWPORT-001`: an open suggested-time helper must not enlarge the
document beyond the requested mobile content viewport or cause automatic page
zoom when the booking form reflows. Its helper text and accessible name remain
available to keyboard users. At the first atomic measurement after the requested
320px client viewport is established, root scrollWidth must be <= clientWidth,
visual scale must remain1, and the helper must fit within one physical pixel of
the client bounds. Root overflow itself has zero tolerance. A later fit cannot
replace that initial result.

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
