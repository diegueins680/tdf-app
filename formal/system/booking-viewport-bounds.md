# Booking viewport verification uses the client content width

Parent requirement: `BOOK-SERVICE-PICKER-001`.

`BOOK-VIEWPORT-BOUNDS-001`: the browser check must reject document content whose
scroll width exceeds the client viewport width, even when mobile `innerWidth`
grows with that content. The booking recovery check first requires a positive
client width no greater than the requested320px. Its overflow assertion remains
a one-shot check in standards mode (as the application doctype requires); subsequent diagnostic observations cannot turn failure into
success. Existing case30s/assertion8s budgets and zero retries are retained.

The controlled counterexample is an ordinary visible body child with width
`calc(100vw + 80px)` after requesting320px. The old comparison fails desktop
and Firefox but passes Chromium phone. Numeric browser measurements on that
passing phone are innerWidth400, clientWidth320, scrollWidth400, visual width320,
visual scale1 and offender right400. The old `scrollWidth > innerWidth` predicate
therefore accepts80px beyond the visible content viewport. This is a confirmed
verification defect, distinct from any cause of the original hosted failure.

The shared measurement reads numeric widths and its new
`scrollWidth > clientWidth` result in one browser evaluation. The permanent
browser regression inserts the controlled wide child, requires detection, then
removes it and requires recovery. Reverting only the comparator to innerWidth
must fail that regression on the mobile profile. The business recovery cases
continue to preserve contact data, verify keyboard/accessible retry and forbid
all business writes and external traffic.

An abstract guarantee is PROVEN under the assumption `clientWidth <= innerWidth`:
if `scrollWidth > innerWidth`, transitivity gives `scrollWidth > clientWidth`.
The new predicate thus retains every old rejection under that assumption, while
rejecting the formerly accepted interval `clientWidth < scrollWidth <= innerWidth`.
This is a width-arithmetic argument, not a proof of CSSOM, browser scheduling,
all page layouts or the complete test implementation. Actual implementation
and fault controls are TESTED; original WebKit attribution remains UNKNOWN.

On failure the test retains the initial result and widths and attaches later
numeric samples, control rectangles, tooltip/Popper placement and candidate
rectangles beyond client bounds. These candidates may be clipped by ancestors;
their rectangles alone do not establish a cause of root overflow. The scheduled
sampling waits are nominally at most650ms before computation overhead; the
unchanged30s case budget bounds execution. Later fitting geometry never replaces
the initial failed result. A transient-width control checks this distinction.

The original PR638 hosted run40796438713/job123194166119 failed its WebKit iPhone
booking error-catalog case with635 passed/22 skipped/1 failed. The raw trace has
only the old boolean, not its numeric widths or later geometry. Both390px and
320px already satisfy the existing max-width600px mobile branch, so the visible
step marker does not synchronize that resize. A tooltip retains its earlier
Popper position in the trace; persistent versus transient overflow and its
causal element are not established. No tooltip dismissal, CSS clipping or
Select/native-input repair is justified by that evidence.

Local Chrome desktop, Chrome phone emulation and Firefox controls do not
qualify hosted Linux WebKit. macOS12 has no compatible cached current WebKit,
and the probed local Docker Unix daemon is unavailable. New hosted measurements,
required independent review and protected FIFO/current-main admission remain
necessary. No product UI, API, database, workflow policy or production data is
changed by this verification correction.
