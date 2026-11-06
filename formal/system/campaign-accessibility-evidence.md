# Campaign accessibility counterexamples — SYS-VERIFY-CAMPAIGN-001

Authority: the owner's mission to verify the verification system and preserve
counterexamples. Hosted range PR671 run40971456580 reported a campaign contrast
failure but its projection discarded axe node/check data. The target was the unselected Creativos tab; the retained post-failure
screenshot shows it clipped while Calendario is selected. That screenshot is
not an atomic record of geometry during the asynchronous axe scan. This is an
evidence-completeness gap; it does not establish the cause of that contrast result.

Given a successful axe execution, a live page that can be inspected and writable
Playwright artifact storage, a nonempty campaign audit must attach the complete
unfiltered axe violations before the unchanged all-violations assertion fails.
Preserve node targets, checks, colors/ratio and failure summary when axe supplies
them. Record before/after observations of tab rectangles, ancestor CSS, viewport,
scroll values and fonts. Every violation and the existing WCAG2A/2AA/21AA/22AA
rule scope remain; do not add a severity filter, retry or longer deadline.

Geometry samples are observations, not a paint-readiness guard or a single atomic
snapshot covering the asynchronous axe scan. The pre-scan read can affect layout
and timing. The rectangular clip intersection is an axis-aligned CSSOM estimate,
not hit testing, compositor state, visual-viewport, iframe/shadow traversal or a
complete model of transforms, rounded/path clips and overlays. WAAPI observations
do not cover MUI requestAnimationFrame scrolling. No waiting, animation disabling,
color change, clipping exclusion or contrast-threshold relaxation is introduced.
A page/attachment failure may still abort the assertion path; it must never turn
a failing audit into a pass, and guaranteed artifacts under teardown/IO faults
are not claimed.

The original WebKit contrast cause remains UNKNOWN.25 supported-Mac Chromium
baseline scans did not reproduce it. Pinned Playwright1.59.1 WebKit cannot run
on this mac12 host; supported-browser evidence is not WebKit qualification.
Independent review found the existing opacity guards unsuitable as a drop-in
scroll predicate. Parent671's failed gate stays failed. PR673's separately fresh
hosted quality passed; its successful sample does not invalidate prior failures.

Verification obligations: actual axe and Playwright attachment controls with a
known bad-contrast element and a genuinely clipped tab; preserved full rule
results; original320/1280 campaigns; sensitivity to the old lossy projection and
missing attachment; byte restoration and source-bound execution receipts.
Finite browser tests support TESTED evidence only. No new complete proof or
claim that the original contrast failure is repaired follows.
