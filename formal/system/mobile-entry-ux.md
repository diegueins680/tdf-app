# Mobile entry, landing and commerce UX contract

Source: a real-user test on a Redmi Android phone in Chrome (2026-10-07), with four screenshots and the tester's report, reproduced under 360px emulation in PR #512.
This contract covers web (`tdf-hq-ui`) behavior only. Backend authorization and validation stay authoritative.

## SYS-WEB-SHELL-001: no blank application shell
- A valid navigation ends either in rendered content or in a recovery state with actions (reload, Comunidad, sign in).
- This holds even when the entry bundle fails to load, a provider throws, a chunk from an older deployment is gone, or `/session` stalls.
- `BootErrorBoundary` sits outside every provider. The `index.html` watchdog runs without the bundle. A stale chunk triggers at most one reload per deployed commit. Session bootstrap times out after 12s and keeps the stored session.
- Failures are reported as `client_error` events. Messages are redacted and only the route path is sent. Stack traces are never shown to users.

## AUTH-ENTRY-002: low-friction entry that lands on Comunidad
- Email signup asks only for email and password. Terms are acknowledged by clickwrap text next to the create button. The server still records `termsAccepted` and the supported `termsVersion`.
- A Google identity with no TDF account is offered explicit one-tap creation that reuses the same credential. Accounts are never linked by email alone.
- After authentication the user goes to the first of these that applies:
  1. a validated same-origin `redirect`/`returnTo` that the returned roles can access;
  2. an explicit onboarding intent;
  3. Comunidad (`/fans`), whose artist discovery precedes promotional cards.
- External, protocol-relative, backslash and `/login` targets are rejected.
- A rejected recovery token is explained in the active ES/EN locale and offers a new-link action preserving the safe return path and language. Only the reset-confirmation endpoint's known invalid/expired-token response at HTTP400/401 gets this classification; login credential errors, server failures and other validation retain their own meaning. Reset expiry and single use remain authoritative backend decisions.

## MKT-CART-001: discoverable Marketplace cart
- A header cart control with a count badge is visible at every width on Marketplace pages, and on other pages while the cart has items. It opens the cart in one interaction.
- Add, update and remove change the badge immediately. Adds keep the visitor on the product, show a confirmation and ignore repeated taps while pending.
- A failed cart read is an explicit recoverable error, not an empty cart. The saved id and count survive; retry targets the same cart without creating another one. Initial and cached offline-paused reads show a waiting-for-connection status; reconnecting resumes the read and restores checkout only after success. Cached products may remain visible after a failed refresh, but checkout is blocked until the current read succeeds and is idle.
- The cart survives refresh and login/logout in the same browser. Cross-device carts for signed-in users need a backend `cart.party_id` model.

## COURSE-ENROLL-001: immediate course enrollment
- "Inscribirme" (hero, sticky mobile CTA, `?inscribirme=1`) opens the enrollment dialog in view, focused on the first missing field.
- Closing finishes the dialog exit before removing `inscribirme` with replacement navigation; other campaign parameters survive and focus returns to the enrollment trigger. Navigation must not leave a retained modal after closing.
- Signed-in users get account data prefilled. Guests may enroll and can sign in without losing their draft.
- Local Ecuador phone numbers are submitted in E.164 (`+593…`).
- Server errors appear beside the relevant field or as friendly form messages.
- The idempotency key is reused for identical retries and renewed only after a definitive rejection with a changed payload.

## SYS-WEB-LAYOUT-001: usable public layouts at 360px
- Booking, Live Sessions and pages with the docked radio bar do not squeeze sibling columns or overflow horizontally.
- Fixed bars never cover the page end or submit controls.
- Controls on dark surfaces keep at least 3:1 icon contrast and 4.5:1 text contrast.


## Automatic name privacy — AUTH-ENTRY-002

Email signup still asks only for email and password. A missing account name must use a neutral label rather than the private email or its local part. This also applies to the Google account-creation fallback when the verified provider profile supplies no usable name. A supplied provider name keeps its existing trimming semantics; credential email, role grants, linking, terms and existing accounts are unchanged. People can edit the public name through existing profile forms.

An isolated counterexample registered an artist with a fictional email, activated the profile and performed no profile PUT: an anonymous public-profile GET and directory search returned the email-derived marker before the first edit. The repaired email flow must return no such marker before saving, and must still publish a deliberately chosen artistic name after the ordinary save. Two LoginPage request tests fail on the old behavior and require a neutral name while preserving the actual credential email. The five signup-name cases retain coverage of aliases, punctuation and long private identifiers without publishing them.

The backend pure AccountNameSpec covers absent, empty, whitespace-only and supplied provider names. Actual Google/Apple provider login, historical account-name remediation and human comprehension are not established by these checks. This is a narrow default-name requirement, not a claim that all profile-publication or identity-consent obligations are proved.
