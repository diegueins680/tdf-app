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

## MKT-CART-001: discoverable Marketplace cart
- A header cart control with a count badge is visible at every width on Marketplace pages, and on other pages while the cart has items. It opens the cart in one interaction.
- Add, update and remove change the badge immediately. Adds keep the visitor on the product, show a confirmation and ignore repeated taps while pending.
- The cart survives refresh and login/logout in the same browser. Cross-device carts for signed-in users need a backend `cart.party_id` model.

## COURSE-ENROLL-001: immediate course enrollment
- "Inscribirme" (hero, sticky mobile CTA, `?inscribirme=1`) opens the enrollment dialog in view, focused on the first missing field.
- Signed-in users get account data prefilled. Guests may enroll and can sign in without losing their draft.
- Local Ecuador phone numbers are submitted in E.164 (`+593…`).
- Server errors appear beside the relevant field or as friendly form messages.
- The idempotency key is reused for identical retries and renewed only after a definitive rejection with a changed payload.

## SYS-WEB-LAYOUT-001: usable public layouts at 360px
- Booking, Live Sessions and pages with the docked radio bar do not squeeze sibling columns or overflow horizontally.
- Fixed bars never cover the page end or submit controls.
- Controls on dark surfaces keep at least 3:1 icon contrast and 4.5:1 text contrast.
