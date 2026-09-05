# Draft PR: onboarding-first account entry, privacy, and mobile continuity

## Problem

TDF onboarding and first-purchase journeys contained security/privacy risks and task dead ends: self-signup passed a chosen password into a welcome-email/log path; PostHog could receive reset-token URLs and personal identity properties; recovery transport failures appeared successful; general signup asked for unrelated optional choices; two acquisition links lost/doubled intent; mobile could retain a revoked session and trap anonymous ticket buyers behind a spinner; no-quote booking success sent customers toward a staff calendar; and an empty marketplace promised notifications without sending a request.

## Changes

- Remove chosen passwords, reset tokens, names, and emails from unconfigured email fallbacks; introduce credential-free self-signup confirmation content.
- Sanitize sensitive analytics query/properties, disable DOM autocapture, enable personal-data masking, and identify only with opaque party ID.
- Simplify signup to immediate fields, semantic forms, autocomplete, Enter submission, server-matching password policy, truthful recovery, bounded auth requests, and safe intent continuity.
- Correct TDF general CTA and distribution return parameter.
- Clear mobile auth state on authoritative null session and preserve event context for anonymous native ticket buyers through authentication.
- Keep quote-backed booking orders on secure public tracking while removing the staff-only destination from no-quote confirmations.
- Remove unsupported marketplace notification capture and keep booking/marketplace contact drafts in current-tab storage rather than persistent shared-device storage.
- Reject malformed public review payloads at the API boundary so the existing recoverable error state renders instead of dereferencing invalid data.
- Programmatically label repeated Domo campaign controls and correct the static audit's `inputProps` recognition.
- Add focused unit/browser/mobile tests, real before/after screenshots, lab performance evidence, and the consolidated audit.

## Evidence

- Audit: `reports/onboarding-first-ux-audit-2026-09-05.md`
- Screenshots/performance: `artifacts/ux-audit-2026-09-05/`
- Initial web focused tests: 5 suites / 18 tests passed; follow-up commerce/privacy/reviews tests: 6 suites / 30 tests passed (with retained pre-existing React `act(...)` warnings in marketplace tests).
- Full current public-persona browser regression: 16 passed / 2 intentional duplicate-device skips; focused commerce evidence also passed 4/4 on desktop and Pixel 7 with axe serious/critical checks and screenshots.
- Web typecheck/build passed; final bundle guard reports 5 preloads / 411,529 gzip bytes initial JS. Focused follow-up lint and the static UI audit are clean; full lint retains 102 existing warnings.
- Static-audit parser regressions: 5/5 passed; current UI source has zero static findings.
- Mobile full Jest: 64 suites / 316 tests passed; typecheck and lint passed.
- Backend/application and the 184-module test executable compiled/linked; the matched credential-content Hspec test ran 1 example with 0 failures. Existing compiler warnings remain visible; SMTP delivery is not claimed.
- Lab LCP remains about 4.3 seconds in both variants; no performance uplift is claimed.

## Risks and migrations

- No database migration, new dependency, price/policy change, role assignment change, or production action.
- General signup no longer asks for optional marketing choice or unrelated favorite artists; backend payload remains compatible (`marketingOptIn:false`).
- Analytics is intentionally less identifying; dashboards depending on username/display name/roles or DOM autocapture must use privacy-safe aggregate dimensions instead.
- Parent mobile pointer references a feature-branch commit already published on the mobile remote.
- Mobile review is tracked separately in [TDF-mobile draft PR #39](https://github.com/diegueins680/TDF-mobile/pull/39).

## Rollback

Revert the focused root commits and reset the parent submodule pointer to the prior published mobile commit. Do not restore credential-bearing email/logging or token-bearing analytics behavior; if UI rollback is necessary, retain the security/privacy commit.

## Remaining gaps

- Durable account-bound onboarding completion and action resumption.
- Mixed-language legal/auth content and account-bound onboarding/action resumption.
- Mobile query/deep-link normalization and physical-device accessibility checks.
- OpenAPI/client drift, current-tab contact ownership across account changes, and authenticated campaign screen-reader/keyboard validation.
- Controlled field Web Vitals, real provider delivery/payment/OAuth, and staging validation.

No merge or deployment is requested by this draft.
