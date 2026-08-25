# Web/mobile parity assessment

The feature registry is the parity authority. Current treatments are 37 native, 15 native-contextual, 20 explicit external-web, 47 web-only, 10 security-concealed and 10 technical. “Parity” means a truthful, safe treatment—not that every feature must be rebuilt natively.

## Native strengths

The Expo app has real screens for authentication/onboarding, social/community, artist profile creation and detail context, bookings/DJ booth, parties/settings, operations/inventory-related workflows, events, ticket purchase/checkout, course/event-related contexts and pipeline/catalog slices. Mobile unit/component coverage is broad: the executed run passed 49 suites/256 tests. Native ticket checkout specifically covers unavailable tiers, accessible quantity changes, validation, cancellation release, ambiguous provider status, idempotent retry and recovery guidance.

## Intentional web continuation

| Area | Current treatment | User-continuity requirement |
|---|---|---|
| Public platform/commerce/distribution | External responsive web | Deep link to stable public URL; never imply authenticated release management is public |
| Home | External responsive web after PT-001 | Preserve locale/query/referrer; return to app where supported |
| Directory search and public profile/classified/event/venue detail | Native | Keep server projections authoritative and preserve record-specific navigation/back behavior |
| Classified management | Authenticated native | Preserve protected intent through login and keep edit/submit operations capability-scoped |
| Marketplace/service checkout | Web | Preserve authoritative cart/order state; no duplicate charge on app/browser transition |
| Booking/order tracking | Capability-bearing contextual web link | Never place capability/token in logs or analytics; support recovery from confirmation |
| Trials, Live Session intake, courses/registration | Shareable web | Keep consent/files/forms accessible and retain entered state under interruption |
| Domo discovery/quote | Public/contextual web | Carry inquiry/quote identity safely through acceptance, deposit and staff follow-up |
| Release creation and browser audio tooling | Web | Do not render native placeholder success; make upload/draft status authoritative |

PT-001 correctly removed seven false native destinations when their screens did not exist. The completed directory implementation now restores directory search, four directory detail families and classified management/quick-create to six real native routes; home intentionally remains an explicit web continuation. Generated registry, route-audit and mobile tests enforce that every native destination exists.

## Missing or incomplete equivalents

- Native direct messaging has no dedicated screen.
- Native release authoring is absent; contextual viewing is not authoring parity.
- Teacher, intern and broad administration workflows remain primarily web-only.
- Native contract create/detail is incomplete and concealed.
- Directory moderation remains web-only even though public discovery, detail and classified management are native.
- Marketplace catalog/secure checkout and most public lead/commerce surfaces remain web-based.
- Android Detox configuration is absent; the configured simulator is iOS-only and tied to a local iPhone 16 identifier.

## Verified consistency and gaps

Verified locally:

- Feature audit: 139 features, 159 web routes and 44 Expo routes resolve.
- Mobile lint/typecheck/Jest: pass; generated registry assertions pass.
- Chromium phone/tablet public discovery preserves filters and reflows at 320 CSS pixels.

Not demonstrated:

- Native-to-web authentication/session continuity and return-to-app links.
- State preservation for cart, booking, quote, upload or payment when leaving native.
- Android rendering, back behavior, accessibility services and offline synchronization.
- Web/native localization parity outside Spanish.
- End-to-end notifications, file uploads, OAuth, provider payment sheets and distribution.

## Parity acceptance policy

A feature may remain web-only when the registry says so, the mobile UI names the transition before launching it, the URL is safe and record-specific, auth/locale/state are preserved without leaking capabilities, and the responsive web flow meets keyboard/reflow/accessibility requirements. A native feature must resolve to a real route and provide the complete authoritative lifecycle; placeholders and fake success are prohibited.
