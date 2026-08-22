# TDF onboarding UX audit

**Audit date:** 2026-08-20  
**Scope:** web acquisition and signup, mobile first launch and authentication, and the post-signup single-feature experiment  
**Method:** source and contract review, focused React/React Native/Haskell tests, typecheck, and comparison with the 2026-08-05 platform audit  
**Baseline:** findings describe the pre-implementation dirty working tree; the implementation status below records the fixes applied afterward

## Implementation status — completed 2026-08-20

The audited code defects are resolved across the mobile app, web app, auth contract, and feature registry:

- Signup now captures a non-security product intent, persists it by authenticated party, accepts legacy campaign parameters only as intent, and routes against the returned session. Artist and internship goals open governed access-request flows when the Customer account lacks access.
- The mobile first screen is reduced to one value block and two account actions. Mobile onboarding/auth and the paused treatment use one shared Spanish/English copy catalog, expose a language control, respect Reduce Motion, and meet the audited heading, label-in-name, contrast, safe-area, and touch-target requirements.
- The first screen uses overflow-safe vertical centering: it remains centered when content fits, but enlarged text starts at the top of a real scroll container instead of being clipped above the viewport. The web rendering also reserves space for the global offline banner.
- Phone collection is deferred. Password controls and guidance match the server's 8-character/72-byte Unicode policy. Web and mobile link the exact terms/privacy pages, require versioned terms acceptance for new first-party accounts, and keep marketing consent separate. New Google accounts cannot be auto-provisioned through the non-consented login path.
- Post-signup first value is explicit: follow a real artist (with an upcoming-events fallback), save an event, or submit a governed access request. Completion is durable and conversion events are limited to actual signups within 24 hours.
- `single-feature-onboarding-v1` is paused. Its implementation now assigns only after authenticated identity, uses the actual successful signup event, expires after 24 hours, records one-shot control/treatment exposure, persists completion, reduces feed probes, and distinguishes loading, offline, error, empty, and success states before any relaunch.
- Web/mobile now emit the shared no-PII funnel taxonomy for view, primary action, auth mode, signup start/validation/failure/completion, intent selection, first value, and onboarding completion. Acquisition links use canonical `intent=` values and session-authorized destinations.

Apple Sign In was not added because this repository has no Apple identity provider, server token verifier, native entitlement, or configured credentials. The two supported first-party methods—Google and email/password—are now consistent; adding an inert Apple button would create another broken promise.

## Pre-implementation executive verdict

TDF's onboarding has usable visual and accessibility foundations, but its central promise is currently unreliable: users are asked what they want to do, yet that intent is discarded or sent to an account that cannot fulfill it. The post-signup experiment then targets the wrong cohort, has no 24-hour expiry despite its stated hypothesis, and can repeatedly replace the app with an empty feed.

The highest-leverage move is not a visual redesign. It is to restore intent continuity:

1. Capture a product goal, not a security role.
2. Create the governed base account.
3. Route the user to one deterministic, authorized first-value action.
4. Measure that action for both control and treatment within a real time window.

Until those conditions are met, the `single-feature-onboarding-v1` treatment should be paused. Its current exposure and conversion data should not be used to make product decisions.

## Pre-implementation journey

```text
Campaign/web route ──> Login + signup dialog ──> governed Customer account ──> redirect
      │                        │                         │                    │
      │                        └ roles= is ignored      │                    └ may require a role the account does not have
      └ promises Fan/Intern/Artist intent               └ valid security policy

Fresh mobile install ──> benefit screen ──> mobile signup ──> Events
                                │                │              │
                                │                └ role choice is discarded
                                └ onboarding marked seen        └ 50% may receive moments gate

Moments treatment ──> most recent past event with content ──> react or exit
      │                              │
      ├ cohort means first local auth on first launch, not new signup
      ├ no 24-hour expiry; treatment returns after a cold start
      └ network/content failure looks like a legitimate empty state
```

## Pre-implementation scorecard

| Dimension | Score | Assessment |
| --- | ---: | --- |
| Value proposition | 3/5 | The benefit copy is understandable, but too broad and not role-specific. |
| Signup usability | 3/5 | Good fields and keyboard behavior; unnecessary data and misleading intent controls remain. |
| Intent continuity | 1/5 | Mobile role selection is a no-op and web `roles=` acquisition intent is ignored. |
| Time to first value | 2/5 | Everyone defaults to events; treatment may depend on unavailable past-event content. |
| Accessibility | 3/5 | Several earlier issues are fixed, but label-in-name, touch-size, contrast, locale, and hierarchy gaps remain. |
| Measurement quality | 1/5 | The funnel is incomplete and the active experiment does not implement its own cohort/window contract. |

## Priority findings

### P0 — Mobile asks for a role and silently discards it

The mobile signup renders Fan, Artista, and Profesor choices and stores the selection in local state (`app/auth.tsx:57`, `450-488`). `handleSignup` sends only name, email, password, and optional phone (`159-165`). The signup contract intentionally has no caller-selected role (`src/api/generated/types.ts:6429-6453`), and the backend's governed signup policy assigns Customer (`tdf-hq/src/TDF/Catalog/Security.hs:68-74`). The focused test even codifies that the account is created “without caller-selected roles” (`__tests__/AuthScreen.test.tsx:178-202`).

This is the most damaging issue because it creates a direct false promise. Choosing Artista or Profesor produces the same account, destination, and confirmation—“Ya puedes elegir tus entradas.”—as choosing Fan.

**Recommendation:** remove the control immediately or rename it to “¿Qué quieres hacer primero?” and persist it as a non-security onboarding intent. Use that intent only to personalize the next authorized task. Any privileged role must continue through the governed request, claim, or approval flow.

### P0 — Web acquisition intent and destination can disagree with actual access

Public links actively use combinations such as `roles=Fan`, `roles=Intern`, or `roles=Artista` plus a redirect. `LoginPage` opens signup from `signup=1` or a narrow `intent` list, but never reads `roles` (`tdf-hq-ui/src/pages/LoginPage.tsx:280-295`). After signup, it trusts the safe local redirect over the session-derived landing path (`625-660`). The backend correctly creates a governed Customer account by default.

The visible quick routes also overpromise:

- “Crear cuenta fan” opens the same general signup dialog (`1070-1087`).
- “Postular prácticas” opens that dialog without setting the internships redirect or creating an application (`1116-1134`).
- `/login?signup=1&roles=Intern&redirect=/practicas` can create a Customer account and then send it to a protected Intern route.

This is safe from an authorization perspective, but it is a broken acquisition experience.

**Recommendation:** replace role parameters with explicit product intents. Resolve the post-signup destination against the returned session before navigating. If the task needs approval, land on the relevant public application/access-request flow, not the protected destination.

### P0 — The post-signup experiment cohort and duration are invalid

The experiment claims to measure first reaction within 24 hours (`ExperimentProvider.tsx:38-44`), but no signup timestamp or expiry exists.

`FirstRunProvider` snapshots whether this is the device's first launch and marks the install seen immediately (`FirstRunProvider.tsx:57-70`). It later treats the first locally unseen authenticated party on that launch as a new user (`77-100`, `firstRunFlags.ts:104-123`). Consequently:

- an existing account logging in on a new install can enter the new-user treatment;
- a genuinely new user who explores on day one but signs up after relaunch can be excluded;
- a treatment user remains in the persisted `isNewUser=true` cohort indefinitely;
- “Ver eventos” exits only for the current component session (`NewUserOnboardingGate.tsx:102-113`, `251-254`), so the gate can return after a cold start.

**Recommendation:** pause the treatment. Reintroduce it only with a server- or auth-event-backed `createdAt`, a one-time exposure/completion flag, a maximum 24-hour eligibility window, stable identity-based assignment, and exposure events for both control and treatment.

### P1 — The first-value treatment depends on old and possibly absent content

The gate deliberately searches past events, probes up to five moment feeds, and displays at most three cards (`NewUserOnboardingGate.tsx:127-203`, `260-330`). The headline can therefore welcome a new user with an event that has already happened. If no moments exist, reactions are unavailable, catalogs are unsynchronized, or requests fail, the user sees an empty-state explanation instead of the promised action.

There is no error branch: network failure is indistinguishable from “Aún no hay momentos publicados.” The initial treatment can issue one event request, up to five feed probes, and another featured-feed query before delivering value.

**Recommendation:** make the first task deterministic and available without historical user-generated content—for example, follow one artist, save one upcoming event, or complete the profile's display name. If moments remain the treatment, seed a guaranteed eligible item and distinguish loading, offline, error, empty, and success states.

### P1 — Funnel measurement cannot explain onboarding performance

The mobile benefit screen emits no viewed, skipped, or CTA event. `signup_started` fires only when the user manually switches tabs (`app/auth.tsx:294-319`), not when onboarding deep-links directly to `mode=signup`. Mobile captures completion but not failure category, source, selected intent, return destination, or time to value. The web has a growth-attribution helper, but `LoginPage` does not call it.

The experiment emits a treatment view and eventual reaction conversion, but no control exposure and no enforced 24-hour conversion window. Assignment happens locally at app boot, before a known authenticated identity (`ExperimentProvider.tsx:67-101`).

**Recommendation:** adopt one cross-platform taxonomy and prohibit PII in properties:

- `onboarding_viewed`
- `onboarding_primary_clicked`
- `auth_mode_viewed`
- `signup_started`
- `signup_validation_failed`
- `signup_failed`
- `signup_completed`
- `onboarding_intent_selected`
- `first_value_completed`
- `onboarding_completed`

Useful properties are `entry_point`, `intent`, `platform`, `locale`, `return_to_shape`, `experiment_id`, `variant`, and elapsed milliseconds. Capture both experiment exposures after identity resolution.

### P1 — Consent and data use are not inspectable before account creation

Web says “Al crear la cuenta aceptas los términos…” but does not link to the terms or privacy content (`LoginPage.tsx:1389-1391`). Mobile signup has no equivalent disclosure. Phone is collected despite being optional and unused in the immediate first-value path.

**Recommendation:** provide accessible links to the exact terms/privacy documents before submission, record the applicable version where required, keep marketing consent separate, and defer phone collection until a feature actually needs it.

### P1 — Completion copy and destination ignore the user's context

Mobile always defaults to Events (`mobileSurface.ts:3`) and always confirms “Ya puedes elegir tus entradas” (`app/auth.tsx:171-173`), including for users who selected Artista or Profesor. The pre-auth screen advertises profiles, vCards, streaming, and fan clubs but offers no guided bridge to any of them.

**Recommendation:** preserve `entry_point`, safe `returnTo`, and product intent through signup. Resolve one authorized next action in this order: explicit task continuation, intent-specific public task, then general discovery.

### P2 — The first screen is clear but unnecessarily repetitive

The mobile screen presents a headline, subtitle, three benefit pills, three benefit cards, and three account actions. Returning users get “Ingresar” in the hero and “Ya tengo cuenta” below. On short phones or at large text sizes, the primary CTA follows roughly 565 px of content and may start below the first viewport.

**Recommendation:** keep one headline, one proof/value block, one primary “Crear cuenta” action, and one secondary “Ingresar” link. Deep-linked users should skip the generic benefit screen and see acquisition-specific context directly above signup.

### P2 — Localization and Ecuador context are inconsistent

Mobile onboarding, auth, and most of the treatment are hard-coded in Spanish even though the app has locale settings. The signup phone example is Colombian (`+57`) rather than Ecuadorian (`+593`) (`app/auth.tsx:382-401`). Treatment error copy alone branches on English in places, leaving a mixed-language surface.

**Recommendation:** move all onboarding copy into the shared locale catalog and use region-neutral formatting or an Ecuador-appropriate example for the current market.

### P2 — Remaining accessibility issues are small but concrete

- The treatment button's visible label is “Ver eventos,” while its accessible name is “Explorar más” (`NewUserOnboardingGate.tsx:333-341`), so the visible label is not contained in the accessible name.
- Role choices are 40 px high (`app/auth.tsx:766-777`), below the app's 44 px mobile target standard.
- The treatment's 11 px preview label uses `#64748b` on `#f1f5f9`, approximately 4.34:1 contrast (`NewUserOnboardingGate.tsx:377-390`), below 4.5:1 for normal text.
- Signup and treatment titles are visually headings but do not expose a heading role.
- The treatment uses an absolute footer inside a Safe Area that applies only the top edge, so bottom-inset behavior needs physical-device verification.

## What is already working

- The pre-auth screen now uses a light status bar on its dark surface and respects Reduce Motion (`app/onboarding.tsx:46-80`). These issues from the 2026-08-05 audit are fixed.
- Primary and secondary onboarding actions meet the current 44/48 px touch-target standard and have usable names.
- Mobile auth uses Safe Area, keyboard avoidance, appropriate email/phone keyboards, autocomplete hints, focus chaining, explicit field labels, live error announcements, and disabled/busy states.
- `returnTo` rejects external and protocol-relative values (`app/auth.tsx:42-49`).
- Web exposes password visibility, Google signup/login, and account-free exploration.
- The backend correctly prevents public callers from self-assigning privileged security roles. The UX should build on that boundary, not weaken it.

## Recommended replacement journey

### 1. Entry

Show acquisition-specific context when available. A fan campaign should say “Sigue a [artist]”; an artist campaign should say “Crea o reclama tu perfil”; an internship route should say “Inicia tu postulación.” Generic app launches can use the short TDF value proposition.

### 2. Account

Ask only for the minimum required to create the governed account. Prefer Google/Apple and email options consistently in both login and signup. Defer phone and nonessential profile fields. Show password requirements that exactly match server validation and add a show/hide control on mobile.

### 3. Goal

Ask “¿Qué quieres hacer primero?” after or alongside signup. Store this as onboarding intent, never as a security role:

- Discover events
- Follow artists
- Create or claim an artist profile
- Apply for internships/classes
- Explore professional tools

Each choice must have a real side effect and an authorized destination.

### 4. First value

Route to one deterministic task with a visible success condition. Do not block the app shell indefinitely. Offer “Ahora no” and persist completion so the prompt does not return every launch.

### 5. Progressive profile

Request phone, biography, links, vCard details, notification permission, and access requests only when the user reaches a feature that benefits from them.

## Delivery plan

### First 48 hours

1. Pause `single-feature-onboarding-v1` treatment.
2. Remove the mobile role selector or convert it to a persisted non-security intent with a tested destination.
3. Replace or repair web links and quick routes that promise Fan/Intern/Artist outcomes.
4. Resolve redirects against the returned session before navigating.
5. Add onboarding view, start, completion, and failure events on both platforms.

### Week 1

1. Reduce the mobile first screen to one value proposition and two account actions.
2. Align web/mobile signup fields, password guidance, Google/Apple options, terms/privacy links, and error localization.
3. Build the deterministic first-value router and distinct loading/offline/error/empty states.
4. Add regression tests for campaign intent, safe accessible destinations, and intent persistence.

### Week 2

1. Rebuild the experiment cohort from account creation time and identity.
2. Add one-shot exposure/completion plus a real 24-hour window.
3. Relaunch only after control and treatment dashboards pass event-completeness checks.
4. Validate on small iPhone, modern iPhone, Android, 200% text, VoiceOver, and TalkBack.

## Acceptance criteria

- Every displayed intent changes either the next task or the stored onboarding preference.
- No acquisition link lands a newly created account on a destination it cannot use.
- A treatment is shown at most once per account, within its defined eligibility window.
- First value remains achievable when event moments are empty or the network fails.
- All visible button labels are contained in their accessible names.
- Interactive targets are at least 44 × 44 px under the app's mobile standard.
- Normal text reaches at least 4.5:1 contrast.
- Spanish and English journeys do not mix languages unintentionally.
- Funnel events are at least 95% complete from onboarding view through first value in test telemetry.
- No event properties contain names, email addresses, phone numbers, passwords, or free-form field content.

## Verification and limitations

- Mobile focused validation passes: 8 suites, 32 tests covering auth, onboarding and enlarged-content reachability, the real artist/event first-value path, paused experiment assignment, control/treatment exposure, loading/offline/error/empty/success gate states, intent, first-run eligibility, and the exact password policy. Targeted onboarding lint also passes with zero warnings.
- Web focused validation passes: routing, first-value persistence, registry, access request, campaign route, internship, and release-feed suites; web TypeScript and targeted lint pass.
- Backend auth compiles and the focused terms/Google-provisioning tests pass. Both generated TypeScript clients contain versioned consent and `accountCreated` fields.
- The locked mobile install restores the declared `expo-notifications` and `expo-haptics` runtime modules. Expo config evaluation and a clean static web export of all 50 routes pass. Mobile TypeScript now reports only eight pre-existing typed-route diagnostics outside the onboarding implementation and tests.
- Device-emulated rendering of the compiled export passes at exact 320 × 568 and 390 × 844 CSS viewports with no horizontal overflow. A Spanish 200% text-size stress test keeps the top controls visible and the full 1,212 px journey vertically reachable.
- Expo Doctor passes 14 of 18 checks. Its four project-wide findings are duplicate dependency versions, native/app-config synchronization risk, `react-native-webrtc` New Architecture compatibility, and four Expo patch-version mismatches; these are not onboarding regressions. The locked dependency tree also reports 39 existing npm audit findings (4 moderate, 35 high), which require a separately scoped dependency upgrade.
- The repository feature audit still reports eight pre-existing missing mobile directory destinations. Its mobile registry test consequently reports seven missing destination entries (the audit counts the separate quick-create path as the eighth issue).
- Final native sign-off still requires a small iPhone, a modern iPhone, Android, VoiceOver, and TalkBack. The local Android AVD could not start because the host had only about 1 GB free when checked; no user build artifacts were deleted to make room.
