# TDF Records Platform — Comprehensive UX/UI Audit and Improvement Plan

**Audit date:** 2026-08-05
**Scope:** `tdf-hq-ui/` web application and initialized `tdf-mobile/` Expo submodule
**Baseline:** current working tree, including existing uncommitted work; no product code was changed by this audit

## Audit method and confidence

This report combines:

- static review of the theme, routing, shared components, representative public/admin pages, mobile navigation, forms, query configuration, analytics, assets, and tests;
- a fresh production build of `tdf-hq-ui`;
- a Playwright inspection of `/tdf` at a 390 × 844 viewport, including the accessibility tree, computed layout, resource timing, and console output;
- review of the repository's mobile simulator/e2e screenshots and visual documentation;
- exact contrast calculations using the colors implemented in the source.

The runtime numbers below are diagnostic measurements from a local preview, not production p75 Core Web Vitals. The local API was unavailable during the browser pass, so authenticated journeys and data-backed content states were not exercised. A release claim of “WCAG 2.1 AA compliant” still requires automated scans plus keyboard, VoiceOver, TalkBack, zoom/reflow, and real-device testing.

The repository was already dirty and the mobile submodule already had local changes. The audit preserved them. The requested `UI_VISUAL_GUIDE.md`, `BEST_PRACTICES.md`, `specs.yaml`, and root brand SVGs were present; the paths differ from the `/workspace/...` examples in the brief.

---

# Section 1: Structured Executive Report

## Executive assessment

TDF has a stronger engineering UX foundation than its current presentation suggests. The web app already centralizes MUI theming, uses route-level lazy imports, has shared loading/error/empty states, employs React Query, includes strict `jsx-a11y`, and has broad component tests. Mobile has a focused four-tab new-user surface, Safe Area handling, debounced discovery search, virtualized lists in key screens, and unusually thoughtful ticket-checkout accessibility and funnel events.

The platform is not yet ready to claim WCAG 2.1 AA or polished cross-platform parity. Five issues dominate the risk:

1. The web production build defeats much of its intended code splitting: `dist/index.html` preloads 38 JavaScript chunks totaling **2,270,516 raw bytes / approximately 688,735 gzip bytes**, including FullCalendar and admin pages on the public `/tdf` route. The likely source is the page-based `manualChunks` strategy in `tdf-hq-ui/vite.config.ts:66-120`, which creates circular cross-chunk dependencies.
2. The default palette fails text contrast for white text on both brand colors: `#8b5cf6` is **4.23:1** and `#f43f5e` is **3.67:1** against white. Both are configured with white `contrastText` in `tdf-hq-ui/src/theme/AppThemeProvider.tsx:37-50`.
3. The public web shell exposes no `main`, `nav`, `header`, or `footer` landmark and has no skip link. At 390 px, `/tdf` had 46 focusable elements and a roughly 6,193 px document, making bypass navigation material rather than theoretical.
4. Mobile has design tokens but almost no design-system adoption: only `app/(tabs)/create.tsx` imports them, while the audited mobile source contains **873 hard-coded hex-color occurrences**. There is no color-scheme provider, yet `app/_layout.tsx:9` permanently uses a dark-content status bar—including on the dark onboarding screen.
5. Conversion measurement is incomplete. Web PostHog captures page views/page leaves (`tdf-hq-ui/src/analytics/posthog.ts:64-70`) and mobile ticket purchase has useful events, but there is no Web Vitals collection, route-level CTA funnel, automatic native screen tracking, or shared bounce/activation dashboard. The redesign cannot honestly be called data-backed until this baseline exists.

The investor narrative should be: **the scalable platform and product breadth already exist; the eight-week redesign consolidates them into a measurable, accessible, conversion-oriented experience without a framework rewrite.**

## Quantitative baseline

| Measure | Observed baseline | Interpretation / target |
|---|---:|---|
| `/tdf` local-preview resource transfer | ~2.26 MB across 56 resources | Diagnostic only; reduce first-view transfer below 1 MB on mobile where practical |
| `/tdf` local-preview `load` | ~3.3 s, unthrottled | Not a production Web Vital; target production p75 LCP <2.5 s |
| Hero video transfer | ~1.12 MB | Do not autoplay/download for reduced-motion, Save-Data, or narrow-mobile users |
| Initial module preloads | 38 chunks; ~689 KB gzip | Target only true shell/runtime dependencies and <350 KB gzip initial JS |
| Largest JS chunks | MUI 153 KB gzip; vendor 103 KB; FullCalendar 69 KB | FullCalendar/admin code must not preload on public acquisition pages |
| Public mobile page height | ~6,193 px at 390 px | Shorten acquisition story; keep one primary CTA visible early and repeat intentionally |
| Brand contrast | white/primary 4.23:1; white/secondary 3.67:1 | Minimum 4.5:1 for normal text; use darker action shades |
| Mobile color literals | 873; one token-consuming screen | Migrate all new/shared surfaces to semantic tokens; drive legacy count down by phase |
| Automated tests | 119 web; 36 mobile; 27 tests contain accessibility/focus assertions | Strong base; add axe, visual regression, and real assistive-technology scripts |
| Current validation | Web build/typecheck pass; mobile lint/typecheck pass; web lint fails | Clear the 6 web lint errors and triage 76 warnings before a redesign branch is release-ready |

## Current state — web

### Strengths to preserve

- `AppThemeProvider.tsx:26-193` is a usable central theme boundary with persisted choice, OS preference on first load, semantic MUI palette keys, typography, radii, and component overrides.
- All public and protected page components are declared with `lazyWithReload` in `src/routes/publicRoutes.tsx:6-32` and `protectedRoutes.tsx:6-75`. The architectural intent is correct even though the current Rollup chunk graph weakens the result.
- Shared status patterns are good: `RouteLoadingFallback.tsx:3-24`, `PageShell.tsx:64-84`, `SkeletonCards` at `PageShell.tsx:201-219`, `ApiErrorNotice`, and `ApiActivityIndicator` expose `role=status`, `aria-live`, and `aria-busy` appropriately.
- Navigation has strong details worth retaining: `aria-current`, searchable grouped navigation, `Ctrl/Cmd+K`, arrow-key command-palette navigation, labeled icon buttons, and Escape handling.
- `DataTable.tsx:49-177`, `PageShell`, and `EmptyState` provide a coherent component vocabulary. React Query has sensible retry handling and a two-minute web stale time in `src/main.tsx:15-28`.
- Brand rendering is centralized through `BrandLogo.tsx:23-40`; the root asset set includes black, white, alternate, wordmark, and isotype SVG variants.
- The current `/tdf` mobile layout has no horizontal overflow at 390 px and its dark art direction has high text contrast. The implementation uses responsive MUI grids and a clear H1.

### Critical/high web issues

- **Critical — ineffective initial-load splitting.** The fresh build inserts 38 module-preload links into `dist/index.html`, including `fullcalendar`, `mui-x`, `CourseRegistrationsAdminPage`, `BookingsPage`, `CmsAdminPage`, and other route code. This is the most direct code-backed bounce-rate risk.
- **Critical — known contrast failures.** The configured `contrastText` values are invalid for normal-size button text. Darker existing shades (`#7c3aed` at 5.70:1 and `#e11d48` at 4.70:1 against white) are viable action colors.
- **Critical — bypass/landmark failure on public pages.** `PublicBranding.tsx:105-210` uses generic boxes for header/content and `:211-340` for the footer; `index.html:12-14` has no skip target. This affects WCAG 1.3.1 and 2.4.1.
- **Critical — auto-playing motion has no pause/reduction policy.** `TdfPlatformPage.tsx:659-675` loops an autoplay video with no visible pause control; this is a WCAG 2.2.2 concern. No `prefers-reduced-motion` handling exists in either application.
- **High — mobile drawer is visually modal but not semantically modal.** `AppShell.tsx:118-130` creates a click-only backdrop while `SidebarNav.tsx:295-318` is a fixed `aside`, not a focus-trapped MUI `Drawer`. Focus is not moved into the menu or restored to the toggle.
- **High — dark mode follows the OS only once.** `AppThemeProvider.tsx:13-32` reads `matchMedia` initially and immediately persists a resolved light/dark value. There is no media-query listener or “system” state, and runtime inspection showed the root CSS `color-scheme` remains `normal`. The only theme toggle is on the login card (`LoginPage.tsx:862-870`), not in authenticated preferences.
- **High — dark parity is not enforced for complex surfaces.** FullCalendar event content hard-codes dark text in `BookingsPage.tsx:947-981`; no FullCalendar, dialog/backdrop, or semantic focus overrides exist in the theme.
- **High — acquisition page is long and message-dense.** `/tdf` is roughly 6.2k px tall at 390 px, repeats several persona propositions, and exposes three competing hero CTAs. It needs a shorter, testable path per acquisition intent.
- **High — the fixed radio competes with conversion.** `App.tsx:19` mounts `RadioWidget` globally. On `/tdf`, its fixed dock sits over the bottom viewport (`RadioWidget.tsx:1825-1860`) and the browser measured six 30 × 30 controls. `/tdf` is not in the hide/on-demand list in `radioRouteVisibility.ts:1-23`.
- **High — language and tone are inconsistent.** Browser locale produced English CTAs inside largely hard-coded Spanish content; the document remained `lang=es`. `i18n/index.ts:19-57` does not update the document synchronously before the initial render, and most `TdfPlatformPage` body copy is not localized. This affects WCAG 3.1.2 and brand trust.
- **Medium — headings communicate style instead of hierarchy.** The `/tdf` H1 is followed by a long H5 subtitle (`TdfPlatformPage.tsx:687-703`) and later H3/H5/H6 sections without a consistent outline.
- **Medium — mobile table behavior is only overflow.** `DataTable.tsx:71-134` relies on `TableContainer`; the visual guide explicitly accepts horizontal scrolling (`UI_VISUAL_GUIDE.md:186-208`). High-frequency admin workflows need a card/priority-column alternative at narrow widths.
- **Medium — action rows can overflow.** `PageShell.tsx:98` renders actions in a single unwrapped row even when the title stack changes to a column.
- **Medium — Inter is declared but not shipped.** The theme declares Inter at `AppThemeProvider.tsx:52-67`, but no font assets, `@font-face`, or font request were found. Rendering therefore varies by device.
- **Medium — image loading is inconsistent.** Several public images lack `loading=lazy`, explicit intrinsic dimensions, or responsive `srcset`; the hero video has no poster/source variants.
- **Medium — the current web lint gate is red.** The audit observed 6 errors and 76 warnings. Errors are concentrated in new environment access in `CurrencyContext`, `LocalePreferencesContext`, `i18n/index.ts`, and `DomoVenuePage`; warnings include missing hook dependencies.

## Current state — mobile

### Strengths to preserve

- Expo Router cleanly separates root stack and tabs (`app/_layout.tsx` and `app/(tabs)/_layout.tsx`). The visible new-user surface is intentionally limited to Eventos, Seguir, vCard, and Perfil (`src/navigation/mobileSurface.ts:1-17`), reducing navigation overload.
- `AppProviders.tsx:17-43` correctly centralizes Safe Area, query, authentication, analytics, user settings, first-run, experiments, and optional Stripe providers.
- Auth uses `KeyboardAvoidingView`, safe-area handling, correct email keyboard/autocomplete, password content types, submission loading, and alert semantics in `app/auth.tsx:225-447`.
- Event discovery uses debounced search, `FlatList`, explicit accessibility roles/states, and useful empty/error paths in `app/(tabs)/events.tsx`.
- The ticket funnel is the accessibility high-water mark: radio-group semantics, labeled quantity controls, field alerts, loading labels, and analytics events from checkout start through success/failure.
- Key list screens already use `FlatList`; bookings and parties configure render windows (`initialNumToRender`, `windowSize`, `removeClippedSubviews`).
- Mobile lint and TypeScript pass, and 36 tests give the redesign a regression base.

### Critical/high mobile issues

- **Critical — there is no functional mobile theme.** `src/theme/designTokens.ts:8-25` contains light/dark values, but only one screen imports the token module. The application has no `Appearance`/`useColorScheme` provider and 873 hard-coded hex occurrences.
- **Critical — modal accessibility is incomplete.** Event composer and invite modals (`eventDetail.tsx:1189-1337`) have neither `onRequestClose` nor `accessibilityViewIsModal`; the city modal has `onRequestClose` but no modal accessibility grouping (`events.tsx:437-521`). Background content/focus containment and Android Back behavior are inconsistent.
- **High — touch targets are not governed.** A static scan found 160 Pressable/Touchable instances, only 3 `hitSlop` uses, and 15 explicit 44 px dimensions. Concrete failures include unlabeled month arrows at `CalendarScreen.tsx:80-88` and the 40 px onboarding icon/compact skip affordance. The 44 × 44 target is an internal mobile-platform usability standard; it is not itself a WCAG 2.1 AA requirement (WCAG 2.1 target size 2.5.5 is AAA), but it should remain a release criterion.
- **High — many controls lack native names/states.** `ArtistCard.tsx:23-46` has no button role/name; calendar arrows have no labels; numerous legacy buttons rely on descendant text and omit disabled/selected state. Mobile ESLint has no React Native accessibility plugin (`tdf-mobile/eslint.config.js:42-65`), so the green lint result does not cover these defects.
- **High — labels are visually adjacent, not programmatically associated.** Auth and edit/profile forms frequently render `<Text>` followed by `<TextInput>` without `accessibilityLabel`/`accessibilityHint`. `editArtistProfile.tsx:129-161` also omits URL keyboard/autocapitalization settings.
- **High — status-bar contrast is wrong on dark surfaces.** `app/_layout.tsx:9` uses `style=dark`, while onboarding uses `#0b1220` (`onboarding.tsx:130-145`).
- **High — motion preferences are ignored.** Onboarding always runs a 500 ms translate/fade (`onboarding.tsx:44-49`). No `AccessibilityInfo.isReduceMotionEnabled` or Reanimated reduced-motion policy exists.
- **High — localization is visibly mixed.** The invite modal switches to “Close” and “Invite Friends” (`eventDetail.tsx:1255-1263`), while surrounding UI is Spanish; edit-artist failure/form copy is largely English (`editArtistProfile.tsx:88-190`).
- **Medium — React Query is not wired to native lifecycle/connectivity.** `src/lib/queryClient.ts:3-7` sets cache defaults, but there is no `focusManager`/`onlineManager` integration with `AppState` and NetInfo and no persisted cache. This can show stale content after app resume and waste retries offline.
- **Medium — remote images use core `Image` without a shared cache/placeholder policy.** Event, artist, moment, and inventory surfaces have no consistent progressive image handling.
- **Medium — large screens are monolithic.** `eventDetail.tsx` is 1,888 lines and `ticketCheckout.tsx` is 1,164 lines. This raises re-render, ownership, and accessibility-regression risk even when bundling is acceptable.
- **Low — available screenshot evidence is not investor-ready.** The repository mostly contains auth/debug artifacts; one reviewed screenshot visibly includes “Open debugger to view warnings,” and another is effectively black. A deliberate cross-platform evidence set is needed.

## WCAG 2.1 AA alignment snapshot

| Criterion | Status | Evidence / required action |
|---|---|---|
| 1.3.1 Info and Relationships | **Fail/partial** | Public shell lacks landmarks; mobile labels/modals are not consistently programmatic |
| 1.4.3 Contrast (Minimum) | **Fail** | White on primary 4.23:1; white on secondary 3.67:1 |
| 1.4.10 Reflow | **Partial** | `/tdf` has no horizontal overflow at 390 px; dense tables still depend on horizontal scrolling |
| 1.4.11 Non-text Contrast | **At risk** | Theme dividers/input outlines use 0.04–0.20 alpha and need 3:1 checks where they convey boundaries/state |
| 2.1.1 Keyboard | **Partial** | MUI basics and quick nav are strong; custom drawer/backdrop and bespoke controls need end-to-end testing |
| 2.2.2 Pause, Stop, Hide | **Fail** | Looping autoplay hero video has no pause/hide or reduced-motion branch |
| 2.4.1 Bypass Blocks | **Fail** | No skip link; public page has no main landmark |
| 2.4.3 Focus Order | **At risk** | Web mobile drawer and native modals lack explicit focus containment/restoration |
| 2.4.6 Headings and Labels | **Partial** | Labels usually exist, but `/tdf` heading hierarchy is stylistic and several native controls are unnamed |
| 2.4.7 Focus Visible | **Partial** | MUI supplies focus behavior and a few custom components add it; no global token/test coverage |
| 3.1.1/3.1.2 Language | **Fail/partial** | `lang=es` remains while English CTA fragments render; mobile mixes English/Spanish |
| 3.3.1/3.3.3 Errors and Suggestions | **Partial** | Strong MUI/ticket examples; native form associations and live announcements are inconsistent |
| 4.1.2 Name, Role, Value | **Fail/partial** | Numerous mobile pressables and modals lack explicit role/name/state |

## Business impact and investor framing

- **Bounce and acquisition:** unnecessary initial JS, auto-downloaded media, a long multi-persona landing story, and competing radio controls increase time and cognitive cost before the first meaningful action.
- **Activation:** mixed-language CTAs and three simultaneous hero choices make intent less clear. A route-aware hero should select one primary action based on campaign/source and expose secondary roles below.
- **Retention:** a consistent mobile theme, lifecycle-aware cache, accessible modals, and predictable loading/error states reduce frustration in repeat discovery and ticket workflows.
- **Operational scalability:** semantic tokens, smaller feature modules, shared form/control primitives, and automated WCAG/visual gates reduce the cost of each new TDF surface.
- **Investor confidence:** show the current technical assets—role-aware platform, commerce, event discovery, React Query, analytics, tests—alongside a baseline-to-target scorecard. Do not claim AA compliance or bounce improvement until independently measured.

---

# Section 2: Quick Wins

- **Darken contained action colors** — change interactive primary/secondary shades or `contrastText` in `tdf-hq-ui/src/theme/AppThemeProvider.tsx:37-50`; use the already defined `#7c3aed` and `#e11d48` where white text is required. **Effort:** 1–2 h. **Expected impact:** remove two known WCAG 1.4.3 failures across shared buttons.

- **Add an explicit skip link and semantic public shell** — render `header`, `nav`, `main id=main-content`, and `footer` in `tdf-hq-ui/src/components/PublicBranding.tsx:105-340`; add the same skip target to `AppShell.tsx:95-146`. **Effort:** 3–4 h. **Expected impact:** WCAG 2.4.1 pass on shared shells; keyboard users bypass dozens of controls.

- **Stop preloading route chunks** — replace the page-name `manualChunks` rule in `tdf-hq-ui/vite.config.ts:66-120` with stable vendor groups and let dynamic route boundaries own page code; assert allowed preload names in a build test. **Effort:** 4–6 h. **Expected impact:** remove up to the observed 38 route preloads / ~689 KB gzip from first navigation.

- **Make hero media adaptive** — add a poster, `preload=none`, pause control, `prefers-reduced-motion`, Save-Data, and narrow-viewport fallback around `TdfPlatformPage.tsx:659-675`. **Effort:** 3–5 h. **Expected impact:** avoid the observed ~1.12 MB transfer for motion-sensitive/data-constrained visitors and improve LCP opportunity.

- **Remove the radio from acquisition routes by default** — add `/tdf` and other landing routes to `RADIO_ON_DEMAND_PATH_PREFIXES` in `src/utils/radioRouteVisibility.ts:12-23`; make dock controls at least 44 × 44 in `RadioWidget.tsx:1871-1937`. **Effort:** 2–3 h. **Expected impact:** eliminate six 30 px targets and reclaim the mobile viewport for the primary CTA.

- **Respect OS theme changes** — store `light | dark | system`, subscribe to `matchMedia`, set root `color-scheme`, and expose the selector in `SystemPage.tsx:110-143`, not only `LoginPage.tsx:862-870`. **Effort:** 3–5 h. **Expected impact:** correct persistence/system parity and native control rendering.

- **Fix FullCalendar dark text** — replace `#0f172a` literals in `BookingsPage.tsx:947-981` with theme-derived accessible colors and add dark-mode screenshot coverage. **Effort:** 2 h. **Expected impact:** remove a known dark-mode parity hotspot in a core workflow.

- **Make the public page language coherent** — localize the hard-coded `/tdf` body, set `documentElement.lang` immediately during i18n initialization (`src/i18n/index.ts:19-57`), and mark intentional foreign-language fragments. **Effort:** 4–6 h. **Expected impact:** WCAG 3.1.2 improvement and lower trust friction from mixed CTA language.

- **Fix mobile status bar and introduce a minimal theme context** — derive `StatusBar` style from `useColorScheme` in `tdf-mobile/app/_layout.tsx:6-16`; expose semantic background/surface/text/action tokens. **Effort:** 4–6 h. **Expected impact:** immediate legibility on onboarding and a migration path away from 873 literals.

- **Repair obvious mobile control semantics** — give month arrows 44 px hit areas, roles, and labels in `src/screens/CalendarScreen.tsx:80-88`; add role/label to `ArtistCard.tsx:23-46`. **Effort:** 1–2 h. **Expected impact:** remove repeatable screen-reader/tap failures in shared discovery components.

- **Harden native modals** — add `onRequestClose`, `accessibilityViewIsModal`, labeled close actions, and focus restoration to `eventDetail.tsx:1189-1337`, `createEvent.tsx:452-520`, and inventory modals. **Effort:** 4–6 h. **Expected impact:** consistent Android Back and screen-reader modal behavior.

- **Optimize input keyboards/labels** — add `type=tel`/`autoComplete=tel` to web signup phone at `LoginPage.tsx:1342-1348`; add native accessibility labels, URL keyboards, `autoCapitalize=none`, return keys, and focus chaining to forms such as `editArtistProfile.tsx:129-161`. **Effort:** 3–4 h. **Expected impact:** fewer mobile-entry errors and faster completion.

- **Add performance and conversion telemetry** — collect LCP/INP/CLS and hero/CTA events beside `tdf-hq-ui/src/analytics/posthog.ts:64-102`; add automatic native route screen events in `tdf-mobile/src/analytics/AnalyticsProvider.tsx:16-30`. **Effort:** 4–6 h. **Expected impact:** establish route/device/source bounce and activation baselines within one release.

- **Make accessibility lint meaningful on native** — add a React Native accessibility ESLint plugin/rules to `tdf-mobile/eslint.config.js:42-65`; add axe to representative web tests. **Effort:** 3–4 h. **Expected impact:** prevent new unnamed-control regressions in CI.

- **Clear the web quality gate** — fix the 6 current ESLint errors and triage the 76 warnings, especially environment access and hook dependencies. **Effort:** 2–4 h. **Expected impact:** restore investor/release confidence and make subsequent redesign failures attributable.

---

# Section 3: Step-by-Step Implementation Plan

## Phase 1 (Weeks 1–2): Critical accessibility and performance

### Tasks and acceptance criteria

1. **Create the measurement baseline.** Add Web Vitals and shared event names for landing viewed, primary CTA clicked, signup started/completed, event viewed, checkout started/completed, and error abandoned. Segment by route, campaign, device class, locale, and theme.
   **Accept:** PostHog dashboards show a full acquisition/activation funnel without collecting sensitive form content.
2. **Repair initial chunking.** Remove route-page module preloads and add a build manifest budget test.
   **Accept:** `/tdf` HTML preloads only the shell/runtime it uses; initial JS is <350 KB gzip; FullCalendar, MUI X, DnD, QR, and admin pages load only on demand.
3. **Optimize above-the-fold media.** Provide AVIF/WebP poster, responsive sources, conditional video, lazy below-fold images, and a visible pause control.
   **Accept:** reduced-motion and Save-Data users receive no autoplay video; hero still communicates the brand without media.
4. **Implement shared semantics.** Add skip links, landmarks, coherent headings, focus-visible token, drawer focus containment/return, and semantic public navigation/footer.
   **Accept:** keyboard-only users can reach and leave every shell region, drawer, menu, and dialog in a predictable order.
5. **Resolve color/non-text contrast.** Replace invalid contained-button colors and audit input borders, statuses, chips, disabled states, calendar content, overlays, and both themes.
   **Accept:** text is >=4.5:1 normal / >=3:1 large; meaningful UI boundaries and focus indicators are >=3:1.
6. **Automate the baseline gate.** Add axe scans for login, `/tdf`, booking, marketplace, shell, and one admin table; add a keyboard Playwright smoke. Clear current lint failures.
   **Accept:** zero critical/serious axe violations, zero lint errors, and documented exceptions only for third-party widgets.

### Resources and dependencies

- One senior web engineer, one product designer/content designer, and accessibility QA at least half-time.
- PostHog project access and a privacy-reviewed taxonomy.
- Production-like public API fixture/mock so performance and accessibility runs are deterministic.
- Optimized brand media/poster assets.

### Validation

- Lighthouse/WebPageTest on throttled mobile, production RUM p75, bundle-manifest diff, axe, keyboard-only Playwright, 200% browser zoom, and manual NVDA/VoiceOver smoke.
- Test 320, 390, 430, 768, 1024, and 1440 px widths in light/dark and Spanish/English.

### Success metrics

- Production p75 LCP <2.5 s, INP <200 ms, CLS <0.10.
- <350 KB gzip initial JS on public acquisition routes; no unrelated route preloads.
- Zero known WCAG A/AA critical/serious violations in the audited shell routes.
- Funnel baseline has >=95% event completeness for pageview → CTA → signup.

## Phase 2 (Weeks 3–4): Mobile responsiveness and interaction

### Tasks and acceptance criteria

1. Build shared native `Screen`, `Card`, `Button/IconButton`, `Field`, `EmptyState`, `LoadingState`, and `ModalSheet` primitives with semantic tokens and 44 px minimum targets.
   **Accept:** new primitives pass VoiceOver/TalkBack names, roles, states, and large-text tests.
2. Migrate the four visible tabs and the ticket/event-detail path first; split `eventDetail` and `ticketCheckout` into focused feature components.
   **Accept:** no clipped actions at smallest supported width or 200% font scaling; modal Back/close/focus behavior is consistent.
3. Optimize forms for mobile keyboard and error recovery; scroll/focus the first invalid field and preserve draft state.
   **Accept:** email, phone, URL, numeric, and date inputs open correct keyboards and expose associated instructions/errors.
4. Wire React Query to native `AppState`/connectivity, cancel stale requests, and define image cache/placeholder behavior.
   **Accept:** resume/offline behavior is deterministic and does not show indefinite spinners.
5. Improve narrow web admin patterns: convert high-use tables to priority cards or responsive columns and make action groups wrap.
   **Accept:** core CRM, schedule, inventory, and role workflows complete at 320 px without page-level horizontal scroll.

### Resources and dependencies

- One React Native engineer, one web engineer for shared responsive tables, designer, QA with iOS and Android hardware.
- NetInfo or equivalent connectivity dependency; decision on `expo-image`/image caching.
- A supported-device matrix and minimum OS policy.

### Validation

- VoiceOver and TalkBack task scripts; iOS Dynamic Type and Android font scale 2.0; portrait/landscape; keyboard open/closed; Detox smoke plus React Native Testing Library assertions.
- Measure tap error/retry rate and task completion time for city setup, event save, ticket quantity, login, and profile edit.

### Success metrics

- 100% of interactive controls in migrated flows have a >=44 × 44 target and programmatic name/role/state.
- Zero clipped/overlapping primary actions in the supported device matrix.
- >=20% reduction in validation retries or median form completion time after baseline, treated as a target rather than a guarantee.

## Phase 3 (Weeks 5–6): Dark mode and brand consistency

### Tasks and acceptance criteria

1. Implement `light | dark | system` on web and mobile with live OS updates and persistence.
   **Accept:** changing system theme updates both apps when in system mode; status/navigation bars match every surface.
2. Convert tokens to semantic roles (`canvas`, `surface`, `surfaceRaised`, `textPrimary`, `textMuted`, `border`, `focus`, `actionPrimary`, `danger`, etc.) and migrate shared/high-traffic components.
   **Accept:** no direct palette literals in shared components; dark/light screenshots have feature parity.
3. Audit FullCalendar, QR/canvas output, native modals, alerts, disabled states, scrims, images, and external widgets.
   **Accept:** every state passes contrast and remains understandable without color alone.
4. Ship Inter correctly or deliberately standardize on system fonts. Consolidate `BrandLogo` variants and asset ownership.
   **Accept:** no font-layout swap regression; approved logo variant is documented per background/context.
5. Localize all acquisition/core-flow copy and define tone rules for success, error, empty, permission, and destructive messages.
   **Accept:** no mixed-language UI in Spanish/English journeys; language metadata updates correctly.
6. Add a shared reduced-motion policy.
   **Accept:** hover lifts, card transforms, onboarding animation, carousels, and hero media respect user preference.

### Resources and dependencies

- Design-system owner, brand/content designer, one web and one mobile engineer, bilingual copy review.
- Decision on licensed/self-hosted font files and canonical logo source.

### Validation

- Token contrast unit tests, light/dark visual regression at key widths, system-theme switching, screen-reader state review, forced-colors/high-contrast smoke, and reduced-motion snapshots.

### Success metrics

- 100% parity for critical journeys in light/dark/system.
- Zero contrast failures in token tests and no unapproved hard-coded colors in shared components.
- Mobile hard-coded color count reduced by at least 70% from the 873-occurrence baseline.

## Phase 4 (Weeks 7–8): Investor-ready experience and documentation

### Tasks and acceptance criteria

1. Redesign `/tdf` into an intent-led acquisition page with one campaign-aware primary CTA, concise proof, product breadth below the fold, and an accessible media fallback.
   **Accept:** the first viewport states audience, value, proof, and one next step; secondary personas remain discoverable.
2. Produce a controlled evidence pack: before/after mobile and desktop captures, light/dark, Spanish/English, VoiceOver/TalkBack clips, bundle/Web Vitals chart, and funnel dashboard.
   **Accept:** every screenshot is from a release build with no debug overlays or private data.
3. Replace outdated UX documentation. Update `UI_VISUAL_GUIDE.md` (currently describing the old `#1976d2` palette and horizontal-scroll mobile tables) and add a versioned design-system/accessibility guide.
   **Accept:** documented tokens, breakpoints, states, WCAG checks, ownership, and review gates match code.
4. Run a moderated usability study with representative fan, artist, front-desk, and admin users, plus an independent accessibility review.
   **Accept:** top issues are severity-ranked, assigned, and retested before claiming compliance.
5. Publish the investor scorecard internally: product breadth, baseline, shipped improvements, remaining risk, next-quarter plan.
   **Accept:** every metric names its source/window and separates target from observed result.

### Resources and dependencies

- Product lead/researcher, investor-deck owner, designer, analytics owner, accessibility specialist, and 5–8 representative participants.
- Stable staging environment with seeded non-sensitive accounts and production-like CDN/API behavior.

### Validation

- Moderated task testing; independent WCAG audit; production RUM/funnel comparison for at least two statistically useful release windows; visual regression sign-off.

### Success metrics

- Target 15–25% relative bounce-rate reduction on acquisition routes after controlling for source/device; report confidence and sample size.
- Target >=15% relative lift in signup-start rate and >=10% lift in checkout completion where baseline volume supports inference.
- WCAG A/AA audit has no open critical/high findings before external compliance language is used.
- Investor evidence pack contains reproducible before/after measures, not vanity screenshots alone.

---

# Section 4: Critique Table

| Problem | Impact | Solution | Priority | Effort | Component/File |
|---|---|---|---|---|---|
| Public build preloads 38 route/vendor chunks (~689 KB gzip) | Slower parsing/interaction and higher mobile bounce risk | Remove page-based manual chunk cycles; add preload/bundle budget assertion | Critical | M | `tdf-hq-ui/vite.config.ts:66-120` |
| Hero video autoloads ~1.12 MB and loops | Delays first view; consumes data; motion cannot be stopped | Poster + adaptive load + pause + reduced-motion/Save-Data fallback | Critical | M | `tdf-hq-ui/src/pages/TdfPlatformPage.tsx:659-675` |
| White on `#8b5cf6` is 4.23:1 | Normal button text fails WCAG 1.4.3 | Use darker action shade or dark contrast text; token-test it | Critical | S | `tdf-hq-ui/src/theme/AppThemeProvider.tsx:37-40` |
| White on `#f43f5e` is 3.67:1 | Secondary button text fails WCAG 1.4.3 | Use `#e11d48` or accessible contrast text | Critical | S | `tdf-hq-ui/src/theme/AppThemeProvider.tsx:39-40,97-99` |
| No public landmarks or skip link | Keyboard/screen-reader users traverse 46 focusables on a long page | Semantic header/nav/main/footer plus skip link | Critical | S | `tdf-hq-ui/src/components/PublicBranding.tsx:105-340` |
| Autoplay motion has no pause/reduced-motion path | WCAG 2.2.2 failure; vestibular/distraction risk | Pause control and motion preference policy | Critical | M | `TdfPlatformPage.tsx:659-675` |
| Mobile has tokens but no theme provider | No dark parity; 873 literals cause visual drift | Semantic ThemeProvider using Appearance/system mode | Critical | L | `tdf-mobile/src/theme/designTokens.ts`; `src/providers/AppProviders.tsx` |
| Native modals lack consistent modal semantics/Back handling | Screen-reader context leaks; Android users can be trapped | Shared accessible ModalSheet with close/focus contracts | Critical | M | `tdf-mobile/app/eventDetail.tsx:1189-1337` |
| Web mobile drawer lacks focus trap/restore | Keyboard focus remains behind visual overlay | Replace with MUI Drawer or implement dialog focus lifecycle | High | M | `tdf-hq-ui/src/routes/AppShell.tsx:118-131`; `SidebarNav.tsx:291-318` |
| System theme is resolved then permanently stored | App stops following later OS changes | Store explicit `system`; subscribe to `matchMedia` | High | S | `tdf-hq-ui/src/theme/AppThemeProvider.tsx:13-32` |
| Root CSS color scheme stays `normal` | Browser-native controls/scrollbars may mismatch dark theme | Enable/set CSS `color-scheme` with active mode | High | S | `AppThemeProvider.tsx:157-170` |
| Theme control exists only on login | Authenticated users cannot discover/change appearance | Add appearance control to Preferences and session menu | High | S | `LoginPage.tsx:862-870`; `SystemPage.tsx:110-143` |
| FullCalendar uses hard-coded dark event text | Dark-mode calendar content can become unreadable | Theme event CSS/content and snapshot both modes | High | S | `tdf-hq-ui/src/pages/BookingsPage.tsx:947-981` |
| Radio is fixed over `/tdf` with 30 px controls | Obscures content; fails TDF 44 px target; distracts conversion | Hide/on-demand on landing routes; enlarge controls/add bottom space | High | S | `RadioWidget.tsx:1825-1937`; `radioRouteVisibility.ts:1-23` |
| `/tdf` is ~6.2k px tall with three hero CTAs | Diluted value proposition and choice overload | Intent-based primary CTA; shorten/progressive disclosure; A/B test | High | M | `tdf-hq-ui/src/pages/TdfPlatformPage.tsx` |
| English CTAs render inside Spanish page; `lang=es` remains | Trust/cognitive friction; WCAG language-of-parts issue | Complete localization and set language synchronously | High | M | `TdfPlatformPage.tsx:628-703`; `src/i18n/index.ts:19-57` |
| No Web Vitals or shared activation funnel | Bounce causes cannot be separated from traffic quality | Add RUM and route/source/device funnel dashboards | High | M | `tdf-hq-ui/src/analytics/posthog.ts:64-102` |
| Mobile has no automatic screen analytics | Retention/navigation drop-off is opaque | Observe Expo Router segments and call `screen()` consistently | High | S | `tdf-mobile/src/analytics/AnalyticsProvider.tsx:16-30` |
| Mobile status bar always uses dark content | Low contrast on dark onboarding and future dark mode | Bind status-bar style to surface/theme | High | S | `tdf-mobile/app/_layout.tsx:6-16` |
| Month arrows have tiny unlabeled Pressables | Tap and screen-reader navigation failure | 44 px IconButton, role, label, hint | High | S | `tdf-mobile/src/screens/CalendarScreen.tsx:80-88` |
| Artist card lacks programmatic role/name | Screen reader does not announce it reliably as navigation | Add button/link role, label, hint, pressed/disabled state if relevant | High | S | `tdf-mobile/src/components/ArtistCard.tsx:23-46` |
| Visual labels are not associated with native fields | Errors/instructions may be missed by screen readers | Shared Field primitive with accessibilityLabel/hint/error announcement | High | M | `tdf-mobile/app/auth.tsx:269-364`; `editArtistProfile.tsx:129-161` |
| Onboarding ignores Reduce Motion | Unwanted translate/fade for motion-sensitive users | Query `AccessibilityInfo`; skip/shorten animation | High | S | `tdf-mobile/app/onboarding.tsx:40-49` |
| Native lint has no accessibility plugin | Passing CI misses unnamed Pressables/modals | Add RN a11y rules plus focused component tests | High | S | `tdf-mobile/eslint.config.js:42-65` |
| React Query is not connected to AppState/network | Stale resumes and wasted offline retries | Wire `focusManager`/`onlineManager`; define offline UX | Medium | M | `tdf-mobile/src/lib/queryClient.ts:1-7` |
| Remote mobile images have no shared cache/placeholder | Blank/janky cards and repeated transfer | Adopt cached image component with aspect-ratio placeholder | Medium | M | `tdf-mobile/src/components/EventCard.tsx`; `ArtistCard.tsx` |
| Web public images inconsistently lazy-load | Extra below-fold bandwidth affects LCP contention | Add lazy/async decode, dimensions, responsive sources | Medium | M | `TdfPlatformPage.tsx:545-556,923-935` and public pages |
| Shared table only offers horizontal overflow | Mobile admin comparison/actions become hard to scan | Responsive card/priority-column mode and sticky actions | Medium | M | `tdf-hq-ui/src/components/DataTable.tsx:70-134` |
| PageShell action row does not wrap | Actions can overflow at narrow widths/long locales | Wrap or stack actions at xs/sm with full-width primary | Medium | S | `tdf-hq-ui/src/components/PageShell.tsx:57-101` |
| Heading levels on `/tdf` are visually chosen | Screen-reader outline is harder to understand | H1 → H2 sections → H3 cards; style separately | Medium | S | `TdfPlatformPage.tsx:687-703` and section helpers |
| Inter is named but not loaded deterministically | Typography/line breaks vary across investor screenshots/devices | Self-host and preload subsets or adopt explicit system stack | Medium | M | `AppThemeProvider.tsx:52-67`; `tdf-mobile/src/theme/designTokens.ts:66-83` |
| Web lint currently fails (6 errors/76 warnings) | Red quality gate undermines release readiness | Fix env access; resolve hook deps; hold zero-error gate | Medium | S | `.eslintrc.cjs`; current i18n/context files |
| Visual guide uses old blue palette and endorses scroll tables | Design/documentation contradict production and overstates accessibility | Replace with versioned tokens, responsive patterns, verified states | Medium | M | `UI_VISUAL_GUIDE.md:186-208,349-464` |
| Brand SVGs are duplicated across root/public/src | Updates can drift and dark variants are applied with filters | Establish one canonical asset pipeline and usage matrix | Low | S | `public/assets/tdf-ui/`; `tdf-hq-ui/src/assets/`; `BrandLogo.tsx` |
| Error-boundary microcopy lacks Spanish accents | Small but visible quality/trust defect | Correct localized copy and route errors through i18n | Low | S | `tdf-hq-ui/src/routes/AppErrorBoundary.tsx:44-58` |
| Existing mobile screenshots include debug/blank artifacts | Weak investor evidence and unclear current UX state | Capture scripted release-build matrix with seeded safe data | Low | M | `tdf-mobile/artifacts/`; `tdf-mobile/e2e/screenshots/` |

## Recommended decision

Approve Phase 1 immediately and treat WCAG A/AA plus initial-load budgets as release gates, not backlog polish. Run the investor redesign in parallel only after the event taxonomy is live; otherwise the team will be unable to distinguish a more attractive interface from one that measurably improves acquisition and activation.
