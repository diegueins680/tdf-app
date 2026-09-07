# Product Analytics

TDF uses [PostHog](https://posthog.com) (EU cloud by default) as the single product-analytics + experiment readout tool. This doc covers the contract, the env vars, the events we emit, and how to add a new event.

## Contract at a glance

Both surfaces emit the **same event names** and the **same property shapes** so the dashboards work uniformly across mobile and web. The distinct id for a logged-in user is **`String(partyId)`** on both surfaces.

| Surface | Package | Init location | Identify location |
|---|---|---|---|
| Mobile (`tdf-mobile`) | `posthog-react-native` | `src/analytics/posthog.ts` (singleton) | `src/analytics/AnalyticsProvider.tsx` (observes `useAuth().partyId`) |
| Web (`tdf-hq-ui`) | `posthog-js` | `src/analytics/posthog.ts` (singleton, kicked off in `main.tsx`) | `src/session/SessionContext.tsx` (effect on `session.partyId`) |

Both surfaces fall back to a **no-op client** if no PostHog key is configured. Calls to `capture` / `identify` / `reset` are safe in dev, in preview builds, in tests.

## Environment variables

### Mobile

```env
EXPO_PUBLIC_POSTHOG_KEY=phc_xxx           # leave empty to disable analytics
EXPO_PUBLIC_POSTHOG_HOST=https://eu.i.posthog.com
```

### Web

```env
VITE_POSTHOG_KEY=phc_xxx                  # leave empty to disable analytics
VITE_POSTHOG_HOST=https://eu.i.posthog.com
```

Both keys are **client-side** PostHog project keys (start with `phc_`). They are public by design — PostHog rate-limits and segregates by project. Do **not** put server-side `phs_` keys in either of these vars; those belong on the backend if and when we add server-side event emission.

## Events we emit today

| Event | Surface | Emitter | Properties |
|---|---|---|---|
| `$identify` | mobile + web | on partyId becoming known | opaque `partyId` distinct id only; no person properties |
| `$screen` (mobile) / `$pageview` (web) | mobile + web | PostHog autocapture | route, screen name |
| `experiment_assigned` | mobile (web TODO when ExperimentProvider lands on web) | `ExperimentProvider` on first assignment | `experimentId`, `variant`, `source: 'client_local'` |
| `experiment_viewed`, `experiment_converted`, … | mobile + web | wherever `useExperimentEvent().track(...)` is called | `experimentId`, `variant`, free-form metadata |

### Community growth funnel (web)

The acquisition and activation surfaces now attach the latest explicit
UTM/referral attribution to every growth event. Direct navigation keeps the
previous attribution. Only an allowlist of campaign parameters is persisted;
arbitrary query parameters, emails, and phone numbers are ignored.

| Funnel stage | Events |
|---|---|
| Landing | `acquisition_landing_viewed`, `acquisition_cta_clicked` |
| Auth entry | `auth_page_viewed`, `login_submitted`, `login_completed`, `login_failed` |
| Signup | `signup_started`, `signup_roles_selected`, `signup_submitted`, `signup_completed`, `signup_failed`, `signup_abandoned` |
| Activation | `fan_profile_saved`, `artist_profile_saved`, `fan_role_enabled`, `artist_followed`, `artist_unfollowed` |

Stable attribution properties are `attribution_source`,
`attribution_medium`, `attribution_campaign`, `attribution_content`,
`attribution_term`, `referral_code`, `attribution_landing_path`, and
`attribution_captured_at`.

### "First meaningful action" events

These are the ones #128 will read. They are NOT emitted by this PR — they land with the implementation PR for #128. Calling them out so the shapes are agreed up front:

| Event | Properties |
|---|---|
| `rsvp_created` | `eventId`, `artistId`, `isFirstRsvp: boolean`, `experimentId`, `variant` |
| `rsvp_broadcast_emitted` | `eventId`, `artistId`, `fanCount`, `experimentId`, `variant` |

## Adding a new event

1. Pick a stable lowercase snake_case name, no PII in the event name itself.
2. **Mobile:**
   ```tsx
   import { useAnalytics } from '../analytics/AnalyticsProvider';
   const analytics = useAnalytics();
   analytics.capture('rsvp_created', { eventId, artistId });
   ```
3. **Web:**
   ```tsx
   import { useAnalytics } from '../analytics/useAnalytics';
   const analytics = useAnalytics();
   analytics.capture('rsvp_created', { eventId, artistId });
   ```
4. Document the event + property shape in the table above so the dashboards and future contributors stay in sync.

## What we deliberately do NOT do

- **No session recording.** Disabled at init on both surfaces. Turn on per-context only with explicit user consent.
- **No personal or credential data in analytics.** Identify only with opaque `partyId`. Never send email, phone, username, display name, roles, passwords, tokens, OAuth codes/state, or free-text personal information as event or person properties.
- **Sensitive URLs are redacted before delivery.** Web analytics masks credential-bearing query values such as reset tokens and OAuth code/state. New URL-bearing events must use the shared sanitizer and its sentinel-secret regression test.
- **DOM autocapture is disabled.** Track only reviewed, named events from the shared taxonomy so labels and user-authored text are not collected implicitly.
- **No server-side event emission for v1.** When we add it for high-value actions (e.g. RSVP broadcast confirmation from `tdf-hq` Haskell), it gets its own doc + a PostHog `phs_` key in the Haskell env, not in the client envs.
- **No experiment assignment on the server yet.** `ExperimentProvider` still rolls dice client-side. When traffic grows enough that sticky-across-devices assignment matters, swap to PostHog feature flags.

## Smoke-checking the wiring locally

```bash
# Mobile
cd tdf-mobile
EXPO_PUBLIC_POSTHOG_KEY=phc_your_dev_key npm run start
# Then open the app, sign in, look for $identify + $screen in the PostHog
# 'Activity' tab tagged with your partyId.

# Web
cd tdf-hq-ui
VITE_POSTHOG_KEY=phc_your_dev_key npm run dev
# Then open http://localhost:5173 and look for $pageview + $identify.
```

If you see nothing in PostHog, check the browser console / Metro log — the no-op client prints a single `[analytics] PostHog disabled: <reason>` line on startup so you know immediately whether the env var was picked up.
