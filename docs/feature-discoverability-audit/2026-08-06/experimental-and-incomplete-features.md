# Experimental and incomplete feature decision report

## Restricted or incomplete

| Area | Current state | Risk | Disposition |
| --- | --- | --- | --- |
| DDEX upload/storage | Handler returns `501`; persistence model exists but object storage integration is absent | Misleading import success, partial records | Keep inbox beta; conceal upload/import actions until implemented and transaction-tested |
| DDEX raw download/preview | Handler returns `501` | Broken contextual actions | Do not render as working; complete storage and safe download authorization |
| DDEX import plans/commit | Handlers return `501` | Partial catalog mutation and excessive grants | Mark incomplete/security-concealed; complete conflict resolution, idempotency, rollback, exact import/approve checks |
| DDEX export | Handler returns `501` | Incorrect delivery data | Conceal export until render, persistence, partner rules, and audit tests pass |
| DDEX catalog read-through | Handler returns `501` | Empty or misleading imported catalog | Keep API-only and incomplete |
| Mobile contracts create/detail | Placeholder screens; API helper is unused | Users could assume contracts were sent | Keep incomplete and concealed; helper moved out of route tree; complete authenticated API and record scope before exposure |
| Social event discovery | Beta behind `EVENT_DISCOVERY_ENABLED` | External-source quality and moderation | Preserve flag; session exposes only the enabled flag name, never secrets |
| Native direct messages | No native screen | Web/mobile parity gap | Documented web fallback; complete only with offline/session/privacy behavior |
| Native release creation | No native authoring flow | Partial data entry | Web fallback only; do not emulate a native success state |
| Native teacher/intern/admin workflows | Web only | Mobile parity gap | Documented exceptions; prioritize by legitimate demand, not usage-derived privilege |

## Backend capabilities without a mapped interface

The generated backend inventory contains no unresolved interface decisions. Of 408 concrete endpoints, 358 map to an expected user/admin interface, 32 are intentionally documented API-only, 3 are security-sensitive and concealed, 11 are technical API-only, and 4 are technical/static. Each row includes method, path shape, authentication boundary, stable feature/action where applicable, source alias, and final disposition.

API-only status is deliberate rather than an invitation to expose an endpoint. Public handlers still require their existing validation, signature, or rate-limit controls; authenticated handlers still require exact action and record-scope enforcement. Any later proposal to add an interface must update the registry, permission rules, route mapping, and consistency tests together.

## Orphaned and broken destinations corrected

- The Expo `app/contracts/api.ts` helper was an accidental route. It is now a non-route source module.
- DDEX pages previously suggested operations that the backend cannot execute. The UI now states the incomplete status and avoids fake success actions.
- Incorrect mobile-equivalent metadata that sent preferences, payments, or inventory scanning to unrelated screens was removed or documented.
- Every registered web route/alias and every Expo screen now has a registry classification, and every native destination resolves to a real screen.

No role, module, feature, preference, or historical authorization data was deleted.
