# `/ddex`, `/radio` and `/session` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/Server/DDEX.hs`, `DDEX/DB.hs`, `ServerRadio.hs` and `ServerAuth.hs` on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Guards**
- `/ddex` and `/radio` are behind `AuthProtect "bearer-token"`. DDEX adds `requireDdexCapability` (ModuleCatalog plus a live `catalog.import` or `catalog.export` grant, 403).
- `/session` routes are public at the Servant level and resolve the session themselves (`handler-session`): missing or invalid credentials answer 401, except logout, which answers 204 for anyone.
- DDEX, radio and session tables carry the canonical-integrity triggers listed in the requirements; no trigger enforces the radio catalog.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| ddex documents, validation-runs | REC-DDEX-001 | reviewed |
| ddex partners, import-plans (503), commit (503), exports (503) | DDEX-PARTNERS-PLANNING-001 | new |
| radio streams active, import, refresh-metadata, now-playing | RADIO-CATALOG-001 | new (critical) |
| radio presence upsert, delete | RADIO-PRESENCE-001 | new |
| radio transmissions (503) | MEDIA-STREAM-001 | reviewed |
| session logout | AUTH-LOGOUT-001 | reviewed |
| session preferences, currency-conversions | SESSION-PREFERENCES-001 | new |
| session onboarding intent, complete, reconcile, experiment exposure | SESSION-ONBOARDING-001 | new |

## Defect fixed on 2026-11-07

**Any authenticated account could rewrite the shared radio catalog and make the server fetch URLs.** The stream save, import, metadata refresh and now-playing routes checked nothing beyond a valid bearer session, so a Fan account (no modules) could upsert or import up to 800 `radio_stream` rows per call and trigger up to 400 outbound fetches. `validateRadioCatalogAccess` now requires at least one module grant under a coherent role set (403 otherwise); HQ staff keep the widget, consumer accounts keep their own presence and the read-only search. Tested in `ServerSpec`.

## Open behaviours for the owner to decide

**Outbound fetches**
- **Decided 2026-11-10.** Radio fetches follow no redirect and keep no cookie, and refresh re-validates stored URLs. Still open: host validation is lexical, so DNS rebinding to a private address is not caught; source and metadata bodies are read with no size cap. Whether an egress proxy mitigates this in production is not known here.

**DDEX**
- Partner creation is gated by the import capability alone, accepts an empty name and answers 500 on a duplicate name.
- No distinct-approver rule exists yet for the (disabled) import commit.

**Session**
- Currency-conversion audit rows are unthrottled and accept any minor-unit amounts.
- Logout deactivates any token string presented in either header without checking that both belong to the same party.

**Concurrency**
- Concurrent first inserts of a radio stream URL or of a party's presence row answer 500.
