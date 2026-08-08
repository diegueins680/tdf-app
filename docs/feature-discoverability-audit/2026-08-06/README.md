# TDF feature discoverability and authorization audit

Audit date: 2026-08-06. Registry version: `2026-08-06`.

This directory is the reproducible, anonymized audit packet for the web application, mobile application, backend surface, and production authorization assignments. It contains no names, email addresses, tokens, full production identifiers, or record contents.

## Inventory

- 115 product, contextual, incomplete, and technical feature records.
- 37 primary destinations, 50 important submodules, 19 record/context routes, and 9 technical routes.
- 125 composed React Router paths or redirects checked against the registry.
- 35 Expo Router screen paths checked against the registry.
- 408 concrete endpoints expanded from the root Servant `API` type. All have an explicit disposition: 358 map to a user/admin interface, 32 remain documented API-only capabilities, 3 are security-sensitive and concealed, 11 are technical API-only routes, and 4 are technical/static routes. In total, 393 map to a stable feature/action and zero remain undecided. Technical endpoints are deliberately not turned into navigation entries.
- 2,530 role/module/feature/action/platform matrix rows, covering 11 representative user types on web and mobile and 17 distinct actions.

Generated deliverables:

- [Feature inventory](feature-inventory.csv)
- [Route classification and web/mobile mapping](route-and-mobile-mapping.csv)
- [Role × module × feature × action × platform matrix](role-module-feature-action-platform-matrix.csv)
- [Complete backend capability inventory](backend-capability-inventory.csv)
- [Machine-readable counts](generated-summary.json)

Regenerate and verify them with:

```sh
npm run generate:features
npm run audit:features
npm run generate:feature-audit-reports
```

## Implemented architecture and information design

The authoritative public-safe registry is `tdf-hq/assets/feature-registry.json`. Web navigation, command search, quick creation, route guards, breadcrumbs, locked states, recent/favorite/pin behavior, mobile presentation, telemetry IDs, and backend feature-action checks consume it. The backend embeds the registry at build time; mobile receives a generated typed copy with a SHA-256 source marker; web imports the source directly. The backend remains the enforcement authority.

Spanish remains the default. Labels, descriptions, synonyms, and search keywords include Spanish and English. Technical routes are neither searchable nor navigable. Public Marketplace, Community, Domo, booking, donation, and related destinations remain discoverable to authenticated users.

Navigation is organized into predictable registry groups, then ranked by legitimate preferences. Authenticated preferences persist on the server across devices. Removed permissions cause stored shortcuts to be filtered as unavailable. Every primary destination is available in the sidebar/drawer or permission-aware feature explorer; the global command palette and mobile explorer provide the second interaction path.

Global quick creation includes Nuevo contacto, Nueva reserva, Nuevo lanzamiento, and Importar DDEX, plus Nuevo evento. Each item evaluates the exact create/import action. A safely disclosable missing permission routes to the internal access-request experience; concealed capabilities do not appear.

Breadcrumbs are generated from registry feature IDs and omit inaccessible ancestors. Context routes keep parent/list/module paths without putting record details in global navigation.

## DDEX

The information architecture is:

- `DDEX / Bandeja` under Label/Sello.
- `DDEX / Partners` as an administrative suboption.
- Document detail and validation from the inbox.
- Import from document context and quick creation only when the import action is authorized.
- Error, failed-validation, and pending indicators use permission-filtered server results.

The source still contains incomplete DDEX storage, preview, import-plan, import-commit, export, and catalog-read-through handlers that return `501`. Those actions are not presented as working. Import detail is marked incomplete and security-concealed, while the inbox explains the limitation. See [experimental and incomplete features](experimental-and-incomplete-features.md).

## Internal access requests

The former mailto-based request path is replaced by an authenticated workflow with exact feature/action selection, role/module context captured at submission, optional justification, reviewer group, timestamps, pending/approved/rejected/cancelled/expired states, reviewer notes, notification rows, audit history, self-review prevention, duplicate-pending protection, and transactional state changes.

A reviewer must already possess the exact requested action and cannot decide their own request. Approval records a provisioning decision; it does not silently assign a role, module, or broader grant. Effective access remains unchanged until an authorized administrator applies a compatible existing role/module change. This deliberate separation avoids privilege escalation but leaves automated provisioning as a documented follow-up.

## Security corrections in this revision

- Venue create/update now enforces the exact registry action instead of relying on a visible button.
- Artist profile creation is restricted to the authenticated party except for a coherent strict administrator.
- Artist profile edits require ownership or coherent strict administrator access and cannot reassign the owner.
- Artist follow/unfollow ignores no caller identity: the referenced follower must equal the authenticated party.
- DDEX handlers enforce exact view/import/validate/approve/export actions and return safe errors.
- Unsupported feature actions no longer inherit `view` permission in web, mobile, or backend evaluators.
- Navigation preference reads/writes filter current effective access and stale shortcuts.
- An accidental non-screen TypeScript helper was moved out of Expo's `app/` route tree.
- Public feature flags now arrive through the authoritative session snapshot; no configuration secrets are exposed.

No production authorization records were changed.

## Supporting reports

- [Production assignment audit](production-assignment-audit.md)
- [Experimental and incomplete features](experimental-and-incomplete-features.md)
- [Registry, authorization, and access-request design](architecture-and-authorization.md)
- [Telemetry and privacy](telemetry-and-privacy.md)
- [Verification and accessibility evidence](verification.md)
- [Business-flow verification by user type](business-flow-results.md)
- [Rollout and rollback](rollout-and-rollback.md)

## Delivery status and external gates

Implementation, generated inventories, unit/integration authorization tests, isolated migration forward/rollback, responsive screenshots, and automated accessibility scans are complete on the audit branch. No production role/module correction, migration, backfill, or deployment was performed.

Production rollout is deliberately blocked by two independent safety controls: production has only one verified coherent emergency-administrator credential path, and the reviewed draft pull request has not been merged to the protected default branch. The exact continuation action is to verify a second emergency administrator, review and merge the pull request, take an encrypted authorization/navigation export, and then execute the bounded rollout in [Rollout and rollback](rollout-and-rollback.md).
