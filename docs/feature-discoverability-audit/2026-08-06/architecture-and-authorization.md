# Registry, authorization, and access-request architecture

## Effective access

Effective access is evaluated from authentication, coherent roles, modules derived from those roles, the exact feature action, public feature flags, and handler-owned record scope. A missing action is denied; `view` is never a fallback for create/edit/delete/import/approve or any other action. Technical, broken, and incomplete routes are concealed.

Role normalization covers established aliases such as A&R and Studio Manager. Strict administration requires Admin with only the baseline Customer/Fan roles and coherent derived modules. The frontend evaluator supports safe locked disclosure, but backend checks remain authoritative.

Record scope is metadata for tests and explanation, while the actual owner/tenant check stays in the handler where the record is loaded. Newly corrected artist and follower operations demonstrate this boundary.

## Registry consistency contract

`npm run audit:features` fails for duplicate IDs or telemetry IDs, missing bilingual labels/descriptions/search metadata, invalid parents or breadcrumbs, invalid classifications/actions, technical feature exposure, contextual routes in global navigation, incomplete mobile exceptions, invalid quick actions, API scopes without an exact action rule, nonexistent React Router destinations, nonexistent Expo destinations, unclassified screens, or a stale generated mobile registry.

`npm run generate:feature-audit-reports` expands the root Servant type graph and regenerates all CSV matrices. A newly added endpoint is therefore visible in the next review even when it has no frontend.

## Role fixture interpretation

The matrix uses backend-derived modules and baseline Fan + Customer roles for authenticated fixtures:

- Fan/client: Packages.
- Artist: Scheduling + Packages.
- Professor: Scheduling + Packages.
- Intern: Internships + Packages.
- Reception: CRM + Scheduling + Packages.
- Producer/A&R: CRM + Scheduling + Catalog + Packages.
- Studio manager: CRM + Scheduling + Packages + Invoicing + Admin + Internships + Ops + Catalog.
- Maintenance: Packages + Scheduling + Ops.
- Webmaster: Admin + CRM + Packages.
- Administrator: all modules with the strict Admin + Fan + Customer combination.

The visitor fixture has no session. Matrix states distinguish allowed, locked/requestable, login-required, flag-required, concealed, unsupported action, and platform unavailable.

## Navigation personalization

Preferences are keyed by stable feature ID and authenticated party. Favorite, pinned, pin order, last visit, use count, and update time persist across devices. The server re-evaluates current view permission before returning or accepting a preference, so a removed permission leaves no usable stale shortcut. Sensitive and incomplete features cannot be persisted.

## Access-request state machine

`pending` may transition atomically to `approved`, `rejected`, `cancelled`, or `expired`. Only the requester can cancel, reviewers cannot decide their own request, rejection requires notes, duplicate pending requester/feature/action rows are rejected by application logic and a partial unique database index, and every transition writes history plus an audit event. Eligible reviewers receive an internal notification only if their current effective access includes the exact requested action.

Approval is intentionally non-provisioning in this revision. It is an auditable decision for a later existing-model role/module change; the request cannot itself create a broader privilege.
