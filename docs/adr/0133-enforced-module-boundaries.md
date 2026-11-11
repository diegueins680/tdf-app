# ADR 0133: Declared module layers, enforced by a shrink-only ratchet

Date: 2026-11-10. Status: implemented in `scripts/check-architecture-boundaries.mjs`
and `docs/architecture/module-boundaries.json`; requirement SYS-ARCH-BOUNDARY-001.

## Finding

An architecture review of main `083d1c6e` found the structure eroding in the
ways that end in a big ball of mud. Nothing stated which module may depend on
which, so nothing stopped the next shortcut.

Backend (`tdf-hq`, 274 modules, about 159,000 lines):

- `TDF.Server` is 21,881 lines, imports 104 project modules and has no export
  list, so every helper in it is public. `TDF.Server.SocialEventsHandlers` is
  10,874 lines and `TDF.ServerExtra` 5,254. Eight modules exceed 2,500 lines.
- Background work depends on HTTP delivery. `TDF.Cron` imports nine helpers from
  `TDF.Server`; the payment event worker imports its PayPal processing from
  `TDF.Server.ServiceStorefront`; two domain modules imported a URL validator
  through `TDF.Trials.Server`.
- The route contract imports a response type from a handler module
  (`TDF.Server.TicketPaymentReceipt`), and seven domain modules import types
  that are declared beside route definitions in `TDF.API.*`.

Web (`tdf-hq-ui/src`, 521 non-test modules):

- `api` and `session` import each other at run time (eight edges), `utils`
  imports `api`, `features` and `mobile`, and shared `components` import
  `features`. A change to one of these directories can break any other.
- Nine modules exceed 2,000 lines; `CourseRegistrationsAdminPage.tsx` is 9,669.

## Decision

1. Each system declares ordered layers, lowest first. A module may import its
   own layer and lower layers only.

   Backend: `persistence` (Persistent models) → `transfer-types` (`TDF.DTO*`,
   `TDF.API.Types`) → `domain` (everything not matched elsewhere, including
   workers and `TDF.Cron`) → `http-contract` (`TDF.API*`, `TDF.Routes.*`) →
   `http-delivery` (`TDF.Server*`, `*.Server`) → `composition` (`TDF.App.Boot`
   and the executables).

   Web: `foundation` (`vendor`, `assets`, `styles`, `lib`, `i18n`) → `utils` →
   `config` → `api` → `app-services` (session, services, analytics, contexts,
   hooks, player, mobile, desktop, theme) → `components` → `features` → `pages`
   → `routing` → `shell` (`App.tsx`, `main.tsx`, `player/PlayerShell.tsx`).

   Type-only TypeScript imports are not edges: the compiler erases them, so they
   cannot create a load-order or bundle dependency. Test files are not scanned.

2. A module may not exceed 2,500 lines (backend) or 2,000 lines (web, generated
   API types exempt).

3. Existing debt is listed, not excused. `knownViolations` names each offending
   import and `sizeBudget.oversized` gives each oversized file a ceiling just
   above its current size. The check fails when:

   - an import crosses a layer upward and is not listed;
   - a listed import no longer exists (the entry must be deleted, so the gain
     cannot be lost again);
   - an unlisted file passes the limit, or a listed file passes its ceiling;
   - a listed file has shrunk more than the headroom below its ceiling (the
     ceiling must come down with it).

   The lists can therefore only shrink unless a reviewer accepts an explicit
   edit to the baseline in the same change.

4. The check runs in `npm run quality:repo`, which CI runs for every change.

## What this change fixes directly

`TDF.Profiles.Artist` and `TDF.Artists.Enrichment` now import `isValidHttpUrl`
from `TDF.PublicUrl`, where it is defined, instead of through
`TDF.Trials.Server`, which only re-exported it. Behaviour is identical. Domain
code reaching into HTTP delivery drops from four imports to two.

## Recorded debt and the order to retire it

Listed in `docs/architecture/module-boundaries.json`: 11 backend imports, 21 web
imports, 17 oversized files. Suggested order, each as its own small change:

1. `TDF.Cron -> TDF.Server`: move the course metadata helpers and the RAG reply
   function into domain modules. This removes the worker's dependency on the
   whole HTTP server and is the first real cut into `TDF.Server`.
2. `TDF.Commerce.ProviderEventWorker -> TDF.Server.ServiceStorefront`: move
   PayPal webhook parsing and processing under `TDF.Commerce`.
3. `TicketPaymentReceipt`: move the response type out of the handler module so
   the route contract stops importing delivery.
4. Web `api -> session`: move `logoutIntent` and `authEvents` below `api`; they
   are storage and event primitives, not session UI.
5. Web `features/featureRegistry.ts`: it is static registry data used from
   `utils`, `api` and `components`; move it to a low layer.
6. Split the oversized files along the seams their own section comments mark,
   lowering each ceiling in the same change.

## Consequences

- A change that adds an upward import, or grows a listed file past its ceiling,
  fails repository checks with the offending edge or file named. The fix is to
  place the code in the correct layer or a new focused module. Raising a ceiling
  or adding a `knownViolations` entry is possible but is a visible, reviewed
  edit to the baseline.
- Removing debt requires deleting its baseline entry in the same change.
- Large code motion out of `TDF.Server` was deliberately not bundled here: with
  serialized merges and many open branches touching that file, a bulk move would
  conflict with every one of them. The ratchet stops the growth now; the
  extractions land one seam at a time.

## Limits

The check reads import statements. It does not see dependencies through
re-exports within a layer, Template Haskell, or string-built dynamic imports,
and layers are assigned by module name, so a misnamed module is misclassified
until the configuration says otherwise. It measures direction and size, not
cohesion; a small module in the right layer can still be badly designed.
`tdf-mobile` is a separate repository and is not covered.
