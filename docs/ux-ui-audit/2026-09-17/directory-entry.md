# Directory arrival and current-session favorites — 2026-09-18

Continuation of UX-260917-007; new confirmed UX-260917-046. Source baseline
`2ce7a1a0d275ecc65924b5df68bc39cd7a373b1d`. This record supplements the existing
register, without replacing earlier startup fixes or implying full directory acceptance.

## Reproduced failures and implementation

The public search evaluates an optional saved city outside a storage guard and writes
it from an unguarded effect. Three component counterexamples deny the storage getter,
getItem and setItem: all three fail the original component and pass the successor.
The search now uses lazy, guarded preference access. An explicit city in the URL takes
precedence; absent/denied preference falls back to the authoritative city catalog. The
selected city and recoverable query remain in memory and the URL when persistence is denied.

Favorites originally check only a party ID updated by a React effect. Three actual
component counterexamples dispatch after a session change before repaint, accept a late
response before repaint, and accept a response after A→B→A. Each fails the previous
implementation. The successor checks the immediate SessionContext authority, render
occurrence and mount state before dispatch and before receipt effects. A unique component
scope plus occurrence partitions favorites query caches; mutation cards remount on session
changes while the parent preserves search input. A stale read also cannot populate a new
session. The same authority predicate is passed through the existing asynchronous first-value
persistence helper. A failed transport no longer asserts that the account was unchanged:
the visitor can explicitly refresh the authoritative favorites list before retrying.

## Executed verification and scope

- 11 DirectorySearchPage cases plus 7 onboardingProgress cases pass. Includes all six
  original counterexamples, late private read, ambiguous mutation recovery, acknowledged
  save, removal, storage fallback, images and component accessibility.
- Production bundle: TypeScript, lint and build pass; 365639 gzip bytes of initial JS,
  five preloads, below the established 410 KiB budget.
- Browser test `e2e/web/directory-entry.spec.mjs` uses synthetic read-only APIs and the
  actual production bundle. It checks explicit URL-city precedence, denied storage,
  keyboard search submission, reload, and serious/critical axe findings across five
  profiles/three engines. The receipt records actual outcomes, not backend persistence.
- Baseline browser comparison uses the unchanged DirectorySearchPage in the reflow
  candidate bundle: getter/setItem fail; getItem passes because the explicit URL city
  correctly bypasses that read. The no-URL getItem failure is independently reproduced
  by the component test. An initial broad Playwright selector matched the worktree path;
  that run was interrupted and excluded before using the exact spec filename.
- Strict catalog audit uses the parent-pinned mobile 4122bb75e9c550eaf5ed7ee81b89ee5188938271;
  all 1138 candidates retain reviewed classifications.

## Formal contract and conformance

`DirectoryFavoriteAuthority.tla` models one read/save operation, three session occurrences
(A→B→A), lagging React render, queued dispatch, response success/failure, and unmount.
TLC 1.7.2 explores 356 generated / 208 distinct states, depth 9. Safety properties require
current authority at dispatch, persisted receipts and current mounted-session receipt
application. `RequestSettles` assumes weak fairness for dispatch and a success/failure
response; a hung transport is not certified. Terminal quiescence is valid, so deadlock
checking is disabled explicitly. Bounds do not cover unlimited sessions or multiple tabs.

The unsafe dispatch configuration violates `AuthorizedDispatch` in 29 generated states;
the old party/render receipt guard violates `CurrentSessionReceipt` in 63. The runner
requires these exact failures. All pinned TLC/Alloy models also pass their positive and
negative-control gates. Mapping: Queue/Dispatch → ResultCard click and mutationFn;
Switch/Render/Unmount → immediate SessionContext, occurrence and mounted guards;
Response → queryFn/onSuccess/first-value predicate. Regression tests reproduce each
changed mechanism. No claim is made about server revocation, independent endpoints,
physical assistive technology, field performance or whole-platform completion.

## Release boundary

No migration, API or mobile contract change. Merge requires independent review and
applicable checks. Cloudflare publication and actual production acceptance remain pending.
The broader English directory content and other not-yet-executed directory failure states
remain explicit coverage work; this increment does not claim to complete them.

## Review follow-up: close the recovered error state

Review4046711715 identified that a successful query refresh did not reset the separate
mutation error. `refreshFavorites` now returns success only for an authoritative response
owned by the same occurrence; the card resets its error only for that receipt. A failed
refresh keeps the error and retry action. Nineteen focused component/helper cases pass,
including both refresh outcomes. Production bundle rebuild:365647gzip/fivepreloads.
The Spanish branch of the bilingual recovery browser script is also retained as
`directory-favorite-recovery.spec.mjs` to exercise delayed search, search retry, ambiguous
save, authoritative refresh and error dismissal. Browser receipt records the final result.
