# PR 36 — traceable stale catalog reconciliation

## Scope and dependency

Branch `chore/event-catalog-stale-reconciliation`, based on draft #416,
`d5505e2731d395cc2a5859d215e36ffda9e88138`. Continues the approved `gh-fix-ci`
plan and the [pre-edit contract](catalog-stale-reconciliation-contract.md).
This increment changes review metadata and tests, not event behavior.

The exact parent hosted catalog job reported **163 unreviewed / 9 stale**, exit 1:
[job 104957866545](https://github.com/diegueins680/tdf-app/actions/runs/35144761473/job/104957866545).
The parent's formal `verify` and ten PostgreSQL jobs passed in
[run 35144761445](https://github.com/diegueins680/tdf-app/actions/runs/35144761445).
Those are parent checks, not new verification of this increment. External Vercel
and Cloudflare checks were not diagnosed or changed.

## Individual review and retained evidence

All nine old fingerprints were reconstructed using the unchanged scanner and
historical source. Mobile source revisions were resolved from historical root
gitlinks, not inferred from file names or an empty checkout. The complete original
decision objects, full source/root SHAs, successor IDs, value counts and deltas are
retained in [the retirement ledger](../catalog-persistence/event-catalog-retirements.json).
Object digests detect accidental evidence changes; they are not signatures or an
independent tamper-proof audit service. Git preserves the original decision file.

| Candidate | Retired → reviewed current fingerprint | Evidence and remaining authority |
| --- | --- | --- |
| Web `SocialRsvpStatus` | `1467b26fe9075c13f2f7` → `91952765f2d528c2a6c6` | Title-case to canonical lowercase; governed reference consumer of `event_rsvp_status_check` and DTO normalization |
| Web `SocialEventsAPI` | `2245e57c3a5e6500548e` → `74f76e178df15df54768` | 61 to 66 methods: one legacy RSVP method replaced by six self-scoped/feed functions; technical dispatch |
| OpenAPI `Role` | `e56ffb099089c4e7309f` → `c7c31a11cdb3859ae4bb` | 30 to 31 roles, adding Agency; security registry consumer, not a role grant |
| Lifecycle capabilities | `3916642c4d54d32e3205` → `0eb0488703525594608c` | Adds RSVP to listing/purchase boundaries; persisted workflow capability assignments |
| Mobile `Events` | `dbd19df5feca0af20a2c` → `068d6f3f0c164631ab35` | 24 to 29 methods; public detail and four RSVP methods; technical dispatch |
| `eventUpdateAllowedKeys` | `3f00137f715a55f77dc3` → `2a1f5ad58ef7b8fc4bf3` | Adds `eventRsvpEligible`; corrects inconsistent historical classification to security parser boundary, not writable-field authorization |
| Mobile generated registry | `b98598fcc3a39ba88a8d` → `ffb84e8fd3972fd0730d` | 143 to 144 entries, adding `reputation.consents`; generated from backend feature asset, security-sensitive navigation consumer |
| Mobile onboarding `intent` switch | `3b37706d3daff89e8890` → `34d5f2f94d1a165783ba` | Same six labels reordered; business consumer with pending authoritative alignment |
| Production migration manifest | `1af20293af1230e9fbb9` → `7578a30aa3cc5aa23870` | 91 to 98 entries plus dependency reorder; deployment security registry, no execution authorized |

The mobile onboarding fingerprint covers case labels, **not function bodies**.
Historical `ad16d28e3df1a8b5b358542f5823c5b3ec0dfd04` used server progress and
feature-registry helpers; current source uses local storage and role/module hints
and defaults to `MOBILE_LANDING_ROUTE`. Its catalog decision explicitly requires
consumer alignment. It does not certify authoritative onboarding, safe offline
permission changes or equivalence of the old and new implementation.

Likewise, navigation classification does not complete persisted governance, an
OpenAPI enum does not grant Agency permissions, and accepting an event projection
key in a strict parser does not authorize its mutation. No security finding is
resolved solely by a catalog classification.

## Executable verification

```sh
npm run test:catalog-list-audit
npm run test:production-release
node scripts/catalog-list-audit.mjs \
  --decisions docs/catalog-persistence/catalog-list-decisions.json \
  --fail-on-unreviewed \
  --output /private/tmp/tdf-catalog-reconcile.8wzsku/reviewed.json
git diff --check
git -C tdf-mobile status --short
git -C tdf-mobile rev-parse HEAD
```

- Catalog tests: **3 passed**. They verify ledger object digests, unique reviewed
  successors, exact current fingerprints from isolated source copies, and a
  negative control in which the nine retired decisions still fail the gate.
- Release/entrypoint regression tests: **61 passed**. These use static checks and
  isolated fixtures; this is not a database migration rehearsal or deployment.
- Separate executed Node assertions compared decisions with exact parent `d5505e273`:
  exactly nine replacements, every original archived object equal to its parent,
  every other decision unchanged, all nine successors previously unreviewed.
- Full repository audit: **154 unreviewed / 0 stale**, exit 1 as required for
  remaining findings. Additional Node assertions verified identical discovered
  fingerprint IDs and exactly nine changed classifications against the complete
  parent report. No candidate was excluded. The mobile source remains clean at
  unchanged gitlink `53569fc4baa842a6882235d9a12c4ee68c44ff24`.

Historical reconstruction used `git rev-parse REV`, `git ls-tree REV tdf-mobile`,
`git show REV:FILE` (or `git -C tdf-mobile show MOBILE_REV:FILE`) for each ledger
entry. The existing scanner's pure `scanTypeScript`, `scanHaskell`, `scanYaml` and
`scanJson` functions were evaluated from its unchanged source with `typescript`
and `createHash`; assertions required both the retired ID and current ID to match.
The first reporting attempt exceeded the tool output limit because it included
all active decisions; the retry output only the nine evidence entries and passed.

| Contract obligation | Executable evidence |
| --- | --- |
| Preserve historical decisions; no unrelated deletion | Ledger digest test and exact-parent comparison |
| Review the exact present source, including mobile | Isolated-source fingerprint test and complete audit |
| No stale-ID waiver | Negative control with nine retired decisions fails |
| No migration or source mutation | Scoped Git diff; release regression checks; clean mobile checkout |

No TLC/Alloy rerun, browser screenshot, backend build, database transaction,
migration/rollback execution or end-to-end application success is claimed for
this metadata-only increment. Existing domain models and implementation remain
unchanged.

## Rollback and remaining work

Restore the nine original active decisions from the ledger, remove the nine
successors and revert this documentation/test increment. There is no persisted
application data to migrate or roll back. The retirement ledger is never loaded
by the scanner and cannot act as an approval allowlist.

Continue individual review of remaining candidates; preserve business/security
classifications and their unresolved migration or consumer work. The catalog
gate and full event-operations definition of done remain incomplete. No merge,
production deployment, provider activation or real-money operation was performed.
