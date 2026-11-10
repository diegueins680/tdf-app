# `/catalog` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/Server/Catalog.hs`, `API/Catalog.hs` and `tdf-hq/sql/2026-08-14_catalog_integrity.sql` on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Guards**
- All 13 routes are behind `AuthProtect "bearer-token"`.
- `requireCatalogCapability`: ModuleCatalog (Admin, Manager, StudioManager) plus a persisted `security_permission` grant with the named `catalog.*` code; 403 otherwise. `requireSecurityCapability` is the same with ModuleAdmin and `security.*` codes. No strict-admin check in the module.
- Every `runDB` is its own transaction: multi-step catalog handlers are not atomic unless noted (reorder is).
- Triggers: `catalog_revision_workflow_integrity` raises 42501 whenever `approved_by = created_by` on any catalog; the security triggers allow self-approval only in a justified emergency by a coherent emergency administrator.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| revisions create, submit, approve, reject | CAT-REVISION-WORKFLOW-001 | new (critical) |
| items activation, reorder, merge, import.csv | CAT-DIRECT-ITEM-001 | new |
| security party-role and role-permission revisions, submit, approve, reject | CAT-SECURITY-GRANT-001 | new (critical) |

`REC-CATALOG-001` already specified the reorder transaction; the ledger now links it through CAT-DIRECT-ITEM-001 as well.

## Defect fixed on 2026-11-07

**Self-approval of a non-sensitive catalog change published the item and then failed with 500.** `approveRevisionHandler` only refused the author on sensitive catalogs, but the database trigger refuses `approved_by = created_by` on every catalog. For a non-sensitive catalog (or a sensitive one with `emergencyOverride`) the handler published the item, bumped the catalog version and wrote the audit before the revision update raised 42501: the item was live while its revision stayed in review, and a retry answered 409 on the item version. `validateCatalogApproval` now refuses the author on every catalog (403) before anything is published; the override no longer lifts that rule. Tested in `tdf-hq/test/TDF/Catalog/SecuritySpec.hs`.

## Open behaviours for the owner to decide

**Workflow**
- Catalog submit and reject write no audit row; the security routes write one per transition.
- Anyone with `catalog.update` may submit another author's draft; the security routes require the author.
- Publication (item upsert, family tables, aliases, catalog bump, audit) and the revision update are separate transactions.
- Revisions approved with a future `scheduledPublishAt` stay in `approved`: no code path publishes them later.
- The emergency override flag now only distinguishes sensitive from non-sensitive catalogs; whether a justified emergency self-approval should exist for catalogs (as it does for security grants) is an owner decision.

**Merge and import**
- `catalog_merge_operation` is inserted and never read: approving the merge draft republishes the source unchanged. A merge has no merge effect.
- Import jobs are deduplicated by digest but their drafts are not; re-posting a CSV with `dryRun=false` creates a second set of drafts.
- Sensitive catalogs can be deactivated directly, bypassing the approval workflow.

**Security grants**
- A grant-version conflict detected under the advisory lock answers 500 (`userError`) instead of 409.
- The target permission is not checked for being active at draft time; the approval triggers decide.
