# `/artists`, `/assets` and `/label` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/Server.hs` (artist and label sections), `Profiles/Artist.hs`, `VideoCampaigns.hs`, `ServerExtra.hs` and `API/Inventory.hs` on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Guards**
- All 20 routes are behind `AuthProtect "bearer-token"`.
- Artist self-service: coherent auth scope for activation; Artist, Artista or Admin role for profile edits, with the artist id forced to the caller. Video campaigns re-validate the session, roles and manager grants inside the transaction (MEDIA-VIDEO-CAMPAIGN-001).
- Inventory: `ensureInventoryAccess` = ModuleOps or ModuleAdmin (403). Label: roles only (Admin, Artist, Artista for tracks; Admin, Webmaster for notes), no module check.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| me/activate, me/profile (POST, PUT), me/photo | ART-SELF-SERVICE-001 | new (critical) |
| video-campaigns save, conversion, feature | MEDIA-VIDEO-CAMPAIGN-001 | reviewed |
| assets create, upload, patch, delete, qr | INV-ASSET-ADMIN-001 | new |
| assets checkout, checkin | INV-ASSET-LOAN-001 | new |
| label tracks and project notes | LABEL-NOTES-TRACKS-001 | new |

`ArtistActivationSpec` covers the activation transaction (serialized concurrent activations, idempotent retry, rollback).

## Open behaviours for the owner to decide

**Storage**
- `POST /assets/upload` trusts the declared MIME type, writes into the public assets root, keeps no database record and never cleans up.

**Concurrency**
- Staff asset checkout checks for an open loan outside the insert transaction with no lock or partial unique index; two concurrent checkouts can both succeed (same finding as the public QR twin).
- The artist photo route reads then writes across two transactions and can overwrite a concurrent profile edit; track updates and note deletes have no version check.
- A delete racing a checkout answers 500.

**Status codes and policy**
- Duplicate artist slugs answer 500; an administrator naming an unknown or archived track owner answers 500.
- Activation reports a missing or inactive self-service policy as 403 rather than 503.
- QR rotation invalidates printed codes with no confirmation or audit.
