# Account storage and upload recovery: MEDIA-STORAGE-001

The November 4 user-approved prompt sets the free allowance to 5,000,000,000
decimal bytes per Party and preserves larger existing entitlements. This updates
the earlier one-GB proposal; it does not approve its pricing or unrelated catalog
changes. TDF local persistent storage and personal Google Drive are distinct
destinations. Changing a preference does not copy or migrate existing files.

## Executable boundary

`POST /drive/upload` accepts explicit destination, authority, visibility and a
64-hex request identity. A personal request without its credential fails before
selecting any service credential or service folder. Legacy operations uploads
retain their existing authority boundary. Corporate inventory asset uploads use
their existing adapter, outside this personal-account quota implementation.

For managed uploads, the database locks the account before admitting a new
reservation. Known occupied bytes are reconciled legacy usage plus completed,
pending, uncertain and physically unconfirmed deleted objects. Reservations do
not expire into free space. Existing accounts with unknown legacy usage cannot
grow managed storage until a reviewed inventory is applied. New Parties receive
a verified empty account. A fingerprint binds Party, exact bytes, name, MIME and
visibility. Replays preserve identity; changed fingerprints conflict. An expired
lease can be replaced without allocating a new object. Publication requires the
current lease under a row lock. Public staging stays on the destination
filesystem (avoiding cross-bind EXDEV rename failures) and the API rejects its
staging prefix before static serving. Any independent CDN must enforce that denial. Ownership gates list, download and delete.

An owner-requested delete records a tombstone before touching bytes; capacity is
released only after physical deletion is confirmed. Interrupted deletion remains
visible and can be retried. Request cancellation in the browser suppresses late
attachment effects; it does not promise a server cancellation or free quota.

Personal Drive persists a Google-generated object ID, authenticated Party,
provider-account identity and request fingerprint before create. Retry reconciles
that exact ID; creation is not retried with a second generated ID. Explicit public
sharing must succeed; private requests do not grant an anyone-reader permission.
Personal private uploads reject caller-selected folders and withhold results unless a complete provider ACL contains only the authenticated Google owner. This response-time check cannot prevent the user changing provider permissions later and needs a real-provider creation/inheritance rehearsal before activation. Provider responses are sanitized. Personal revoked consent asks for personal
reconnection; shared credential failure is an operator issue, not TDF logout.

OAuth state binds nonce, owner and expiry; refreshes are single-flight per owner,
and late results cannot overwrite a changed session or credential. Popup consent
keeps the parent form and in-memory File. Bounded owner/editor-scoped metadata is
retained in sessionStorage for eight hours and cleared by existing account/session
cleanup. Reload restores form metadata and the request identity, then asks for the
original binary; no File bytes are persisted. Name/size/MIME prechecks are advisory;
the server fingerprint binds the exact bytes. Storage denial is visible. A shared
browser queue admits at most three transfers, including separate widgets and runs.
Unmounted, changed-owner and changed-editor callbacks cannot attach or clear drafts.

## Evidence and open obligations

`content-storage-model.mjs` explores a finite policy model and four deliberately
unsafe variants. It is not a refinement proof of Haskell, filesystem or Google.
`test-content-storage-postgres.mjs` exercises actual concurrent SQL, ordered batch
reapplication, leases, ownership, audit and deletion capacity in a nonce-owned
test database. It is not an HTTP-handler or crash-consistency test. The plan
builder verifies evidence hashes and exact integers without connecting or
authorizing execution. Operator assertions of ownership/completeness still need
independent evidence. No production inventory has been applied.

Web and mobile unit tests exercise recovery and session fences. The isolated
browser fixture uses synthetic HTTP and OAuth; it cannot certify real Google
consent or permissions. Backend compilation and hosted browser execution need
fresh candidate-specific CI receipts. The prior hosted backend run at 7b4b825
completed 4,184 Hspec examples with zero failures and 21 pending; those pending
cases are not passes. Its API inspector found exactly the five new storage routes;
`compiler-observation-20261104.json` records that compiled tree. Header magic is checked for public passive media; full codec,
malware and playable-content verification is not established.

This delivery remains PARTIAL and production activation is blocked. The November 6
security follow-up is a candidate on top of merged main; it does not repeat PR703
or Mobile179. The new backend replaces returned provider credentials with nonsecret
owner-bound TDFConnection handles. Strict client retirement is activated only
with literal `VITE_DRIVE_VAULT_REQUIRED=true`, after backend/key recovery
qualification. Default-off staged compatibility retains the existing owner-scoped
provider descriptor for the current backend proxy and does not meet strict
vault-retirement acceptance; malformed vault identities never downgrade. AES-256-GCM uses fresh 96-bit nonces, a 128-bit
tag and authenticated owner/provider/connection binding. The 32-byte key is supplied
only through `CONTENT_STORAGE_VAULT_KEY` (64 hexadecimal characters), outside the
DB and frontend. Current-session and connection-row locks cover refresh; reconnect
and disconnect erase the prior envelope. Provider-wide Google consent revocation
is separate from disconnecting the TDF connection. Old browser credentials require
new consent; they are never silently imported into the vault.

Private downloads force and verify complete bytes under current session/object
locks. Course receipts, follow-ups and payment attachments bind canonical object
IDs in the domain-write transaction. Another account cannot attach a private file
merely by pasting its URL. Domain staff readers require a current Admin module
grant AND a live bound resource; unrelated staff cannot browse arbitrary files.
Read failures return no body bytes, and successful downloads use attachment,
no-store and nosniff headers. Clients download authenticated blobs and suppress
late delivery after a session change. A separate new native account-storage helper must require explicit
destination/consent/request identity and activation before network I/O. Existing
corporate inventory upload consumers retain their established adapter. Native destination/consent
screens, store/device builds and provider reads still need end-to-end qualification.

Managed public static reads consult the completed/public registry. A pre-commit
rename, unknown object or tombstone does not authorize static serving. The API
rejects overlapping canonical private/public roots. An independent CDN/static
mount must not bypass this reader. Upload copy/publish holds the current session
and upload-row locks; deletion cleans pending staging before acknowledging freed
capacity and refuses live attachments. Missing physical acknowledgement retains
occupancy. There is no crash-durability or coordinated-restoration proof yet.

`CONTENT_STORAGE_UPLOADS_ENABLED` defaults disabled and only literal `true`
admits account uploads. No production flag is changed by this work. Complete the
historical ownership/size inventory, canonical publication body/manifest provider
routing, fresh compiled HTTP/handler tests, current protected CI and independent
review, real-provider consent/revocation/permission checks, persistent-volume
replacement and coordinated database/files/key recovery before activation.

Do not infer production readiness from a model, generated type, migration test
or build alone. See `docs/content-storage/rollout.md` for the concrete release
prerequisites. Existing MEDIA-DRIVE-001/002 contracts continue to describe their
legacy scopes; explicit destination behavior above extends those adapters.

Primary provider reference: Google's [pre-generated identity contract](https://developers.google.com/workspace/drive/api/guides/create-file)
and [generateIds method](https://developers.google.com/workspace/drive/api/reference/rest/v3/files/generateIds),
read November 4. The documented409 behavior motivates persisted provider IDs;
it is not evidence of an actual provider rehearsal.
