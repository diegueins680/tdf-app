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
Provider responses are sanitized. Personal revoked consent asks for personal
reconnection; shared credential failure is an operator issue, not TDF logout.

OAuth state binds nonce, owner and expiry; refreshes are single-flight per owner,
and late results cannot overwrite a changed session or credential. Popup consent
keeps the parent form and in-memory File. This is not persistence across reloads.

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
fresh CI receipts. Header magic is checked for public passive media; full codec,
malware and playable-content verification is not established.

This delivery is PARTIAL. Before production activation, complete authenticated
receipt/attachment reader integration, reload-safe scoped form/File drafts,
server-side encrypted delegated credential lifecycle, canonical content and
manifest accounting/provider routing, coordinated backup/restore and exact-image
HTTP/provider rehearsals. The existing browser credential store is owner-scoped
but still localStorage, not an encrypted BFF vault. Mobile changes cover the
shared helper; no native destination/consent UI or store build is certified.

Do not infer production readiness from a model, generated type, migration test
or build alone. See `docs/content-storage/rollout.md` for the concrete release
prerequisites. Existing MEDIA-DRIVE-001/002 contracts continue to describe their
legacy scopes; explicit destination behavior above extends those adapters.

Primary provider reference: Google's [pre-generated identity contract](https://developers.google.com/workspace/drive/api/guides/create-file)
and [generateIds method](https://developers.google.com/workspace/drive/api/reference/rest/v3/files/generateIds),
read November 4. The documented409 behavior motivates persisted provider IDs;
it is not evidence of an actual provider rehearsal.
