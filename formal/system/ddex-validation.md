# REC-DDEX-001: canonical validation and Mobile compatibility

`requirements.json` owns the normative lifecycle, atomicity, privacy and recovery
contract. This guide explains the executable boundary, not a second state table.

The migration171 repair retains both legacy issue columns and their historical
values, removing only obsolete NOT NULL constraints. Existing integrity triggers
still require active canonical severity/layer IDs and reject legacy constructors.
Completed runs use result_id; report validity resolves its canonical code.
The synchronous handler locks and reloads the document, binds its hash/storage/
standard relationship, and commits run, issues and document state together. Its
response reads the persisted run timestamps. An ineligible fresh state returns409.

The full HTTP/PostgreSQL fixture in `scripts/test-booking-conformance.py` exercises
private upload/download/preview, structural completion, deliberate503 capabilities,
faults after run/issue/completion writes and canonical guard rejection. Its local
storage contains synthetic XML only; it never contacts a DDEX partner or provider.
This is empirical transactional evidence, not a formal proof. Official XSD and
recipient-profile validation have not run; PROFILE_VALIDATION_REQUIRED stays visible.

Pinned Mobile aliases generated DTOs and loads current reference IDs for document
filtering and partner creation. It renders canonical labels rather than removed
legacy properties. Existing Mobile parity remains restricted: this repair does
not expose upload, preview, raw download, import or export UI. Adapter/component
checks do not substitute for native device acceptance.

`api-response-status.json` admits only six explicitly unavailable handlers whose
Servant return type has a nominal success status. It requires503 and rejects an
invented2xx response (including2XX ranges or default responses), missing/replaced declaration, duplicate policy entry or any
other unresolved status mismatch. Both the always-selected local gate and the
compiled inspector execute it. This proves declared status correspondence only;
385 undocumented routes, DTO codec coverage and broad authorization remain open.

Previously committed validation runs stranded by failures are retained. No cleanup
or historical-success assertion is made. Re-applying migration171 is safe; restoring
legacy NOT NULL after canonical writes is unsupported. Recovery uses a compatible
canonical writer and preserves audit history.
