# Private persistent contracts

`DATA-CONTRACT-001` and `AUTHORITY-047` govern the mounted contract API. The
canonical row records actors, transitions, failures and traceability. The event
finance workflow stores returned contract IDs; those references require durable
private document storage across container replacement.

New JSON documents are published exclusively under `uploads/contracts` using
`TDF.Storage.AtomicPublication`. Creation synchronizes the file and directory
ancestors, acknowledges only a newly published UUID, and never overwrites a
retained ID. Directory mode is0700; document mode is0600. This depends on trusted
stable ancestors and the independently admitted persistent uploads mount. A lost
response can leave a complete unreferenced document; create is not idempotent.
Database references and file publication are separate effects.

Reads validate canonical IDs and strict JSON bytes. Legacy `contracts/store` is
read-only fallback when the current file is absent. Different duplicate copies,
unsupported files, I/O errors and malformed current data fail; identical copies
are accepted only after the normal stored-document validation. Reading never
migrates or deletes evidence. Neither directory is a public static route; existing
Operations authorization still governs creation, PDF access and send validation.

Contract delivery has no provider implementation. Valid requests for existing
contracts return503 instead of fabricated sent/queued success. Invalid input,
missing documents and access denial retain their existing semantics. OpenAPI
records these three existing routes and unavailable delivery explicitly; generated
web and exact Mobile clients follow the same contract. Mobile contract authoring
remains explicitly unavailable in its shipped screen.

The recovery capture path must inspect the exact retained stopped root for legacy
contract absence, regardless of whether uploads are mounted. It rejects any entry,
symlink, inaccessible directory or unadmitted mount. It repeats inspection after
capture. Live absence observations are not release admission. A populated store
requires an explicit reviewed preservation/migration procedure before original
container removal. The current retained-root adapter only admits legacy uploads;
a persistent-upload source without an independently qualified retained root is
therefore blocked. No automatic migration or directory-discard exception exists.

`verify-contract-storage.py` compiles ten filesystem/concurrency examples and
three broken variants (ephemeral location, ignored creation collision, legacy
fallback hiding a conflict). The backend quality gate runs it. The HTTP/PostgreSQL
booking conformance runner tests actual contract persistence, authorization,
private-route denial, legacy compatibility and503 behavior. Portable and actual
owned-Linux capture fixtures reject uncaptured legacy files, including files that
appear after an earlier absence observation; metadata rejection has a separate
unmasked control. These are scoped executable checks, not a universal proof or
production acceptance. File/database atomicity, provider delivery, arbitrary
hardware power loss and hostile privileged directory replacement are excluded.
