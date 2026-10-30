# Truthful unsupported contract delivery — MSG-CONTRACT-DELIVERY-001

Authority: the existing repository audit dated2026-09-14 and EO-038 gap matrix
identify fake contract-send success without delivery. The current Handler has no
email/queue implementation or dependency and only reads its file. A successful
sent response must not stand in for an action that never happened.

For the legacy file-backed send route, preserve ID/email/payload validation and
stored-contract existence/envelope validation in their established order. Invalid
input remains400, missing files404 and invalid stored envelopes500. After a valid
existing contract is loaded, unsupported email delivery returns HTTP501 with an
explicit not-implemented error; it never returns a successful sent/queued claim.
501 denotes an unimplemented function, rather than a temporary provider outage.

Contract creation/storage and PDF handling remain unchanged. No new provider,
queue, credential, email attempt, delivery receipt, persisted status or production
data mutation is introduced. EO-038's full versioned agreements, party acceptance
and actual delivery workflow remain unimplemented. This correction does not claim
that delivery now works or repair historical confirmation consequences.

Verification obligations: real typed Handler calls with valid contracts created
through the real create route inside owned temporary directories; validsend
refusal, unchanged stored bytes/file set,400/404/500 compatibility, successful
create/storage and pre-render PDF errors. Controls must detect restored false
success and bypassed validation. Full source/manifest/test registration and
canonical Stack qualification are bound separately to immutable commits.

Assumptions: trusted filesystem/compiler/Servant behavior, successful owned fixture
creation, sequential Hspec scheduling for temporary current-directory changes.
The fixture has no SMTP or database configuration; missing/corrupt PDF tests stop
before a PDF renderer. No successful PDF rendering, authenticated HTTP transport,
provider delivery, production filesystem/concurrency or complete refinement proof
follows from these finite tests. Scoped evidence is TESTED only.
