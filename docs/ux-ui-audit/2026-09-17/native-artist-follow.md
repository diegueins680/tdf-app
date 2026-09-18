# Native artist follow: canonical persistence and current session

Finding UX-260917-037, dependency STATE-01. Mobile104 uses the existing FanHub
Party-ID catalog and `/fans/me/follows/:artistId`, matching web. The former
social-event artist namespace persisted a different row and did not satisfy the
server's first-value evidence. Real isolated HTTP/PostgreSQL plus iOS runtime
reproduced the old successful follow / uncompleted onboarding mismatch; the
corrected native follow and real web return then agreed on persisted completion.
See `evidence/native-canonical-follow-negative.json` and the positive receipt.

The follow-up test actually reproduced A → B → A: both the old and latest mutation
callbacks accepted the old receipt and wrote account A's current query cache.
`useSessionOwnership` now captures the session occurrence, including credential
changes, as a predicate in mutation variables. Dispatch, success/error and the
existing authoritative completion helper check it. It is not sufficient to compare
Party IDs after account return. Mobile source09649d63ef219e3b936adf7ca381b249d7277b80
contains this fix plus the reviewed102 documentation; it excludes unrelated intake
application changes.499 mobile tests, TypeScript and lint pass. A previous full run
hit shared disk exhaustion and one timing failure; after removing only the owned
simulator, the affected6 tests and all84suites passed. That interrupted run is not
passing evidence.

## Executable model and conformance

`formal/event-operations/NativeArtistFollow.tla` is run by the existing pinned
`verify-event-operations-formal.sh` gate. TLC2.17 / tla2tools1.7.2 (SHA verified by
runner), Temurin21.0.12.1+1; Alloy6.2.0 also runs in the full unchanged suite.
Two dispatch slots, two accounts and three session occurrences model A → B → A.
Each dispatch either fails or persists the actor's follow, then returns and performs
the completion handshake. Set-valued persisted state represents the existing
idempotent backend relation. Session changes may interleave at every step.

- `CurrentSession`: applying a response requires the captured occurrence, not merely
  the same Party. Mechanism: captured predicate in artist mutation variables;
  regression: `SocialScreen.test.tsx` leaves and returns before delivering both
  old/current callbacks; neither writes cache or records completion.
- `PersistedCompletion`: completed accounts have authoritative canonical evidence.
  Mechanism: FanHub endpoint and the existing server completion receipt; contract
  tests check the path/Party ID, actual isolated negative and positive runtime
  receipts establish the namespace distinction.
- `SuccessfulFollowQualifies`: an applied successful follow can satisfy the
  authoritative first action. The wrong-namespace negative control violates this
  property in31generated/28distinct states, matching the real native counterexample.
- `RequestsSettle`: pending commands eventually settle under weak fairness of a
  server return (failure or persistence, then response). No availability guarantee
  under indefinite disconnection is implied.

Positive configuration:592generated/328distinct states, depth9. Session-check
mutant:171generated/116distinct states violates `CurrentSession`. Full pinned
TLC/Alloy runner exits0. Terminal bounded traces permit stuttering, so deadlock
checking is disabled explicitly; temporal settlement remains checked. An initial
model attempt incorrectly constrained the invalid context transition instead of
recording it. The required negative control failed to detect it, revealing this
model error; parenthesizing the state assignment fixed the model and the negative
control now demonstrates the expected counterexample. No failed attempt is counted
as verification.

Limits: this model abstracts the follow receipt and successful completion handshake
as one transition. It assumes the existing backend authority/atomic persistence
contract; it does not independently prove PostgreSQL, Axios dispatch, React scheduling,
revocation during a server transaction, arbitrary retries, all native actions or
physical OAuth. Separate existing completion-helper tests cover network/storage
failure and retries; no production consequential action was performed. A render
must observe the session boundary; changes entirely outside the provider contract
are outside this model. Source/session invariants do not certify usability.

## Release boundary

Previous60fccd5 actual iOS production-Hermes-in-EAS-shell proof remains valid for its
source, not relabeled as final09649d6 native qualification. Android QA build10d5b607
uses sourceb72174c atop09649d6, separate package`com.tdf.records.uxaudit`, a local
isolated backend and QA signing. This is not a store artifact or OAuth/FCM/Play
identity qualification. Its physical execution is pending after the Samsung was
disconnected at09:36UTC. Existing personal Play installation is preserved. Root
reference must advance only to published, qualified compatible mobile source.
Physical iOS Google and store review/publication remain separate open gates.
