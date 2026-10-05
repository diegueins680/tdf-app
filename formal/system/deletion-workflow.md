# Deletion operations — PRIV-DELETE-001

The product owner requested discovery or creation of a maintained deletion
workflow on2026-10-05. The approved scope here is an executable operator evidence
ledger and runbook, not an automatic account-deletion endpoint. Public commitments
remain in the shipped HTML pages; the canonical machine-readable lifecycle is
[request-workflow.json](../../ops/privacy/request-workflow.json). Operational
procedures and explicit activation gaps are in [the runbook](../../ops/privacy/README.md).
Historical release/privacy sign-offs cannot establish current completion.

A case starts at the earliest actual received timestamp, with a30-day deadline.
Identity clarification and execution failure do not reset it. Canonical scope
requires a disposition and proof reference for every policy data surface, including
backups and external providers. Retention is explicit and has a review deadline;
closed does not mean every record was erased. Case-bound retention review can
record later erasure/anonymization or extend a justified review date without
changing other dispositions, the original deadline or recorded closure time. Closure requires verified planned
effects followed by an evidenced delivered notice. Failure remains nonterminal.
Withdrawal is permitted only before execution and after identity verification.

The local OS account is the authorization boundary. It owns a private0700 parent
and0600 ledger/receipt files outside the source checkout. SQLite BEGIN IMMEDIATE
serializes writers; expected versions reject stale mutations; retry keys bind the
actor, action, parameters and receipt digest. A successful replay returns its
original event result without another write. Stored state and events contain
opaque case IDs and proof hashes, not customer names, emails or request bodies.
The private proof store must retain the actual evidence and original mailbox
mapping. Hashes detect correspondence, not truth or legal adequacy. Host/root
compromise, shared-account impersonation and privileged history rewriting are
outside this boundary.

The case ledger is distinct from the production PostgreSQL application database.
No generic customer DELETE operation is provided. Each actual deletion requires
verified identity, current entity/ownership mapping, service-specific execution,
provider outcome reconciliation and independent effect checks. Intake routing,
operator assignment, daily monitoring, private storage/backup activation and
production fulfillment are not established by the code or synthetic tests. The
current public promise is therefore still PARTIAL, with operational activation
and actual completion evidence outstanding. No customer deletion or outbound
message occurred in this audit.

## Bounded model and negative controls

`PrivacyDeletionWorkflow.tla` models one case with six forward lifecycle stages,
boolean evidence flags, deadline2 (unsafe reset3), and a separate abstract commit
counter0..2 with expected versions0..1. Identity, scope and effect evidence are
assumed valid when their model actions occur; delivery is represented by the
close action. No clocks, fairness or liveness assumption is asserted. The commit counter is a separate product abstraction, not a modeled atomic
version-plus-lifecycle transaction. The safety
checks are ClosureEvidence, FixedDeadline and NoStaleCommit. Three controlled
variants permit premature closure, reset the deadline or accept a stale version;
each must fail its intended invariant. This is bounded abstract checking, not
whole-program refinement or proof of actual deletion, receipt authenticity or
mail delivery. Wait/failure/withdrawal, retention reviews, multiple cases, crashes,
SQLite implementation and private filesystem enforcement are outside the model.

Actual implementation tests separately cover forbidden closure, complete scope,
retention review, fixed deadline through identity wait/failure/replan, eight
concurrent writers, duplicate request replay, stale versions, receipt binding,
permissions/symlinks, ordinary tamper detection, policy drift, isolated SQLite
backup restoration and CLI overdue exit status. Evidence/DB fixtures are synthetic
and removed after tests. Fresh exact-candidate results remain necessary.
