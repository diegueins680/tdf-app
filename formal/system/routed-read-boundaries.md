# Mounted read boundaries

`AUTH-RADIO-001` and `COURSE-SUBJECT-001` in `requirements.json` hold the
normative access/visibility rules. The compiled snapshot covers branch order and
authentication constructors; the HTTP harness checks the actual composed server.
Neither alone establishes complete authorization conformance.

## Reconciled evidence

At clean eadbcb451, a disposable PostgreSQL fixture returned 401 to anonymous and
invalid-token radio presence reads, and 200 to authenticated cross-party reads.
The published OpenAPI inherits bearer security for that operation. Removing the
later public declaration preserves that boundary; reordering it would expose
listener data. The existing cross-party authenticated policy is preserved, with
no invented social-relationship requirement.

The same fixture returned only the active subject even for an administrator's
`/trials/v1/subjects?includeInactive=true`. The earlier public route wins before
the existing private handler and its `ensureSchoolAccess` guard. A distinct
`/trials/v1/subjects/catalog` path makes that management behavior reachable.
The public URL continues its active-only behavior. Both operations are explicitly
published and generated into web and exact-pinned Mobile contracts; neither
client gains a new management screen merely through type generation.

The private catalog keeps the existing guard: Scheduling admission AND one of
the school roles named in the requirement. Administrative subject mutation
permissions are separate and unchanged by this read contract.

## Executable evidence and limits

`scripts/test-booking-conformance.py` starts the real backend on an owned nonce
PostgreSQL database, seeds synthetic active/inactive subjects and radio presence,
and checks anonymous/invalid sessions, each canonical role, default/filter behavior
and malformed queries. Its other booking and retrieval assertions remain active.
The runner records exact Git revision, executable hash and database cleanup.
The exploratory eadb receipt establishes the old defect, not the repaired result.

These are read-only runtime checks. They do not prove universal session revocation,
linearizability of a revocation racing an admitted read, complete radio consent,
all school resources, private profile policy, or codec/schema equivalence across
the whole API. No new formal refinement claim is made.
