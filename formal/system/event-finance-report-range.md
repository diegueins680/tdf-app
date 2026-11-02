# Exact event finance and budget reporting — PAY-EVENT-REPORT-RANGE-001

Authority: the owner's financial-integrity mission; existing event amount DTOs and
Persistent models use bounded host `Int`, and the existing canonical ticket
revenue path rejects amounts beyond the supported reporting range with HTTP500.
This contract extends exact reporting to the remaining aggregates; it introduces
no currency-conversion policy, smaller business amount limit, ledger or charge.

For the rows selected by the existing filters, all sums and differences are
mathematical integers. Every monetary field in a successful event finance summary
or budget-line response must equal its exact expression and fit host `Int`.
Overflow must return HTTP500 rather than a wrapped amount. A budget-line update
whose actual amount cannot be reported must be refused before its first write.
Supported negative net, liability and budget-variance values remain valid.

The finance summary checks all sixteen mandatory money fields and both optional
budget/variance fields. The existing posted/draft/void, direction, source,
canonical-order exclusion and explicit-versus-fallback budget rules remain.
Percentage calculation retains the existing positive-budget rule. A range check
never clamps, saturates or silently discards a row.

Assumptions: the native target's `Int` bounds; existing stored dimension/metadata
validation and source authorization; trusted GHC, Persistent and database reads.
This does not establish that every type-representable extreme can be produced by
every production workflow. Tests must state SQLite versus PostgreSQL scope.

The existing multiple reads do not acquire a new serializable financial snapshot.
The prewrite budget check concerns its observed entries; no stronger concurrent
snapshot guarantee is claimed. Production schema reachability, JavaScript's
53-bit integer representation, mixed-currency historical data, settlement,
provider finality and changes to stored records remain separate obligations.

Verification obligations: real-handler tests must distinguish an assertion failure
from a fixture or compile error and cover overflow, supported boundaries, negative
balances, existing source/status filters and refusal-before-write. Successful
finite executions provide TESTED evidence only; complete implementation and
refinement proofs remain UNKNOWN. Exact execution/source receipts are recorded
separately for each qualified commit.
