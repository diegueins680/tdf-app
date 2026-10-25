# Startup rejects ledger lookup drift

Qualified clean source: `e841d77b0eac5631d71a93bbd5ecebb4dec1d3ee`.
Base: combined financial draft `f9ee2199273471a12cfb95da95bc5503fe51ac79`.
Protected main observed: `000cf8ad3b9dabbd612695c3c107ddde6896c96a`.
This is source qualification, not integration or production evidence.

The original verifier returned success on the complete 220-migration schema
after trusted lookup settings were removed from the ledger trigger functions.
Their migration receipts remained present. The repaired verifier inspects the
installed settings, expected signatures and invoker boundary directly. The
provider-retry fixture also applies the namespace successor.

The actual verifier passes the trusted baseline, rejects ten rollback-scoped
mutations across both functions, and accepts two compatible timeout settings.
Every control restores exact function definitions and migration receipts. The
same regression fails against the frozen original verifier with the named
`startup accepted ... RESET search_path` assertion. The installed ledger guards
also pass their ordinary-role bypass controls after that experiment. All owned
databases were removed and their clusters stopped.

[receipt.json](receipt.json) records commands, current counts, tool versions and
artifact hashes. The PostgreSQL receipt hashes every migration plus the actual
verifier, regression, registry and runner. The included external driver uses
owned fixtures and documents the original-module negative control; adjust its
checkout/output paths and choose fresh fixture names before reproduction.
The canonical CI ledger runner invokes the regression against its full schema.

Repository quality and all 63 production-release tests pass. Fresh assurance
retains eleven conditional source-derived SMT claims, 41 cross-solver matches
and fourteen independently checked CPC obligations, covering twelve distinct
inputs. All 139 complete requirements and 278 implementation/refinement
obligations remain UNPROVEN. `SYS-MONEY-003` remains PARTIAL.

This check does not establish arbitrary function-body or trigger-binding
equivalence, trusted-schema privileges, protection from table owners, complete
SQL/MVCC refinement or deployed correspondence. Backend application sources,
historical migrations and finite models are unchanged. Final-head hosted CI,
protected-main review and deployment approval remain separate gates.
