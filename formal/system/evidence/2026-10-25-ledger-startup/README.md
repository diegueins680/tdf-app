# Startup rejects ledger lookup drift

Current clean qualification: `f8f3d4d3d1de06c5444be238f8b3ed84ee6a3528`;
see [CI selection receipt](ci-selection-receipt.json).
Original qualification: `e841d77b0eac5631d71a93bbd5ecebb4dec1d3ee`.
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

A subsequent integration check found that the original path selector skipped
backend validation for all nine declared ledger/schema-verifier test paths.
Hosted run `40238252396` confirms `backend-quality: skipped`; that superseded
run also contains cancellation/failure statuses and is not green-CI evidence.
The repaired selector selects backend validation for each path, and its own
change selects all existing CI scopes. The frozen original fails the newly
added named regression while the other selector tests pass.

The current F8 source has its own fresh full-220 PostgreSQL execution, ten
startup drift controls, two compatible cases, sixteen selector tests, nine
old/new path comparisons, repository quality, eleven conditional SMT results,
41 cross-solver matches and fourteen CPC obligations over twelve distinct
inputs. Exact definitions and migration receipts restore and owned fixtures
are cleaned up. Original E841 receipts retain their source identity; the
`ci-selection-*` artifacts bind the new execution. Final-head hosted CI and
review remain pending at qualification time. No complete requirement or
implementation/refinement obligation is promoted.
