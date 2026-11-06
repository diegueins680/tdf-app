# Historical checkpoint with complete startup qualification

Baseline clean source:213f9f661d4f4b9d447c40f8346888f5df5543e3. The saved-search
runner applies220 historical migrations and then invokes current full-schema
readiness. Current readiness correctly rejects the absent later posted-header
guard, before the saved-search target221 and header222 migrations can run.
An owned PostgreSQL16.13 reproduction witnesses exactly220 receipts and neither
target nor header receipt. The database and cluster are removed/stopped.

This is a historical-setup modeling defect, not a reason to relax production
readiness. The repair captures old trigger/rows and exact prefix IDs before the
target checksum block inside one complete production replay. Migration bodies,
checksums, registry order, source receipts and final startup verification remain
unchanged. Existing replay, rollback/reapply, plan, saved-row and HTTP controls
remain. Fatal PostgreSQL diagnostics retain their tail without connection args.

The baseline receipt, raw logs and replay driver are committed. Candidate
qualification must be recorded separately at its exact source; baseline failure
is never a successful current-schema or HTTP result. No deployment or production
database operation is involved.

SQL qualification at clean85c7c4b32906ed6e2e63627e2854a9e78625d115 succeeds
with PostgreSQL16.13/UTF8/en_US.UTF-8, matching CI's documented en_US.utf8
setting. Full222/historical220/header-enabled witness is222|220|O. Original
replay, exact rollback/reapply, saved-row retention, public-query and seven
query-plan comparisons pass. Disabling the header trigger causes the named
current-verifier rejection; rollback restores a passing verifier. The actual
HTTP lane remains separately required and is not claimed from this SQL run.

An independent same-function/two-locale comparison confirms an existing
portability failure: normalize('LÚNA') is 'l na' under C and 'luna' under
en_US.UTF-8. The complete C-locale search test fails and is retained. Supported
locale policy and an additive normalization repair remain open; no case/accent
assertion was removed or weakened, and C is not certified. The earlier locale
probe preparation failure is also retained separately.

No production schema verifier or SQL migration body changed. Native HTTP
compilation and exact final-head hosted checks/review remain pending.
