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
