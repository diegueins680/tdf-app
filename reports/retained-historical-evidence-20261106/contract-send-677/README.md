# Historical contract-send refusal evidence — 2026-11-06

Historical only. No current implementation, test, approval or deployment qualification follows from this archive.

Original source: `3d49f83783e407366b2dc8691feec3603e8aabbb` on `fix/contract-send-truthfulness-20261030`, PR677. Owner closed it as superseded by PR701 on 2026-11-04: https://github.com/diegueins680/tdf-app/pull/677#issuecomment-6644069101

The old file-backed Handler had no email implementation; its sole runtime correction refused valid sends with501 rather than claiming success. Canonical main `7a41f893ee14b4346907e9d53b65c2aa54506cfb` now stores contracts in the database and imports legacy files. It records delivery before SMTP, returns503 when email is unconfigured, returns502 on transport refusal, and reports sent only after SMTP acceptance. Restoring unconditional501 would regress that reviewed implementation.

`source-snapshot.zip` preserves all21source-affected blobs, including the old ten typed Handler tests, legacy requirement, source/registration/context snapshots and both complete original evidence archives. `manifest.json` records every ZIPmember hash and every inner evidence archive member. The complete original source commit is also retained as this normal merge's second parent. Original names/bytes, counterexamples, failed attempts and qualification limits are unchanged.

Old file-backed tests are valuable historical cases, not claimed exact duplicates of current database Handler coverage. They remain archived without activating obsolete501 behavior or a stale requirement. Native/SMT/provider observations keep their original identities; no new test or provider operation was performed for this documentation reconciliation.

All existing main runtime,191requirements,API/Cabal,244migrations,Mobile pin and135prior archival files remain exact. Existing archive README is untouched. No new branch or tag was created; remote publication and sourcebranch retirement require separate verified parent execution.
