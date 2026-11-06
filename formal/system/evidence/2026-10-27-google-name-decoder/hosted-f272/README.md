# Completed current-head CI receipt

PR #646 candidate: `985be1ca49908260d57ca608e1faf278115ed10c`.
Actual hosted checkout: `f2723ec0867d8f3a4a6cf003f94db16c26efcd29`.
Both have tree `3734b78af47d20f286d54a0a3c3e7de49b9c6496`; the execution
commit is retained in this evidence branch's ancestry.

[CI run 40446969997](https://github.com/diegueins680/tdf-app/actions/runs/40446969997)
and [backend job 122001043076](https://github.com/diegueins680/tdf-app/actions/runs/40446969997/job/122001043076)
completed successfully. This receipt supplements the original canceled-run
receipt in `../hosted-805/`; it does not change that earlier outcome.

- Real TDF.ServerAuth regression group: 15 examples, zero failures or pending.
- Separate exact-source probe: 19 examples, zero failures or pending; restored
  private-email fallback: 19 examples with eight expected failures.
- Aggregate Hspec: 4,058 examples, zero failures, 21 explicit pending cases.
  Pending aggregate cases are not relabeled executed by this receipt. The
  backend job subsequently completed its configured external runtime runners.
- Actual probe versions: Python 3.11.2, Stack 3.3.1, GHC 9.10.3, Hspec 2.11.17,
  Aeson 2.2.5.0, Text 2.1.3 and Time 1.12.2. Source hashes are in `native.json`.

All 19 required technical contexts in the captured protection configuration
match successful checks from the required application on the exact candidate
head. Independent review event 5662064063 approves that head at
2026-10-27T04:01:27Z. The remaining required `main-integration-admission` context
is PENDING, selecting PR #628. Optional Vercel preview quota failure is not a
required technical context; no upgrade or bypass was performed.

Raw job/workflow/check/status/review/queue/protection snapshots and their hashes
are retained. They are dated observations, not evidence that previous merges
enforced the same rules. A later source, review, protection or queue change needs
fresh merge-readiness validation. This evidence branch preserves the approved
application head and does not grant main admission or authorize deployment.

The narrow default-name property is TESTED. No complete implementation/refinement,
Google HTTP/account-persistence, historical-name remediation or full-system proof
is claimed. Reproduction commands and the retained browser failure/success pair
are in the adjacent receipt and original qualification. All 150 complete
requirements remain UNPROVEN in the retained source-bound assurance report.
