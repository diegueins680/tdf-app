# Datadog CI synthetics

The repository uses the **TDF Records** organization on **US1**
(`https://app.datadoghq.com`, action setting `datadog_site: datadoghq.com`).
The organization was verified in the signed-in dashboard on September 14, 2026.

The `Run Datadog Synthetic tests` workflow selects `tag:e2e-tests`:

| Test | Public endpoint | Datadog ID |
| --- | --- | --- |
| TDF production API health contract | `https://api.tdfrecords.net/health` | `r2d-i82-3jy` |
| Legacy Pages web entry (not the canonical www surface) | `https://tdf-app.pages.dev/` | `rv2-x2n-epx` |

The latest inspected root-main run on `fdac8e76523befee1603f49f6c7cf7d00762931b`
executed both tests from São Paulo (AWS) at 2026-10-04 23:58 UTC:
[GitHub run37245608548](https://github.com/diegueins680/tdf-app/actions/runs/37245608548),
[Datadog batch](https://app.datadoghq.com/synthetics/explorer/ci?batchResultId=ff538291-8a58-4f01-bf1f-11b04f9328de).
The logs report two passes and zero critical errors, failures, skips, missing tests or timeouts.
The API request targets the canonical Hetzner host. The web test still requests
`tdf-app.pages.dev`; its success does **not** verify `https://www.tdfrecords.net`,
Google login or authenticated upload. Updating that organization-managed web target
and checking its assertions remains outstanding; no external test was modified by
this documentation repair. Older September batches are historical evidence only.

These tests probe the deployed endpoints, not the candidate PR code. They do not
replace preview/persona, migration, contract or authenticated production checks.

The separate Mobile repository currently rejects its Datadog credentials with
HTTP403. Its old action suppressed critical errors; Mobile #125 and consolidation
#71 make that failure visible. An old green Mobile job is not evidence of executed
synthetics. Correct the Mobile repository's organization/site credentials and rerun
its actual tests; do not copy root credentials into logs or source.

## Credentials and verification

Repository Actions secrets `DD_API_KEY` and `DD_APP_KEY` must belong to this
organization and site. Their existence was verified without reading their values.
Keep application-key permissions limited to the capabilities required to read and
execute these synthetic tests; do not expose keys in logs, source, or PR comments.

After a credential or test-configuration change, run the existing legitimate
push/PR trigger, or rerun the affected job. Confirm that **Run Datadog Synthetic
tests** executes, discovers both IDs, and reports two passes with zero failures,
skips, missing tests, or timeouts. The **Skip Datadog Synthetic tests** fallback is
not evidence of a passing synthetic check. Missing credentials on fork PRs cannot
validate this private integration and must be reported explicitly.

`fail_on_critical_errors` and `fail_on_missing_tests` remain enabled. Do not loosen
assertions, change tags to select fewer tests, or disable either setting to hide
an outage. Diagnose authentication, service availability, and assertion failures
separately. Before enabling recurring schedules or upgrading the trial, confirm
the organization's billing and monitoring policy with its owner.

Reverting repository code does not roll back organization-managed tests or keys.
Review Datadog test history separately; never delete or rotate a credential as an
application rollback step.
