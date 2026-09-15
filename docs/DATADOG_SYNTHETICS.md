# Datadog CI synthetics

The repository uses the **TDF Records** organization on **US1**
(`https://app.datadoghq.com`, action setting `datadog_site: datadoghq.com`).
The organization was verified in the signed-in dashboard on September 14, 2026.

The `Run Datadog Synthetic tests` workflow selects `tag:e2e-tests`:

| Test | Public endpoint | Datadog ID |
| --- | --- | --- |
| TDF production API health contract | `https://tdf-hq.fly.dev/health` | `r2d-i82-3jy` |
| TDF production web entry contract | `https://tdf-app.pages.dev/` | `rv2-x2n-epx` |

Both tests executed successfully from São Paulo (AWS) in
[the verified CI batch](https://app.datadoghq.com/synthetics/explorer/ci?batchResultId=c8fa5520-15b2-40f6-b779-498005f82e5b).
These are production smoke checks, not a substitute for the PR's local/preview
persona, integration, migration, or contract tests. They do not deploy PR code.

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
