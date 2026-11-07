# Selected CI jobs must actually succeed: SYS-CI-SELECTED-001

A final quality, API-contract or migration gate may accept a skipped dependency only when its selection condition is explicitly false. Path classification must succeed, every declared scope output must be exactly `true` or `false`, and every dependency must have a recognized terminal result. A selected job must report `success`. Failed, cancelled, missing or unknown results always reject admission, including optional jobs that ran. An unselected job that successfully ran is acceptable.

For each dependency j, admission requires `result(j) = success OR (selected(j) = false AND result(j) = skipped)`, together with successful classification and valid selection inputs. Quality selects repository, UI, Mobile and backend jobs from their respective outputs; both browser journeys use UI OR repository. Native Android uses its separately declared workflow-dispatch condition. Contract and migration gates use the corresponding scope output. Required GitHub status names and branch protections are unchanged.

Counterexample: the historical aggregate shell bodies accept `BACKEND_RESULT=skipped`, `CONTRACT_RESULT=skipped` or `MIGRATION_RESULT=skipped` despite the corresponding selection flag being true. The exact old bodies are retained in `scripts/__tests__/fixtures/ci-selected-job-admission-before.json` from protected main2e01bbe1263e8761fa2692bd951f828c01142449. Actual workflow CLI tests reproduce all three false accepts and require their corrected counterparts to fail.

This is a component-contract counterexample with injected dependency results. It does not establish that GitHub reached that skipped state in a historical or current run; the current provider backend job really runs. The repair makes the aggregate boundary reject that state if presented, including after an erroneous job-condition change.

`ci-job-admission.mjs` consumes the serialized `needs` context without printing unrelated outputs. Missing Node, checkout, JSON, flags or dependencies fail the required gate. It needs no installed npm dependencies. The three aggregate jobs check out their tested source and select Node22 explicitly. New dependency or scope names require a reviewed mapping. GitHub documents the direct-dependency result/output shape in its [needs context reference](https://docs.github.com/en/actions/reference/workflows-and-actions/contexts#needs-context).

Assumptions: GitHub reports upstream job results faithfully; the path classifier and workflow selection expressions correspond to intended coverage; GitHub executes the checked-in gate; the runtime and branch protections are trusted. This change does not prove those assumptions or that individual test/proof programs establish their claims.

Verification is TESTED finite component behavior, not mathematical implementation equivalence or an end-to-end GitHub proof. Tests enumerate all64 scope vectors, both native selections and all128 quality success/skip vectors (16,384 combinations), plus768 contract/migration combinations and representative malformed, missing, failed and cancelled inputs. Actual parsed workflow bodies execute the CLI and retain historical false-accept controls. Run `npm run test:ci-pipeline`. Native Android condition wiring and the original persona dependency check remain regression-protected.

Recovery: correct the failed upstream validation or selection contract and obtain fresh revision-bound checks and review. Do not convert a selected validation to an optional skip to pass admission. Deployment authority remains outside this gate.


## Owned fixture termination

A selected fixture must finish cleanup after its assertions. An already terminated child must not be mistaken for a live process solely because its `exitCode` is null: a signal termination is represented by `signalCode`, as documented by [Node child-process exit status](https://nodejs.org/api/child_process.html#subprocessexitcode). The experiment HTTP fixture checks both fields before sending a signal or waiting for another exit event. It still closes its log descriptors and preserves an existing assertion failure. The database-host restriction, feature-flag states, real HTTP/SQL assertions, concurrency checks and aggregate admission conditions are unchanged.

The regression executes this actual finalizer with real child processes. Restoring its exact historical guards reproduces Node exit 13; the correction finishes cleanup and retains the original functional failure when one is injected. This local check does not establish a new whole HTTP/PostgreSQL run, and it does not alter any selected-job failure or skip policy. See `reports/campaign-platform-ci-cleanup-2026-10-30.json`.
