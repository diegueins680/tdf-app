# Persona fixtures and scenarios

This directory contains deterministic, fictional quality fixtures. It is safe source data, not a production account list and not human-research data.

- `personas.json`: 26 identities, roles, context, devices, accessibility needs and expected outcomes. Every email ends in `@persona.test`.
- `scenario-blueprints.json`: 17 epics and 78 principal journeys.
- `scenarios.json`: generated full Given/When/Then and lifecycle specifications; do not hand-edit.
- `docs/persona-testing/execution-results.json`: evidence claims consumed by generation.

Regenerate and validate:

```sh
npm run generate:persona-program
npm run test:persona-program
```

## Safe backend seeding

Use only a disposable local/test database with all external adapters disabled or replaced by fakes. From the repository root, provide a runtime-only password:

```sh
cd tdf-hq
APP_ENV=test \
TDF_ENABLE_SYNTHETIC_PERSONAS=1 \
TDF_SYNTHETIC_PERSONA_FILE=../test/personas/personas.json \
TDF_PERSONA_TEST_PASSWORD='<runtime-secret-at-least-16-characters>' \
SEED_DB=true \
stack run
```

The example illustrates variables; never commit a password. Actual backend startup/seeding follows the normal `tdf-hq` workflow. The seed refuses hosted/production markers, requires an explicit local/development/test environment and explicit opt-in, validates the reserved domain, skips the unauthenticated visitor, uses existing idempotent seed helpers, creates no public directory profile and calls no provider.

The seed is additive for roles and credentials. Persona records are reconciled only through their reserved `@persona.test` email or credential; display names are intentionally not identity keys. Cleanup is therefore a reset/drop of the disposable test database, followed by clearing local inbox/outbox/provider mocks and browser/simulator artifacts. Do not enable it on a shared, staging or production database and do not attempt broad record deletion as cleanup.

## UI registration

`PW-PER-01-AUTH` uses the real login/signup interface with an isolated signup route mock and a reserved fictional account. Integrated registration against the backend must use a disposable database and local verification inbox. Duplicate, invalid, verification, expiry and reset cases remain in the full scenario specification.

## Evidence discipline

Only statuses in the execution vocabulary are allowed. `verified-automated` must include an exact command and evidence link. A passing component slice does not establish an entire payment, notification or database lifecycle. Synthetic reaction hypotheses must never be rewritten as participant feedback.
