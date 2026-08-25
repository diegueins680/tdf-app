# Studio-management internship audit dossier

This directory is the engineering dossier for **“Auditoría funcional y de experiencia del manejo del estudio.”** Technical material is in English; the reusable intern guide and the task description are in Spanish.

## Canonical artifacts

- `studio-feature-inventory.csv` and `test/internships/studio-audit/studio-feature-inventory.json`: evidence-backed scope inventory.
- `test-case-index.csv` and `test/internships/studio-audit/test-cases.json`: human and machine-readable test plan.
- `STUART-GUIDE.es.md`: reusable Spanish operating guide.
- `TASK-DESCRIPTION.es.md`: Spanish principal-assignment content.
- `test/internships/studio-audit/draft-project.json`: inactive project/task preview.
- `test/internships/studio-audit/draft-stuart-account.json`: non-sensitive record of the verified existing-account state and least-privilege review gate; production identifiers deliberately remain runtime-only.
- `CURRENT-STATE-ASSESSMENT.md`: verified baseline and gaps.
- `TECHNICAL-DESIGN.md`: data, API, authorization, workflow, and notification design.
- `TEST-PLAN.md`: coverage, execution, evidence, and completion rules.
- `SECURITY-PRIVACY.md`: threat controls and prohibited actions.
- `DEPLOYMENT.md`: migration, staging, activation, and rollback runbook.
- `DRAFT-PREVIEW.md`: formal approval preview and remaining gates.
- `TEST-RESULTS.md`: commands and observed results; never treats an unexecuted check as passing.
- `PULL-REQUEST-DRAFT.md`: proposed draft-PR description. It is not a published PR.

Regenerate the inventory, cases, draft manifest, and indexes with:

```sh
npm run generate:studio-internship-audit
```

The generator is deterministic; review changes to generated files before accepting them.
