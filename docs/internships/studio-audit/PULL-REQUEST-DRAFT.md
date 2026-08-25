# Proposed draft pull-request description

## Title

`feat(internships): add traceable studio-management audit workflow`

## Summary

Adds an explicit draft/activation lifecycle for the studio-management internship audit, normalized test plans/cases/executions and calculated completion controls, and an authenticated private reporting/triage workflow that extends the existing feedback system while preserving public feedback compatibility.

## Included

- Evidence-backed studio feature inventory and 174 deterministic Spanish test cases.
- Two-week Spanish Stewart guide, principal assignment content, daily/final summary model, and inactive draft preview.
- Reversible PostgreSQL migration and legacy feedback compatibility.
- Backend authorization for assigned intern, other intern, and authorized admin/manager behavior.
- Internal report drafts, structured context, multiple evidence, comments, information requests, duplicates, retests, audit history, search/filter/export, and GitHub linkage field.
- Responsive intern test-plan/report surfaces and administrator dashboard.
- In-app notifications, durable test-safe notification outbox, documented rules, and an explicit production-dispatcher deployment gate.
- Deterministic fictional fixture catalog and staging/deployment/security runbooks.
- OpenAPI/generated-client, registry, automated test, and verification updates (list exact generated files after final checks).
- CI coverage for deterministic artifact drift, inventory/schema unit checks, the reversible migration rehearsal, existing API-client drift checks, and the mocked Playwright journey.

## Security and privacy

Intern reports are private by reporter; backend authorization protects reports and evidence. The intern cannot set authoritative severity/priority, broaden roles, activate, close, or see other users' private reports. Attachments are validated and privately stored; heavy video is linked. Production mutation, real recipients, live payments, secrets, publication, inventory mutation, and music delivery are prohibited.

## Migration and rollback

Apply the registered base migration `2026-08-21_studio_internship_audit.sql`, completion-exception control, and historical-failure gate in manifest order; they are rerunnable and leave legacy feedback untouched. After stopping new writes and exporting retained audit records, roll back the historical gate, then the guarded completion-exception migration, then the paired base rollback. Release preflight must verify manifest checksums and ancestry.

## Test plan

Use the exact command/result table from `docs/internships/studio-audit/TEST-RESULTS.md`. Backend, API lifecycle, targeted web E2E/accessibility, OpenAPI, registry, type checking, migration, mobile, isolated staging health/CORS, and inactive-draft authorization checks pass. The global web bundle budget, global lint baseline, feature-audit pending-disposition gate, and missing provider credentials/dispatcher remain visible.

## Deployment gates

- Supply Stewart Moreira's verified production party ID and email only at approved draft-creation runtime; do not commit those identifiers or create a duplicate account.
- Review/approve least-privilege module grants and notification recipients.
- Connect and verify an approved external outbox dispatcher/digest schedule, or explicitly approve in-app-only notifications.
- Review the integrated `TDF-mobile` generated-client commit `dfb4bc5b64f957e93d5deab705aa971d20268f62` (mobile PRs #30, #31, and #32); the parent repository points to that exact remote commit.
- Review the deployed isolated staging topology, synthetic data, and test transports.
- Review the staging inactive draft; the complete intern/admin lifecycle and rollback have disposable local automation, while physical-device and external-provider staging checks remain manual.
- Obtain separate approval for activation/notification.

## Draft-task preview

Link `docs/internships/studio-audit/DRAFT-PREVIEW.md`, the Spanish guide, inventory summary, case counts, schedule, permission proposal, notification behavior, and completion gates.

## External actions

Branch creation, commits, pushes, PR creation, isolated staging deployment, and inactive staging draft creation were authorized. The API/web/database are deployed and the staging draft remains hidden and unassigned with zero assignment notifications. No production deployment, GitHub issue, task activation, assignment to Stewart, or real notification is authorized. Confirm each remaining gate separately.
