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

Apply `2026-08-21_studio_internship_audit.sql`; it is rerunnable and leaves legacy feedback untouched. Roll back with the paired `_rollback.sql` after stopping new writes and exporting any audit records that must be retained. The production migration registry points to the actual authorized feature commit; release preflight must verify ancestry.

## Test plan

Use the exact command/result table from `docs/internships/studio-audit/TEST-RESULTS.md`. Backend, API lifecycle, targeted web E2E/accessibility, OpenAPI, registry, type checking, migration, and mobile checks pass. The global web bundle budget, global lint baseline, and feature-audit pending-disposition gate do not pass and must remain visible. Do not convert external staging/provider checks into passes.

## Deployment gates

- Supply Stewart Moreira's verified production party ID and email only at approved draft-creation runtime; do not commit those identifiers or create a duplicate account.
- Review/approve least-privilege module grants and notification recipients.
- Connect and verify an approved external outbox dispatcher/digest schedule, or explicitly approve in-app-only notifications.
- Review the integrated `TDF-mobile` generated-client commit `c537c729177f92f50c69a47edba5b5dae46afebd` (mobile PR #30); the parent repository points to that exact remote commit.
- Deploy isolated staging with synthetic data and test transports.
- Exercise full intern/admin lifecycle and rollback.
- Create and review inactive TDF App draft.
- Obtain separate approval for activation/notification.

## Draft-task preview

Link `docs/internships/studio-audit/DRAFT-PREVIEW.md`, the Spanish guide, inventory summary, case counts, schedule, permission proposal, notification behavior, and completion gates.

## External actions

Branch creation, commits, pushes, and PR creation were authorized on 2026-08-23. The isolated staging configuration is prepared in-repository only. No staging or production deployment, in-app draft creation, issue, task activation, assignment, or real notification is authorized. Confirm each remaining gate separately.
