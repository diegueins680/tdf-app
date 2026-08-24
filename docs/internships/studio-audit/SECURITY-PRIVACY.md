# Security and privacy assessment

## Data and environment boundary

All executable cases target a dedicated staging environment with deterministic fictional `AUDIT-2026` data. Production is excluded from mutation, payments, communications, schedules, sessions, inventory, permissions, publication, and distribution. Read-only production observation requires separate explicit authorization.

## Least privilege

The verified production account currently has `Intern`, `Reception`, `Customer`, and `Fan`; no grant was changed during inspection. The assignment itself permanently requires only `Intern` and the Internships module. An authorized administrator must review whether the existing `Reception` grant remains independently justified before activation. Additional audit access is provided only in staging through synthetic role accounts or temporary, scenario-specific permission requests. Only authorized Manager/Admin users approve requests. Stewart cannot grant roles, edit protected task fields, activate the plan, assign work, control authoritative triage, view another reporter's private data, or bypass server authorization.

## Report confidentiality

Reporter queries are party-scoped; unauthorized report and evidence access returns no private detail. Administrators receive all-report access only through existing authorized roles. Reports remain internal until an administrator explicitly links/promotes a confirmed report to GitHub. Legacy public feedback remains administrator-only on the internal surface.

## Evidence handling

Attachments are validated for multipart shape, filename, MIME type, size, safe storage name, report access, and download authorization. The database stores the original display name separately from a generated storage key. Heavy video is an HTTPS link, not a direct upload. Evidence content must be fictional and secret-free. Storage configuration must point to a private staging bucket/directory; public object ACLs are prohibited.

## External providers

Datafast and PayPal use sandbox or contract mocks. Email, WhatsApp, calendar, social, and other integrations use sinks/fakes with an allowlist containing only synthetic recipients. A provider configuration preflight is required before activation. CAPTCHA, provider verification, a real recipient, non-sandbox credentials, or possible real charge is a mandatory stop condition.

## Integrity and abuse controls

Database checks constrain report/test states. Executions and histories preserve retest/audit history. Server-side completion rules prevent a UI or direct API from silently closing incomplete critical work. Duplicate detection is advisory and never destroys or merges the reporter's data. Destructive, high-load, penetration, credential, or privilege-escalation testing is prohibited.

## Residual risks and gates

- The requested “Stuart” was resolved read-only to the existing active account for Stewart Moreira on 2026-08-23. Exact production identifiers remain runtime-only and must be revalidated immediately before draft association.
- A disposable staging tenant/database and provider-sandbox credentials must be verified externally.
- Storage malware scanning and retention policy depend on deployment infrastructure and require operational confirmation.
- External delivery from the new notification outbox has no dispatcher in the inspected baseline; recipient allowlisting, digest scheduling, retry/idempotency, and monitoring require operational implementation or an explicit in-app-only decision.
- Real notification recipients must not be configured until Diego approves activation.
- Full cross-provider end-to-end execution is a manual deployment gate; repository tests use isolated transports.
