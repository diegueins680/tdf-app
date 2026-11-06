# Event proposal and account acceptance

EVT-AGREEMENT-001 implements the requested organizer-approved proposal and recipient account acceptance. An acceptance is an authenticated application action, not a certified electronic signature. Only the recipient accepts; the organizer separately approves the exact proposed document and documented budget cap before sending. No real agreement is approved by this implementation task.

The event owner writes structured versioned plans, with an explicitly selected active user account as read/comment recipient. The account selection never grants event editor or scheduling privileges. A proposal captures the complete current plan as an immutable JSON annex. Changes to the operational plan do not amend an existing proposal or acceptance.

Draft revisions clear approval and require a fresh send. Recipients can see only sent versions, reject or request changes with comments, and accept only the current sent, approved, complete version with exact hash, explicit consent and matching legal name. Accepted documents and audit rows are immutable. Subsequent terms require a separate amendment linked to an accepted agreement.

All writes lock the event before any agreement row. Command replay is bound to actor, event, request UUID and payload digest, after current authorization. The UI rereads current state after replay, preserves failed/stale input and discards responses and private downloads after account changes. Acceptance and withdrawal races have one winner.

Reads and PDFs use the same current-session and record authorization as writes. A former recipient retains its sent agreements and receipts, not a reassigned operational plan. PDFs render stored authorized content literally, with the plan annex, document version, SHA-256, account, name and server date. PDF generation failure is unavailable, not a fabricated receipt. The generic contract send route separately retains #701: durable database storage, a delivery row before transport, 503 when unconfigured, 502 on refusal, and success only after SMTP acceptance. Event agreement `send` means the approved version became available to the recipient with an internal notification; it is not an email-delivery receipt.

Approval requires known integer-cent budget lines, a cap equal to their total and no unresolved contractual or plan fields. The organizer remains responsible for approving the actual scope, legal parties and proposed commercial terms; field checks do not establish legal completeness.

Verification maps the actual SQL migration and UI boundaries to eleven isolated PostgreSQL regressions, focused web tests and Haskell document tests. The finite EventAgreement model explores two document versions, two plan revisions and three actor classes; baseline invariants and five deliberately broken controls are checked. This is not a cryptographic proof or a refinement proof of Haskell/SQL. Local PostgreSQL was 16.2; CI runs 17. Stack compilation, exact deployed schema, recipient identity and production PDF rendering are separate prerequisites.

The production migration must be applied through the reviewed manifest/runbook before enabling the UI in production. Root and Mobile generated API/feature registry changes must land together; no native signing screen is claimed.
