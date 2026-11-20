# Event agreements and plan workspace

From an authenticated event detail, open **Acuerdos y plan**. The organizer selects the artist's verified account, prepares a structured plan and cost breakdown, then saves a private proposal. The artist can read and comment on the plan without receiving editor rights.

The organizer completes all pending scope and legal fields, approves the exact proposal and budget, then sends it. A recipient notification opens the Agreements tab. The recipient reviews the proposal and frozen plan annex, then accepts with legal name and explicit consent, rejects, or requests changes. Accepted evidence remains available as a private PDF. New terms use a new proposal version or a separately accepted amendment.

## UX decisions

| Step | Implemented behavior | Remaining improvement |
|---|---|---|
| Find the workspace | Contextual event action and notification deep link | Verify navigation on the deployed artist account |
| Choose recipient | Canonical active-account selector rather than a numeric ID | Show the saved account display name after reload |
| Quote production | Exact cents, explicit unknown amounts and unresolved scope | Connect catalog prices and billable-unit quotation |
| Review plan | Read-only artist view and comments; organizer versioned editor | Optional document import and richer schedule controls |
| Review agreement | Version picker, full text, frozen annex and decision history | Structured clause comparison across versions |
| Accept | Explicit name/consent, exact current version/hash, server enforcement | No certified signature provider is integrated |
| Recover a failed operation | Preserve text, stable retry key, reload current truth | Validate long documents and phone layout in production |
| Obtain evidence | Authenticated no-store PDFs; visible generation errors | Verify production TeX runtime and long-page typography |

The plan's attached document is generated from its stored editable content. An agreement and acceptance PDF contain the captured plan revision. No arbitrary external attachment or public share URL is required.

## Rollout boundaries

Apply the registered SQL migration before deployment. Run Stack backend checks and the compiled API inventory check in CI, regenerate both clients and Mobile feature registry, and verify the actual recipient's account. The generic contract email endpoint retains the durable database store and SMTP implementation from #701: 503 without configuration, 502 on transport refusal, and `sent` only after SMTP acceptance with a ContractDelivery audit row. Event agreement `send` instead records the versioned proposal and internal notification atomically; it does not claim email delivery and does not depend on SMTP.

No real event contract, budget approval, recipient permission or notification is created by the migration. The feature does not publish events, sell tickets, send marketing, charge money or accept a contract for either party.

The historical source companion generated Mobile change is preserved in `event-agreements-mobile.patch`, based on Mobile commit `c3b358caa7660a854ad7ac195cf99169638bf308`. That historical GitHub connection returned 403 for writes to that repository. Apply/review the patch there, publish its commit and PR, update the root gitlink, then regenerate specification evidence. That source pinned Mobile feature mirror was deliberately not claimed as synchronized; its feature audit remains blocked. Do not weaken that gate or pin an unavailable commit.

## Historical source qualification — 2026-11-06

The reconciled source incorporates main `c47c2a8d8851a58a828dec830704f8c352496c20`, including #701. Generic contract storage and SMTP handlers are byte-identical to that main. Existing migrations and their review graphs are preserved, and the event agreement migration is appended after the protected 244-entry manifest.

Historical source local checks: 11 real PostgreSQL 16.2 agreement regressions, 72 focused web tests, ESLint on the event API/page/tests, UI typecheck/build and bounded TLC with five negative controls. Catalog reconciliation tests pass (10); source audit observes 1,401 reviewed candidates. Haskell/Stack was unavailable for that historical local execution; its later C5 hosted compiler and tests are recorded separately. The compiled declaration composes the five previously compiler-observed workspace routes with main; this is not a fresh compiler result or runtime PDF qualification.

At that source stage Mobile remained a separate integration blocker. `event-agreements-mobile-20261106.patch` is regenerated against the root's pinned Mobile `14318a730f96b2053f71885091bbc05cffc3294d`; the original companion patch remains preserved. Mobile main has independently advanced to `d5c718c23b50b83dd2dd76bbb41b835730dbf7a2` with the artist video campaign contract. Reconcile the companion with those newer operations and feature entries without deleting them, obtain Mobile review/CI, merge it, then update the root gitlink and regenerate source evidence. A fresh attempt to create the companion branch returned HTTP 403 (`Resource not accessible by integration`). No Mobile commit is published or claimed synchronized.

Tests use synthetic accounts, isolated database schemas/files, fake PDFs and injected transports. No real contract is sent, no historical commercial document is edited, and no production mutation is part of these checks.

## Staged web rollout and current identity guards

The new web workspace is additive. `VITE_EVENT_AGREEMENTS_ENABLED` must be the
exact string `true` to expose its event-detail link or mount its private page.
Absent, false, or differently spelled values keep the current event experience
and make no workspace read/command/download call. This is a public UI rollout
control, not authorization: every enabled request still needs the current
session and event/recipient checks. Enable it only after the existing reviewed
backend and migration rollout has made the workspace available; no new merge
approval or production action is implied by staging the code.

The full existing 246-migration prefix is retained. The original event agreement
SQL is entry247; the following forward migration installs the canonical archived
Party reference guard on all nine real Party FK columns across its six raw-SQL
tables. The 231 model-declared registry and generator remain unchanged; dynamic
actual-FK discovery already protects identity merge refusal. The new guards
prevent new archived references and changed references without rewriting legal
or audit evidence. Replaying the guard forward preserves its protection.
Populated rollback refuses with55000 and retains evidence/guards; an empty
rollback removes only the new triggers, preserving tables, FKs and canonical
functions. No real agreement or production identity is modified by tests.


## Current private reconciliation — 2026-11-07

The current candidate normally incorporates protected main at `6033b84` and preserves
its complete 246-entry migration prefix and current backend behavior. The old
patches and source receipts above remain historical evidence; their provider
403 does not describe current CLI repository permissions. The root-compatible
Mobile companion at `751637c` is generated from the actual composed OpenAPI context
and retains the qualified 0.5.1 URI decoder. It intentionally excludes unrelated
future Mobile-main contracts. Its separate upstream bridge must pass review and
mandatory CI and become reachable from Mobile main before the root's existing
gitlink reachability check can pass; that check remains unchanged.

Local qualification distinguishes execution sources. The Ccea-based repaired
PDF boundary passed the unchanged six-case native harness, including injected
IOException, explicit renderer failure, successful private PDF response and
asynchronous cancellation. Its actual compiler declaration contains 932 typed
operations: the 927 protected operations plus the five original workspace routes.
The final main at `6033b84` composition requires its own compiled declaration inspection
and fresh mandatory hosted tests; earlier native/CI receipts are not relabelled.
No TeX renderer, deployed endpoint, production migration, real document or
external notification was exercised. The default-off rollout remains in force.
