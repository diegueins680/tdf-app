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

Apply the registered SQL migration before deployment. Run Stack backend checks and the compiled API inventory check in CI, regenerate both clients and Mobile feature registry, and verify the actual recipient's account. The old generic email-send endpoint returns unsupported rather than reporting a delivery that did not happen.

No real event contract, budget approval, recipient permission or notification is created by the migration. The feature does not publish events, sell tickets, send marketing, charge money or accept a contract for either party.

The companion generated Mobile change is preserved in `event-agreements-mobile.patch`, based on Mobile commit `c3b358caa7660a854ad7ac195cf99169638bf308`. The current GitHub connection returned 403 for writes to that repository. Apply/review the patch there, publish its commit and PR, update the root gitlink, then regenerate specification evidence. The current pinned Mobile feature mirror is deliberately not claimed as synchronized; its feature audit remains blocked. Do not weaken that gate or pin an unavailable commit.
