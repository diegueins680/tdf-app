# Staff guide

Open **Dashboard → Operations center** (`/dashboard/operations`) on web or `/operations` in the staff mobile app.

The KPI cards summarize persisted data. Kanban and inbox share the same server filters; neither falls back to demo data. Use search or filters, then save the view for yourself or share it if your role permits. The blue unread marker is organization-shared: the first authorized person to open an item records who saw it and when.

Open a work item to review its source, history, notes, SLA, assignee, and allowed transitions. Assignment and operational status do not approve, pay, issue, cancel, or otherwise alter the source. Use **Open complete record** for the separately authorized business command, then return to record the operational outcome.

Lifecycle guidance:

- **New / Seen**: triage and establish responsibility.
- **Assigned / In progress**: an owner is actively investigating or executing the separate business action.
- **Waiting**: provide a reason and classify whether an external dependency exists. Only an explicit external dependency pauses SLA; add a resumption date when known.
- **Resolved**: the operational follow-up is complete; provide a reason. This does not assert the source entity is paid/approved/completed.
- **Archived**: hide resolved work from active views without deleting history. Resolved items auto-archive after 90 days and remain searchable.

Priority overrides require a Manager/Admin role and an audit reason. Bulk consequential actions show an impact confirmation and still execute as individually authorized, versioned server commands. Refresh a `409` conflict before retrying.

Refunds, reversals/voids, financial chargeback outcomes, paid or near-term cancellation, credit/debit notes, issued-document changes, privacy erasure, and configured high-value actions create an approval request. Another authorized person must decide it; self-approval is impossible.

Mobile notifications contain no customer or financial detail. Opening the deep link fetches the item again under current authorization. If access was removed, the item remains unavailable even if the OS notification is still visible.

If a provider fails, Managers/Admins use the integration failure queue, inspect the redacted code, fix configuration, and replay with a reason. Never paste a token, certificate, raw payload, card value, or customer secret into a note.
