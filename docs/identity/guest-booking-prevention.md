# Guest booking contact identity

Guest contact details do not establish ownership of an existing account. Tentative and payable bookings create an unverified contact for their source operation, without changing an existing account's profile, credentials, roles, or consent. Separate operations can legitimately use shared email addresses.

For payable checkout, contact creation now happens after the existing request-key lock and replay check, in the same PostgreSQL transaction as the booking, service order, checkout, and resource allocations. A retry returns the saved booking. Changed accepted payloads conflict; failed dependent writes roll back the contact too. No payment provider, charge, or notification is invoked by this change.

Validation: the focused Hspec account-isolation example passes. `scripts/test-public-booking-http-concurrency.sh` exercises the candidate API with an isolated synthetic database: equal-key tentative requests, changed payloads, resource conflicts, an existing credential sharing the submitted email, simultaneous payable requests, committed retries, one source contact/order/receipt, and zero payment attempts. It does not send live payments.

No schema migration is needed. Do not restore the previous email-based account adoption behavior as routine rollback; use a forward fix or temporarily disable the affected checkout entry point. Existing records and historical ownership are unchanged.

Marketplace bank-transfer evidence also creates its first guest contact inside the transaction holding the checkout and evidence locks. Later submissions reuse that operation's recorded contact. Equal retries have no additional audit or contact effects; changed evidence under review conflicts; a rejected submission can be corrected without changing identity. Conflicting checkout and evidence identities require review. The isolated PostgreSQL runner `scripts/test-marketplace-contact-identity.sh` covers concurrent submissions, absent payment selection, rejection/correction, audit-write failure rollback, shared email isolation, and zero successful payments.
