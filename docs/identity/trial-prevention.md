# Trial enquiry and student identity

Public signup enquiries and trial requests create unverified contacts for one source operation. Email, names, and phone numbers never select an existing person. Trial requests do not create login credentials, send generated passwords, or grant account access. Existing linked records are preserved.

The public signup, public trial, and authorized student-create endpoints require an Idempotency-Key. PostgreSQL transaction locks serialize each server-controlled scope/key. A private receipt stores the accepted input, result, actor where applicable, and dependency references. Identical retries return that result; edited accepted requests return a conflict. Contact, lead/trial, student policy assignment, and receipt writes commit together. A failed dependency leaves no orphan contact. Unsupported signup credentials are rejected before receipt persistence.

Student creation checks school access before replay. Its scope includes the authenticated actor, and the existing automatic student policy applies only to the newly created contact. It cannot grant rights to an existing person through a shared email. Actor and contact references reject archived identities. Separate actors and separate operations can legitimately share contact details.

Web forms retain their key after failed responses and edits, reset after known success, and keep student dialogs open while a request is pending. Old clients without a key receive a refresh instruction. No current native screen calls these endpoints; generated contracts are synchronized separately.

Migration 111 adds the access-controlled receipt table. Empty-table rollback is supported; rollback refuses to discard accepted receipts. For an application regression, use a forward fix or temporarily close the affected entry point while retaining receipts and historical references. Do not restore email-based identity adoption. Individual identity consolidation remains governed by the reconciliation review and conflict-aware undo workflow.

Validation uses scripts/test-trial-identity.sh with an isolated PostgreSQL database: simultaneous signup retries, legitimate shared contact details, changed accepted input, missing keys, unsupported credentials, trial replay after availability changes, actor scoping, authorization before replay, student policy provenance, and dependent-write rollback. The runner also checks invalid scope rejection and empty/used migration rollback. Rendered trial-page tests cover retry key retention after failure and edits, pending dismissal, and reset after success.
# Ad inquiries and shared student details

Ad inquiries now require a 16–128 character ASCII request key (letters, digits,
hyphens or underscores) in `Idempotency-Key`. The client must keep that key after
an uncertain response or edit and replace it only for a deliberate new inquiry.
The handwritten `Ads.submit` client requires the caller to supply it; there is
currently no bundled form calling that method. Existing external clients missing
the key receive 400 before any records or replies are created.

Migration 112 stores an operator-only receipt for the contact and lead, with the
accepted payload, response and notification state. Shared details never select
an existing account or grant student access. Concurrent retries serialize in one
transaction; changed accepted payloads return 409. The receipt reserves external
reply dispatch once, before sending. Retries never dispatch again. `dispatching`
after a process interruption and `review` after failed or uncertain delivery need
an authorized operator to inspect the existing email/WhatsApp delivery history;
do not blindly resend or delete the receipt. A successful reply updates the
stored response. Routine retries preserve the contact and lead even if delivery
is unresolved. Used receipts block schema rollback.

Student updates permit an unchanged shared email. Changing it to another person's
existing email still follows the existing conflict policy; authorization checks
continue to run before updating the student.

The final trial release also includes commerce prevention (#436) so service
bookings cannot adopt an account by email. The unused legacy account-provisioning
helper has been removed. This combination closes the older primary-email lookup
interaction identified in review. The new ad and student-edit regressions must
pass hosted PostgreSQL tests before release.
