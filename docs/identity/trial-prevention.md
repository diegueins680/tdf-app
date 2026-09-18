# Trial enquiry and student identity

Public signup enquiries and trial requests create unverified contacts for one source operation. Email, names, and phone numbers never select an existing person. Trial requests do not create login credentials, send generated passwords, or grant account access. Existing linked records are preserved.

The public signup, public trial, and authorized student-create endpoints require an Idempotency-Key. PostgreSQL transaction locks serialize each server-controlled scope/key. A private receipt stores the accepted input, result, actor where applicable, and dependency references. Identical retries return that result; edited accepted requests return a conflict. Contact, lead/trial, student policy assignment, and receipt writes commit together. A failed dependency leaves no orphan contact. Unsupported signup credentials are rejected before receipt persistence.

Student creation checks school access before replay. Its scope includes the authenticated actor, and the existing automatic student policy applies only to the newly created contact. It cannot grant rights to an existing person through a shared email. Actor and contact references reject archived identities. Separate actors and separate operations can legitimately share contact details.

Web forms retain their key after failed responses and edits, reset after known success, and keep student dialogs open while a request is pending. Old clients without a key receive a refresh instruction. No current native screen calls these endpoints; generated contracts are synchronized separately.

Migration 111 adds the access-controlled receipt table. Empty-table rollback is supported; rollback refuses to discard accepted receipts. For an application regression, use a forward fix or temporarily close the affected entry point while retaining receipts and historical references. Do not restore email-based identity adoption. Individual identity consolidation remains governed by the reconciliation review and conflict-aware undo workflow.

Validation uses scripts/test-trial-identity.sh with an isolated PostgreSQL database: simultaneous signup retries, legitimate shared contact details, changed accepted input, missing keys, unsupported credentials, trial replay after availability changes, actor scoping, authorization before replay, student policy provenance, and dependent-write rollback. The runner also checks invalid scope rejection and empty/used migration rollback. Rendered trial-page tests cover retry key retention after failure and edits, pending dismissal, and reset after success.
