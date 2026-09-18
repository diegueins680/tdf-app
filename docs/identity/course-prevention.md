# Course registration contact identity

When payable course checkout is disabled, the registration fallback now requires the same caller request key as the canonical checkout. It creates the enquiry contact, registration, follow-up, and private receipt in one PostgreSQL transaction. Email and phone do not select an existing person or pending registration. Distinct source operations can use shared contact details.

The receipt binds the complete payload and source namespace to the registration. Concurrent retries serialize, return the existing registration, and do not repeat contacts, follow-ups, or confirmation attempts. Changed accepted payloads return 409. Public requests and WhatsApp events have separate server-controlled namespaces; WhatsApp uses the persisted inbound message ID as its operation key. The web form keeps its key when fields change after an uncertain response.

Enquiries no longer create credentials, assign student access, or send generated passwords. Authorized administrative dossier reads that need a contact lock and re-read the registration before creating one, preventing competing lazy links. Existing account links and historic enrollment are preserved.

Migration 110 adds the private receipt table. Apply through the normal immutable migration lane. Empty-table rollback is supported; once a receipt exists, keep the table and use a forward application fix. Do not return to contact-detail-based identity selection as routine rollback.

Validation: `sh scripts/test-course-identity.sh` creates a disposable production-shaped schema and exercises shared details, simultaneous retries, source namespace isolation, changed payloads, dependent-write rollback, and receipt-preserving schema rollback. Email is disabled in that test; it checks confirmation attempt records, not live delivery. The web regression covers editing a failed submission while retaining the original key. Staging and deployment must still be verified before claiming active prevention.
