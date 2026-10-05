# Meta webhook receipt privacy — PRIV-WEBHOOK-001

Facebook and Instagram webhook handlers must not copy raw provider envelopes,
message text, sender/recipient identifiers or provider metadata into application
receipt logs. A valid signature authenticates the provider; it does not make the
message public. Existing ingestion and authorized message persistence are separate
from operational log collection.

After successful database ingestion, the receipt contains only the fixed channel
name and count of extracted events. No success receipt precedes the database
transaction. Parsing, signature checks, response codes, database retention and
provider activation are unchanged. Historical logs are not deleted or rewritten by
this patch; their access/retention and any needed remediation remain an operational
obligation. This is not a claim that every application, SQL, proxy or provider log
is free of personal data.

The actual HTTP fixture in `scripts/test-booking-conformance.py` signs synthetic
Facebook and Instagram echo messages containing unique private sentinels in message
text and actor/provider IDs. The messages are acknowledged but ignored by ingestion,
so they cannot trigger outbound replies. The fixture checks no message is queued,
a safe receipt exists, and the entire captured backend log lacks the sentinels.
These empirical checks do not prove noninterference for every error path or other
worker. No TLA+ model is claimed for this local log-field omission; broader consent,
retention, message deletion and webhook replay remain separately open.

Primary guidance: [OWASP Logging Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Logging_Cheat_Sheet.html).
Adopt a minimal operational receipt; authenticated customer payloads do not belong
in broad diagnostic logs. Do not remove domain records merely to pass a log test.
