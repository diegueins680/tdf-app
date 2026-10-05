# Account deletion: authenticated initiation and manual fulfilment

The owner confirmed that `info@tdfrecords.net` handles account/data-deletion
requests. `/cuenta/eliminar` gives the account holder a dedicated, bilingual
initiation flow without composing an email or explaining a reason. The mobile
About → Support and legal → Data Deletion link reaches this action through the
existing legal page. Both canonical and old legal URLs remain supported.

The form shows the current account, requires an explicit confirmation, reloads
the live cookie session before submission and refuses a missing/different
account. It sends the request through the existing feedback API using that
cookie, without a potentially stale bearer-token override. No credentials,
attachments, diagnostic logs or analytics events are added. A successful
response means **request received**, never **account deleted**. Processing is
manual with the already stated target of 30 days and a completion confirmation.

## Operator procedure

1. Find the `account_deletion_request` in the existing internal feedback queue
   (the normal feedback notification also reaches the confirmed inbox).
2. Verify the authoritative database/queue `feedbackCreatedBy` is present and
   matches `requested_account_party_id`. The feedback endpoint also accepts
   anonymous feedback: a body, email, title or claimed ID alone is **not**
   authority to delete an account. Reject mismatched requests for fulfilment;
   ask the account holder to use the authenticated flow again if needed.
3. Verify the account contact against TDF's account records before sending any
   personal information. Handle legal/fiscal/security/dispute retention
   separately and explain the actual retained records to the account holder.
4. Process the entire account and associated personal data, including
   user-generated content. Revocation/deactivation alone is not completion.
   Revoke sessions and linked service access as part of the existing owner-run
   deletion process. Do not delete shared financial records indiscriminately.
5. Confirm completion through the verified account contact, and record the
   processing result in the internal queue. Do not expose the request, identity
   or result to other users or product analytics.

This change does not automate erasure or prove a production account has been
deleted. No real account-deletion request is submitted for QA. Tests use
synthetic identities and intercepted requests. Actual fulfilment remains the
owner's confirmed operational responsibility.

Apple allows manual processing with a clear timeframe and completion notice,
but requires initiation without a mandatory support email for this kind of app.
See [Apple's account-deletion guidance](https://developer.apple.com/help/app-review/guideline-reference/5-1-1-account-deletion).
The separate App Store 2.1/4.8 rejection and physical-iPhone authentication gate
remain open; this implementation is not a claim of App Review approval.
