# Meta App Review Current Packet

Updated: 2026-04-14 America/Guayaquil

## Owner Direction
Keep tdf-label focused on the Meta permissions lane until the current App Review submission is reworked and resubmitted with the active evidence set.

## Live Submission
- App ID: `1098715965613487`
- Business ID: `171424997819091`
- Requests URL: https://developers.facebook.com/apps/1098715965613487/app-review/submissions/?business_id=171424997819091
- Active detail URL: https://developers.facebook.com/apps/1098715965613487/app-review/submissions/feedback/?submission_id=1166713985480351&business_id=171424997819091
- Active submission id: `1166713985480351`
- Historical submission ids: `1148115637340186`

## Live Meta State
- Surface heading: `Your request results for April 4, 2026 at 00:38 GMT-5`
- Status: `Submission not approved`
- Permissions not approved: `instagram_business_basic`, `instagram_business_manage_messages`, `instagram_manage_messages`, `instagram_basic`

## Requested Permissions
- `instagram_basic`
- `instagram_manage_messages`
- `instagram_business_basic`
- `instagram_business_manage_messages`

## Packet Text
TDF uses Instagram Direct for customer communication with the business account. Customers send booking and support inquiries to the TDF business Instagram profile, and TDF staff read and respond to those messages from the business side.

These permissions are used only so TDF can identify the connected Instagram business/professional account, read inbound customer messages sent to that business account, and reply to those customer messages from the business side.

## Evidence Artifacts
### primaryVideo
- Path: `/Users/diegosaa/.openclaw/media/inbound/file_2---f9b3a254-c56b-434b-a00f-31ad19b0c4c8.mp4`
- Size: 3279872 bytes
- SHA256: `b55342c2dbe0f3d9bf878b86f18141001e4db16a6edc43f097097d2fa743d77e`
- Purpose: Strongest current evidence artifact. Shows the business profile identity, Business chat context, prior TDF-side reply context, and a realistic booking/support message flow.
- Proof anchors:
  - 00:00:10 -> shows tdf.records.label / TDF Records with category Entrepreneur
  - 00:00:11 -> shows Business chat

### secondaryVideo
- Path: `/Users/diegosaa/.openclaw/media/inbound/file_0---897b03ee-8915-48b1-94b3-3297b40b05de.mp4`
- Size: 737761 bytes
- SHA256: `e73a6f1f6234c118f9351a51d270564cb3102b535483e2bc74d46fe933c3d7e6`
- Purpose: Supporting evidence for the personal-account-to-business-chat flow.

### supportingStill
- Path: `/Users/diegosaa/.openclaw/media/inbound/file_1---50bbbdeb-66c4-46d5-ae57-004bfb20a67b.jpg`
- Size: 84766 bytes
- SHA256: `84571a56880666e63e9ccd3551126b3b0092056f7e7078a0a12b9272e9343204`
- Purpose: Backup screenshot only. Keep available if Meta asks for an extra still.

## Per-Permission Mapping
### instagram_basic
- Intent: Read the connected Instagram account identity used by the business before the messaging flow starts.
- Note file: `screencast/meta-app-review/permission-notes/instagram_basic.txt`
- Evidence artifacts: `primaryVideo`
- Proof anchors:
  - 00:00:10 -> connected Instagram account identity

### instagram_manage_messages
- Intent: Reply from the TDF business side to customer messages sent to the business account.
- Note file: `screencast/meta-app-review/permission-notes/instagram_manage_messages.txt`
- Evidence artifacts: `primaryVideo`, `secondaryVideo`

### instagram_business_basic
- Intent: Identify and access the connected TDF Instagram business/professional account.
- Note file: `screencast/meta-app-review/permission-notes/instagram_business_basic.txt`
- Evidence artifacts: `primaryVideo`
- Proof anchors:
  - 00:00:10 -> profile shows category Entrepreneur

### instagram_business_manage_messages
- Intent: Read and manage inbound customer messages sent to the TDF business/professional account.
- Note file: `screencast/meta-app-review/permission-notes/instagram_business_manage_messages.txt`
- Evidence artifacts: `primaryVideo`, `secondaryVideo`
- Proof anchors:
  - 00:00:11 -> thread header shows Business chat

## Operator Files
- Combined reviewer notes: `screencast/meta-app-review/submission-notes.txt`
- Screencast guide: `docs/META_APP_REVIEW_SCREENCAST.md`
- Historical context:
  - `docs/meta-app-review-submission-packet-2026-03-26.md`
  - `docs/meta-app-review-war-room-2026-03-27.md`
  - `/Users/diegosaa/.openclaw/workspace/tdf-label-meta-permissions-handoff.md`

## Submission Action
Open https://developers.facebook.com/apps/1098715965613487/app-review/submissions/feedback/?submission_id=1166713985480351&business_id=171424997819091 in an authenticated Meta App Review session, use the files above, paste the matching permission notes, and resubmit only after the active evidence bundle is attached.

