# Meta App Review Current Packet

Updated: 2026-04-18 America/Guayaquil

## Owner Direction
Keep tdf-label focused on the Meta permissions lane until the current App Review submission is reworked and resubmitted with the active evidence set.

## Release Gate
- Canonical reviewer-proof replacement screencast path: `/Users/diegosaa/GitHub/tdf-app/screencast/meta-app-review/output/final-meta-review-combined-2026-04-18.mp4`.
- This path is frozen in this file and in `screencast/meta-app-review/submission-packet.current.json`; use it as the single reviewer-facing video for the next live Meta action.
- The previous inbound carry-forward videos are retired from the packet of record and remain historical context only.

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
- Path: `/Users/diegosaa/GitHub/tdf-app/screencast/meta-app-review/output/final-meta-review-combined-2026-04-18.mp4`
- Size: 11514833 bytes
- SHA256: `7ef9113729e5c1d66897b057d30c38edc9d4ffd64c3496b803aaf8ddd3f4a578`
- Duration: 301.233333 seconds
- Resolution: 1920x1080
- Purpose: Canonical combined reviewer proof. The first segment is the manifest-backed Facebook Login run for `instagram_basic` and `instagram_manage_messages`; the second segment is the manifest-backed Instagram Login run for `instagram_business_basic` and `instagram_business_manage_messages`.
- Source videos:
  - `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`
  - `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`
- Proof anchors:
  - 00:00:00 -> Facebook Login run begins; covers setup and selected account context for `instagram_basic` and `instagram_manage_messages`.
  - 00:02:59 -> Instagram Login run begins; covers business/professional account context for `instagram_business_basic` and `instagram_business_manage_messages`.

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
  - 00:00:00 -> Facebook Login run shows connected account setup and identity context.

### instagram_manage_messages
- Intent: Reply from the TDF business side to customer messages sent to the business account.
- Note file: `screencast/meta-app-review/permission-notes/instagram_manage_messages.txt`
- Evidence artifacts: `primaryVideo`
- Proof anchors:
  - 00:00:00 -> Facebook Login run covers setup, inbox, reply, delivery, and cleanup flow.

### instagram_business_basic
- Intent: Identify and access the connected TDF Instagram business/professional account.
- Note file: `screencast/meta-app-review/permission-notes/instagram_business_basic.txt`
- Evidence artifacts: `primaryVideo`
- Proof anchors:
  - 00:02:59 -> Instagram Login run shows professional/business account context.

### instagram_business_manage_messages
- Intent: Read and manage inbound customer messages sent to the TDF business/professional account.
- Note file: `screencast/meta-app-review/permission-notes/instagram_business_manage_messages.txt`
- Evidence artifacts: `primaryVideo`
- Proof anchors:
  - 00:02:59 -> Instagram Login run covers business messaging flow.

## Operator Files
- Combined reviewer notes: `screencast/meta-app-review/submission-notes.txt`
- Screencast guide: `docs/META_APP_REVIEW_SCREENCAST.md`
- Evidence manifest: `screencast/meta-app-review/evidence-manifest.json`
- Historical context:
  - `docs/meta-app-review-submission-packet-2026-03-26.md`
  - `docs/meta-app-review-war-room-2026-03-27.md`
  - `/Users/diegosaa/.openclaw/workspace/tdf-label-meta-permissions-handoff.md`

## Submission Action
- Current execution line: `Yes, request again was already exercised once; same feedback URL remained open; Request again = disabled; Ask a question = enabled; no visible submission-shell/upload/continue/submit/file-input control was exposed in the immediate post-confirmation state.`
- Use the exact feedback URL above in an authenticated Meta App Review session only to discover one downstream shell, draft location, upload/reference field, metadata field, or other exact actionable control.
- No packet mutation is justified before Release surfaces a real downstream shell, draft location, upload field, attachment/reference field, metadata field, or packet contradiction.
- If one of those exact downstream controls does appear, use the frozen canonical screencast `/Users/diegosaa/GitHub/tdf-app/screencast/meta-app-review/output/final-meta-review-combined-2026-04-18.mp4` with the matching permission notes from this packet.
