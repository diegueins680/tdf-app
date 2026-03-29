# Meta App Review Screencast Guide (Instagram Messaging)

Use this flow to record App Review evidence for:
- `instagram_basic`
- `instagram_manage_messages`
- `instagram_business_basic`
- `instagram_business_manage_messages`

## Permission Mapping In TDF

1. Facebook Login run (`VITE_INSTAGRAM_OAUTH_PROVIDER=facebook`):
   - `instagram_basic`
   - `instagram_manage_messages`
   - plus `pages_show_list,pages_read_engagement` for asset discovery in `/instagram/oauth/exchange`.
2. Instagram Login run (`VITE_INSTAGRAM_OAUTH_PROVIDER=instagram`):
   - `instagram_business_basic`
   - `instagram_business_manage_messages`

If Meta asks for evidence for all four permissions, provide both runs or one combined recording where you switch provider config and reconnect.

## Meta Requirements Checklist (As Of March 27, 2026)

1. The connected Instagram account shown during review must be clearly identified as a professional/business account. In the Facebook Login flow it must also be linked to a Facebook Page, and that selected Page/account pairing should remain visible on `/social/instagram?review=1`.
2. Messaging send endpoint constraints apply:
   - Recipient must have messaged first within the last 7 days.
   - Supported payload for this flow is text/link replies.
3. For `instagram_manage_messages` and `instagram_business_manage_messages`, reviewers now expect message lifecycle sync, not only successful send:
   - open inbound thread,
   - send reply from TDF HQ,
   - show the delivered message in the native Instagram client,
   - delete or unsend that same message in the native Instagram client,
   - return to TDF HQ and show the deleted-message refresh without a manual page reload.
4. `instagram_manage_messages` / `instagram_business_manage_messages` typically require **Advanced Access** for production use (non-owned/non-role accounts).
5. In development/review environments, recipient/test accounts must have proper app role access (admin/developer/tester) when required by Meta.

## Live Validated Asset Baseline

Use this exact asset in the refreshed recording unless Meta review credentials force a different approved asset:
- Facebook Page: `TDF Studio`
- Page ID: `169846481310276`
- Instagram account: `@tdf.records.label`
- Instagram User ID: `17841445628242005`

This asset identity was recovered from the prior verified Facebook Login artifact and revalidated on the live deployed review lane on 2026-03-27.

## Env Profiles

Use one of these frontend profiles before recording:

```env
# Profile A (Facebook Login)
VITE_INSTAGRAM_OAUTH_PROVIDER=facebook
VITE_INSTAGRAM_SCOPES=instagram_basic,instagram_manage_messages,pages_show_list,pages_read_engagement
```

```env
# Profile B (Instagram Login)
VITE_INSTAGRAM_OAUTH_PROVIDER=instagram
VITE_INSTAGRAM_SCOPES=instagram_business_basic,instagram_business_manage_messages
```

## Recording Routes

1. Setup screen (Step 1): `/social/instagram?review=1`
2. Messaging flow (Step 2): `/social/inbox?review=1`

## Exact Recording Script

1. Start recording before clicking **Connect with Meta Login**.
2. Show the full Meta auth + consent dialog with the permission grant visible.
3. Return to `/social/instagram?review=1` and keep these items visible:
   - Provider + requested scope chips
   - Connected status
   - Selected professional/business messaging asset with Page ID / IG User ID
4. Narrate explicitly that the selected Instagram account is the professional/business messaging account being reviewed. In the Facebook Login run, also state that it is linked to the visible Facebook Page.
5. Click **Continue to message send flow**.
6. In `/social/inbox?review=1`, keep these items visible:
   - Checklist panel
   - Active selected asset summary
7. Open an inbound conversation.
8. In the reply dialog, show:
   - Sender + inbound content
   - Outgoing field + **Send message** button
9. Send a clear test sentence.
10. Keep the success status visible in the app UI.
11. Open the native Instagram client and show the same delivered message in-thread.
12. In the native Instagram client, delete or unsend that same message.
13. Return to TDF HQ and keep the checklist panel visible until the inbox auto-refresh reflects the deletion/unsend without a manual page reload.
14. If the refresh takes an extra cycle, wait and keep the same thread visible. Do not cut away before the deleted-message refresh is shown.

## Live-Proven Sequence

This exact behavior is already validated on the live system and should be mirrored in the refreshed final cut:
- the selected asset above is visible on `/social/instagram?review=1`
- a live reply from TDF HQ succeeds on the review Instagram thread
- the same delivered message is visible in the native Instagram thread
- that same message is deleted or unsent in Instagram
- the corresponding row disappears from `/social/inbox?review=1` after the built-in 5-second review refresh window

Treat that sequence as the canonical proof target for both provider runs.

## March 28 Refresh Gate For Instagram Login Replacement

Before swapping any packet reference away from `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`, capture one refreshed Instagram Login run and treat this as the replacement gate:

- Replacement candidate path: `screencast/meta-app-review/output/final-instagram-business-login-2026-03-28.mp4`
- The candidate cut must visibly show, in order:
  1. `/social/instagram?review=1` with the connected professional/business account clearly identified on screen
  2. the selected asset summary for the approved messaging asset
  3. an outbound send from TDF HQ
  4. the same message deleted or unsent in the native Instagram client
  5. the refreshed TDF HQ inbox state showing the delete/unsend result without a manual page reload
- If any one of those five proof points is missing, keep the historical packet reference in place and report the first missing scene or live blocker verbatim.

## Canonical Submission Artifacts

Use these files as the primary approval packet:

- Reviewer notes: `screencast/meta-app-review/submission-notes.txt`
- Evidence manifest: `screencast/meta-app-review/evidence-manifest.json`
- Packet summary: `docs/meta-app-review-submission-packet-2026-03-26.md`
- Facebook Login evidence: `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`
- Instagram Login evidence: `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`

These two videos cover all four requested permissions only after the reviewer-visible business-account wording, live consent step, and delete/unsend evidence are present in the refreshed final cut.

## Submission Notes Template

Use or adapt this for App Review notes:

`This screencast demonstrates end-to-end Instagram messaging from TDF HQ using the professional/business Instagram account selected on the setup screen. The Facebook Login run shows that the selected Instagram account is linked to the visible Facebook Page before messaging begins.`

`The messaging portion shows inbound thread selection, a live reply from the TDF HQ app UI, the exact delivered message in the native Instagram client, deletion or unsend of that same message in the native client, and the TDF HQ inbox auto-refresh reflecting the deleted message without a manual page reload.`

`For permissions: Facebook Login run covers instagram_basic + instagram_manage_messages; Instagram Login run covers instagram_business_basic + instagram_business_manage_messages.`

`If delivery fails during review, verify Advanced Access status, app-role/tester access, recipient 7-day messaging window, and that the selected asset is the same professional/business messaging account connected in Meta.`
