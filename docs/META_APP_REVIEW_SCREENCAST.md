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

If Meta asks for evidence for all four permissions, provide both runs (or one combined recording where you switch provider config and reconnect).

## Meta Requirements Checklist (As Of March 3, 2026)

1. The connected Instagram account must be a **professional account** linked to a Facebook Page for Facebook Login-based messaging flows.
2. Messaging send endpoint constraints apply:
   - Recipient must have messaged first within the last 7 days.
   - Supported payload for this flow is text/link replies.
3. `instagram_manage_messages` / `instagram_business_manage_messages` typically require **Advanced Access** for production use (non-owned/non-role accounts).
4. In development/review environments, recipient/test accounts must have proper app role access (admin/developer/tester) when required by Meta.

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
2. Show full Meta auth + consent dialog (permission grant visible).
3. Return to `/social/instagram?review=1` and keep visible:
   - Provider + requested scope chips
   - Connected status
   - Selected messaging asset with Page ID / IG User ID
4. Click **Continue to message send flow**.
5. In `/social/inbox?review=1`, keep visible:
   - Checklist panel
   - Active selected asset summary
6. Open an inbound conversation.
7. In the reply dialog, show:
   - Sender + inbound content
   - Outgoing field + **Send message** button
8. Send a clear test sentence.
9. Keep success status visible in the app UI.
10. Open native client and show the same delivered message in-thread.

## Submission Notes Template

Use/adapt this for App Review notes:

`This screencast demonstrates end-to-end Instagram messaging from TDF HQ: login and explicit permission grant, visible asset selection (Page ID + IG User ID), inbound conversation selection, live reply from app UI, and verification of the exact delivered message in the native client.`

`For permissions: Facebook Login run covers instagram_basic + instagram_manage_messages; Instagram Login run covers instagram_business_basic + instagram_business_manage_messages.`

`If delivery fails during review, verify Advanced Access status, app-role/tester access, recipient 7-day messaging window, and that the selected asset is the same one connected in Meta.`
