# Meta App Review Screencast Guide

Use this flow to re-record the App Review screencast for:
- `instagram_basic`
- `instagram_manage_messages`
- `instagram_business_basic`
- `instagram_business_manage_messages`

## Prerequisites

1. Frontend env scopes must include at least:
   - `instagram_basic`
   - `instagram_manage_messages`
2. Log in with a test user that can connect a Page with a professional Instagram account.
3. Have a real inbound DM already present in `/social/inbox`.
4. Keep app UI in English by using review mode routes below.

## Recording Routes

1. Instagram setup (Step 1): `/social/instagram?review=1`
2. Message send flow (Step 2): `/social/inbox?review=1`

## Exact Recording Script

1. Start screen recording before clicking **Connect with Meta Login**.
2. Show complete Meta login and permissions grant dialog.
3. Return to `/social/instagram?review=1` and keep visible:
   - Connection status
   - Requested scopes chips
   - Selected messaging asset with Page ID and IG User ID
4. Click **Continue to message send flow**.
5. In `/social/inbox?review=1`, keep visible:
   - Recording checklist panel
   - Selected asset summary
6. Open an inbound conversation.
7. In the dialog, show:
   - Sender and inbound message
   - Reply field and send button
8. Type a clear test sentence in the outgoing message field.
9. Click **Send message**.
10. Keep the success status visible in the app UI.
11. Click **Open native client** (or switch manually), and show the same sent text delivered in Instagram/Messenger/WhatsApp.
12. Return to app (optional) and end recording.

## Submission Notes Template

Use this text in App Review notes (adapt as needed):

`This screencast shows: (1) complete Meta login flow, (2) explicit permission grant, (3) asset selection visible with Page/account IDs, (4) live send action from app UI, and (5) the same delivered message in the native client.`
