# Meta App Review Submission Packet - 2026-03-26

## Goal
Get Meta approval for the Instagram messaging permissions used by TDF HQ with the smallest possible reviewer ambiguity.

## Live validated baseline on 2026-03-27
The reviewed asset and message lifecycle are validated on the live TDF lane:
- Selected Facebook Page: `TDF Studio`
- Page ID: `169846481310276`
- Selected Instagram account: `@tdf.records.label`
- Instagram User ID: `17841445628242005`
- Live send from TDF HQ app UI succeeded on a real Instagram review thread.
- Real Instagram delete or unsend then removed that thread from `/social/inbox?review=1` after the review-mode auto-refresh window, without manual reload.

This means the remaining work is evidence packaging and control-plane submission, not unresolved product behavior.

## March 28 refreshed Facebook Login evidence
A cleaner refreshed Facebook Login run is now rendered and should replace the older March 9 Facebook provider artifact for resubmission:
- Final combined reviewer-facing export:
  - `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`
  - SHA256: `156e3081ee519be93c4abaeeae206672c187dc4c53e032dbe9bbbbdb78fb1aab`
  - Duration: about `179.60s`
- Full clean desktop source with live auth, setup, send, and delete proof:
  - `screencast/meta-app-review/output/d4cc3e4636e280c9bdab91e926d47aa1.webm`
  - Duration: about `614.64s`
- Short desktop helper cut from that same run:
  - `screencast/meta-app-review/output/facebook-login-refresh-desktop-2026-03-28.mp4`
  - Duration: about `329.64s`
- Matching native-client source clip used in the final combined export:
  - `Screen_Recording_20260328_052625_Instagram.mp4`
  - Duration: about `19.74s`
- Exact inbound marker used to keep the thread clean:
  - `Meta review inbound marker 2026-03-28T10-21-55-782Z - from IG native client`
- Exact live send shown in that run:
  - `Meta review test message 2026-03-28T10-25-07-061Z - sent from TDF HQ UI`

Important packaging note:
- `final-facebook-login-2026-03-28.mp4` is the canonical reviewer-facing Facebook Login artifact for this refreshed package.
- `d4cc3e4636e280c9bdab91e926d47aa1.webm` remains the source-of-truth desktop capture because it includes the full Meta auth lane.
- `facebook-login-refresh-desktop-2026-03-28.mp4` is only a shorter desktop helper cut for internal review and should not be treated as the sole reviewer artifact.

## Live reviewer delta captured on 2026-03-27
Meta's current feedback requires two concrete adjustments before resubmission:
- `instagram_basic` and `instagram_business_basic`: the reviewer notes and screencast must explicitly state that the connected Instagram account is a professional or business account. In the Facebook Login flow it must also be clear that this account is linked to the selected Facebook Page.
- `instagram_manage_messages` and `instagram_business_manage_messages`: the screencast must show a sent message being deleted or unsent in the native Instagram client, and TDF HQ must visibly reflect that deletion via the inbox refresh flow.

## Requested permissions
- `instagram_basic`
- `instagram_manage_messages`
- `instagram_business_basic`
- `instagram_business_manage_messages`

Dependency scopes used in the Facebook Login flow for Page and asset discovery:
- `pages_show_list`
- `pages_read_engagement`

## Provider mapping in source
- Facebook Login run covers:
  - `instagram_basic`
  - `instagram_manage_messages`
- Instagram Login run covers:
  - `instagram_business_basic`
  - `instagram_business_manage_messages`

Source of truth:
- `tdf-hq-ui/src/services/instagramAuth.ts`
- `docs/META_APP_REVIEW_SCREENCAST.md`
- `screencast/meta-app-review/run.mjs`

## Current evidence references
Use these two files as the canonical reviewer-facing assets for the current submission route:

1. `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`
   - SHA256: `156e3081ee519be93c4abaeeae206672c187dc4c53e032dbe9bbbbdb78fb1aab`
   - Covers: `instagram_basic`, `instagram_manage_messages`
   - Refreshed March 28 final composed from the clean desktop rerun and matching native-client delete or unsend proof

2. `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`
   - SHA256: `aef00b419017fad5d65c5c76b559b2a382ac684b92193aec36fd6b78015138fc`
   - Covers: `instagram_business_basic`, `instagram_business_manage_messages`
   - Historical Instagram Login final retained unless Meta asks for provider-specific refreshed proof

Supporting Facebook Login sources kept for audit and quick review:
- `screencast/meta-app-review/output/d4cc3e4636e280c9bdab91e926d47aa1.webm`
- `screencast/meta-app-review/output/facebook-login-refresh-desktop-2026-03-28.mp4`

Generated manifest:
- `screencast/meta-app-review/evidence-manifest.json`

Combined reviewer narrative:
- `screencast/meta-app-review/submission-notes.txt`

Per-permission form answers for Meta App Review:
- `screencast/meta-app-review/permission-notes/instagram_basic.txt`
- `screencast/meta-app-review/permission-notes/instagram_manage_messages.txt`
- `screencast/meta-app-review/permission-notes/instagram_business_basic.txt`
- `screencast/meta-app-review/permission-notes/instagram_business_manage_messages.txt`

Primary evidence sources for this packet:
- `docs/META_APP_REVIEW_SCREENCAST.md`
- `screencast/meta-app-review/`

## Canonical four-permission evidence map
- `instagram_basic`
  - Reused guide and source evidence: `docs/META_APP_REVIEW_SCREENCAST.md`, `screencast/meta-app-review/evidence-manifest.json`
  - Base video: `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`
  - Reviewer focus for resubmission: Meta auth plus consent, then explicit proof that the selected Instagram account is the professional or business messaging asset on `/social/instagram?review=1`
  - Verdict: refreshed canonical final exists and is ready to attach
- `instagram_manage_messages`
  - Reused guide and source evidence: `docs/META_APP_REVIEW_SCREENCAST.md`, `screencast/meta-app-review/evidence-manifest.json`
  - Base video: `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`
  - Reviewer focus for resubmission: inbound conversation open, reply send, delivered-message verification in the native client, then delete or unsend plus deleted-message refresh in TDF HQ
  - Verdict: refreshed canonical final exists and is ready to attach
- `instagram_business_basic`
  - Reused guide and source evidence: `docs/META_APP_REVIEW_SCREENCAST.md`, `screencast/meta-app-review/evidence-manifest.json`
  - Base video: `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`
  - Reviewer focus for resubmission: Instagram Login consent plus explicit proof that the selected account is the professional or business messaging asset on `/social/instagram?review=1`
  - Verdict: reuse the historical Instagram final unless Meta asks for a provider-specific refresh
- `instagram_business_manage_messages`
  - Reused guide and source evidence: `docs/META_APP_REVIEW_SCREENCAST.md`, `screencast/meta-app-review/evidence-manifest.json`
  - Base video: `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`
  - Reviewer focus for resubmission: inbound conversation open, reply send, delivered-message verification in the native client, then delete or unsend plus deleted-message refresh in TDF HQ
  - Verdict: reuse the historical Instagram final unless Meta asks for a provider-specific refresh

## Permission packet checklist
- [ ] `instagram_basic`
  - Purpose in TDF HQ: lets the Facebook Login review flow identify the connected Instagram messaging account and show the selected asset before entering the inbox reply flow.
  - Reviewer instructions: in `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`, watch the Meta login plus consent dialog, then confirm `/social/instagram?review=1` shows the provider and scope chips, connected status, and the selected professional or business messaging asset with Page ID plus Instagram User ID before the flow continues.
  - Evidence references: `docs/META_APP_REVIEW_SCREENCAST.md`, `screencast/meta-app-review/permission-notes/instagram_basic.txt`, `screencast/meta-app-review/submission-notes.txt`, `screencast/meta-app-review/evidence-manifest.json`, `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`
- [ ] `instagram_manage_messages`
  - Purpose in TDF HQ: lets the Facebook Login review flow open an inbound Instagram conversation and send a reply from the TDF HQ inbox UI.
  - Reviewer instructions: in `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`, continue from setup into `/social/inbox?review=1`, open the inbound marker thread, send the test reply, show the exact delivered message in the native Instagram client, then show the delete or unsend of that same message and the deleted-message refresh back in TDF HQ.
  - Evidence references: `docs/META_APP_REVIEW_SCREENCAST.md`, `screencast/meta-app-review/permission-notes/instagram_manage_messages.txt`, `screencast/meta-app-review/submission-notes.txt`, `screencast/meta-app-review/evidence-manifest.json`, `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`
- [ ] `instagram_business_basic`
  - Purpose in TDF HQ: lets the Instagram Login review flow identify the connected professional or business Instagram messaging account and show the selected asset before entering the inbox reply flow.
  - Reviewer instructions: in `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`, watch the Meta login plus consent dialog, then confirm `/social/instagram?review=1` shows connected status and the selected professional or business messaging asset before the flow continues.
  - Evidence references: `docs/META_APP_REVIEW_SCREENCAST.md`, `screencast/meta-app-review/permission-notes/instagram_business_basic.txt`, `screencast/meta-app-review/submission-notes.txt`, `screencast/meta-app-review/evidence-manifest.json`, `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`
- [ ] `instagram_business_manage_messages`
  - Purpose in TDF HQ: lets the Instagram Login review flow open an inbound Instagram conversation and send a reply from the TDF HQ inbox UI.
  - Reviewer instructions: in `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`, continue from setup into `/social/inbox?review=1`, open the inbound thread, send the test reply, show the exact delivered message in the native Instagram client, then show the delete or unsend of that same message and the deleted-message refresh back in TDF HQ.
  - Evidence references: `docs/META_APP_REVIEW_SCREENCAST.md`, `screencast/meta-app-review/permission-notes/instagram_business_manage_messages.txt`, `screencast/meta-app-review/submission-notes.txt`, `screencast/meta-app-review/evidence-manifest.json`, `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`

## What the reviewer sees in-app
Setup flow:
- `/social/instagram?review=1`
- explicit review-mode heading
- active provider and requested scope chips
- connected status
- selected professional or business messaging asset with Page ID and Instagram User ID

Validated setup asset:
- `TDF Studio`
- `169846481310276`
- `@tdf.records.label`
- `17841445628242005`

Messaging flow:
- `/social/inbox?review=1`
- selected asset summary
- inbound conversation selection
- outgoing message composer
- send action
- success state and native-client delivery verification
- native-client delete or unsend of the same message
- deleted-message refresh back in TDF HQ

## Repo verification completed on 2026-03-27 and 2026-03-28
- Review-mode inbox copy explicitly calls out deleted-message refresh in `tdf-hq-ui/src/pages/SocialInboxPage.tsx`.
- Review automation script updated in `screencast/meta-app-review/run.mjs`.
- Reviewer notes updated in `screencast/meta-app-review/submission-notes.txt`.
- Live app-UI send on a real Instagram thread succeeded with the validated selected asset.
- Live Instagram delete or unsend on that same real thread removed the row from `/social/inbox?review=1` after the review-mode auto-refresh window.
- The refreshed March 28 Facebook Login final now includes the clean inbound-marker rerun and matching native-client proof segment.

## Strongest submission shape
In Meta App Review, the best truthful package is now:
- Facebook Login final: `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`
- Instagram Login historical final: `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`
- reviewer text from `screencast/meta-app-review/submission-notes.txt`

Supporting internal audit artifacts:
- `screencast/meta-app-review/output/d4cc3e4636e280c9bdab91e926d47aa1.webm`
- `screencast/meta-app-review/output/facebook-login-refresh-desktop-2026-03-28.mp4`

This route avoids another provider rerun unless Meta explicitly asks for refreshed Instagram Login-specific proof.

## Known operational constraints to mention if Meta asks
- Facebook Login messaging requires a professional Instagram account linked to a Facebook Page.
- Recipient must have messaged first within the last 7 days.
- Review and development accounts may need app role or tester access.
- `instagram_manage_messages` and `instagram_business_manage_messages` typically need Advanced Access for production use with non-role accounts.

## Current blocker
The browser-control blocker and the Facebook packaging blocker are cleared.
The remaining work is control-plane submission:
- attach `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`
- attach `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`
- paste the matching file from `screencast/meta-app-review/permission-notes/` into each permission form
- use `screencast/meta-app-review/submission-notes.txt` only for any general reviewer-instructions field
- click Meta `Request again`

Live submission target remains:
- `https://developers.facebook.com/apps/1098715965613487/app-review/submissions/feedback/?submission_id=1148115637340186&business_id=171424997819091`

Manual last mile:
1. Open the live Meta feedback page above in the correct authenticated session.
2. Open **Permissions and Features**.
3. Attach `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4` and `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`.
4. For each permission textarea, paste the matching note file:
   - `instagram_basic` -> `screencast/meta-app-review/permission-notes/instagram_basic.txt`
   - `instagram_manage_messages` -> `screencast/meta-app-review/permission-notes/instagram_manage_messages.txt`
   - `instagram_business_basic` -> `screencast/meta-app-review/permission-notes/instagram_business_basic.txt`
   - `instagram_business_manage_messages` -> `screencast/meta-app-review/permission-notes/instagram_business_manage_messages.txt`
5. If Meta also shows a general reviewer-instructions field, paste `screencast/meta-app-review/submission-notes.txt` there.
6. Submit.
