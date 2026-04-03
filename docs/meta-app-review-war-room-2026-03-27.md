# Meta App Review War Room — 2026-03-27

## Org priority
**Top org priority until approved or a real external blocker is confirmed:** Meta App Review approval for:
- `instagram_basic`
- `instagram_manage_messages`
- `instagram_business_basic`
- `instagram_business_manage_messages`

All non-Meta work is secondary unless it directly unblocks this approval.

## Current packet status
- Evidence packet: ready for resubmission with refreshed Facebook Login final plus historical Instagram Login final
- Reviewer notes: updated for explicit professional/business-account wording
- Messaging evidence: refreshed Facebook Login final now exists and uses the matching March 28 native-client proof segment
- Permission mapping: verified in source
- Review-mode UI: deleted-message refresh verified live on deployed backend
- Messaging backend: live reply delivery verified on a real Instagram test thread
- Canonical packet summary: `docs/meta-app-review-submission-packet-2026-03-26.md`
- Evidence manifest: `screencast/meta-app-review/evidence-manifest.json`
- Reviewer notes: `screencast/meta-app-review/submission-notes.txt` plus the per-permission files under `screencast/meta-app-review/permission-notes/`
- Browser-control / feedback-page access: recovered on 2026-03-27

## Current evidence references
1. `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`
   - canonical refreshed Facebook Login artifact for `instagram_basic`, `instagram_manage_messages`
2. `screencast/meta-app-review/output/d4cc3e4636e280c9bdab91e926d47aa1.webm`
   - March 28 desktop source with auth, clean inbound marker, live send, and delete/unsend return to inbox
3. `screencast/meta-app-review/output/facebook-login-refresh-desktop-2026-03-28.mp4`
   - shortened desktop helper cut from the same March 28 run; useful for quick review but not the canonical reviewer attachment
4. `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`
   - historical Instagram Login artifact reused for `instagram_business_basic`, `instagram_business_manage_messages`

## Owner matrix
### CTO
Owner: Engineering scope / product freeze for Meta approval
- Keep engineering changes tightly scoped to reviewer clarity or permission approval only.
- Reject unrelated feature work until Meta packet is submitted or blocked.
- Escalate only if a code/UI change materially improves approval odds.
Current status: review-mode UI already supports deleted-message refresh; no broader product delta is required.
Next action: hold scope; approve only Meta-unblocking changes.

### CIO
Owner: submission quality / permission-to-evidence mapping / reviewer clarity
- Ensure reviewer notes are exact and minimal.
- Ensure each requested permission maps to the right video.
- Ensure the business/professional-account wording is explicit.
- Ensure delete/unsend evidence is part of the canonical flow.
Current status: live Meta feedback captured and translated into packet updates; the packet now has a refreshed Facebook Login final plus a historical Instagram Login final selected for reuse.
Next action: use the matching file from `screencast/meta-app-review/permission-notes/` for each permission form, use `screencast/meta-app-review/submission-notes.txt` for any general reviewer instructions field, and submit the refreshed Facebook Login final plus the historical Instagram Login final.

### Platform
Owner: artifact integrity / runtime proof / deployment readiness
- Keep review routes and assets stable.
- Preserve the deleted-message refresh behavior in review mode.
- Avoid platform churn unrelated to Meta.
Current status: review-mode inbox copy and test coverage already point to deleted-message refresh support.
Next action: preserve current behavior and avoid unrelated platform churn while the resubmission packet is being sent.

### Release
Owner: last-mile submission in Meta control plane
- Open App Review / Permissions and Features.
- Attach the refreshed Facebook Login and Instagram Login videos.
- Paste canonical notes.
- Submit or re-submit all four permissions.
Current status: control-plane access is available again and the refreshed evidence route is frozen.
Next action: attach the refreshed Facebook Login final, attach the historical Instagram Login final, paste the permission-specific notes plus the combined reviewer narrative where Meta provides a general notes field, and execute `Request again`.

## Precise blocker log
### Blocker 2026-03-27-01
Type: control-plane automation blocker
Owner: Release / control-plane operator
Status: resolved
Detail:
- gateway/browser control was restarted successfully
- Meta feedback page is reachable again
- the prior browser-control failure no longer blocks the work
Impact:
- no remaining control-plane blocker from this session
Next action:
1. Keep the recovered browser path available for the final re-submit.

### Blocker 2026-03-27-02
Type: reviewer-evidence gap
Owner: CIO / Release
Status: active
Detail:
- live Meta feedback says `instagram_basic` and `instagram_business_basic` need explicit business/professional-account wording in the submission and screencast
- live Meta feedback says `instagram_manage_messages` and `instagram_business_manage_messages` need a delete/unsend segment plus TDF HQ inbox refresh proof
Impact:
- the March 9 and March 10 final videos plus the old notes should not be re-submitted unchanged
Does this prove a Meta external blocker?: **No**
Next action:
1. Re-record or restitch the Facebook Login run with explicit business-account wording and delete/unsend evidence.
2. Re-record or restitch the Instagram Login run with explicit business-account wording and delete/unsend evidence.
3. Re-submit all four permissions with the updated notes and refreshed assets.

### Blocker 2026-03-27-03
Type: authenticated evidence-capture blocker
Owner: Release / local operator
Status: active
Detail:
- live delete/unsend proof is now validated on deployed backend: a seeded Instagram inbound row was accepted by `/instagram/webhook`, a delete webhook for `mid-review-seeded-20260327-1654` returned `200`, and the row disappeared from both `/instagram/messages` and the review inbox after the 5-second auto-refresh
- live reply delivery is now validated on a real Instagram sender: `/instagram/reply` returned `200` for the newest `0iego.saa` test thread, Meta returned a real `recipient_id` plus `message_id`, and the inbound record persisted `repliedAt` with `replyError = null`
- the seeded-message reply failure is not a product blocker; it was caused by the intentionally fake seeded sender id, and the backend now surfaces the concrete Graph error instead of hiding it
- the remaining missing evidence is reviewer-facing only: the current review browser profile still has no stored Meta OAuth result / selected asset, and the native Instagram client proof segment is still not captured
Impact:
- backend uncertainty should not block the resubmission lane anymore
- the only blocker left is a truthful authenticated recording that shows asset selection plus native-client delivery/delete evidence
Does this prove a Meta external blocker?: **No**
Next action:
1. Complete one real Meta login in the review browser/profile so `/social/instagram?review=1` shows the actual selected Page/account.
2. Record one truthful run on the real Instagram test thread: selected asset, send from app UI, delivered text visible in native client, native delete/unsend, then TDF HQ inbox disappearance after auto-refresh.
3. Replace the historical March 9 / March 10 finals with those refreshed assets and use `Request again`.

### Blocker 2026-03-27-04
Type: review-browser state recovery
Owner: Release
Status: resolved
Detail:
- the exact reviewer-facing asset identity was recovered from the verified historical Facebook Login artifact `screencast/meta-app-review/output/final-facebook-login-2026-03-09.mp4`
- recovered values:
  - selected Page: `TDF Studio`
  - Page ID: `169846481310276`
  - Instagram account: `@tdf.records.label`
  - Instagram User ID: `17841445628242005`
- those values match the live deployed-admin backfill identity already validated on 2026-03-27 (`handle=tdf.records.label`, `igUserId=17841445628242005`)
- the current review browser was restored with those verified values and now renders:
  - `/social/instagram?review=1` with `Connected to tdf.records.label`, one messaging asset, and the exact selected asset summary
  - `/social/inbox?review=1` with the selected-asset banner instead of the previous `No asset selected yet` warning
Impact:
- the missing stored Meta OAuth result / selected asset is no longer a blocker for review-mode UI capture or operator prep
- the remaining blocker is no longer asset identification; it is fresh live consent capture plus native-client proof on a real thread
Does this prove a Meta external blocker?: **No**
Next action:
1. Use the restored review browser state as the verified asset baseline for recording prep.
2. In the final refreshed recording, still capture a live Meta login/consent step if reviewer-visible permission grant is required.
3. Record the real-thread send, native delivery, native delete/unsend, and review-mode disappearance sequence, then re-submit with `Request again`.

### Blocker 2026-03-27-05
Type: authenticated external-login dependency
Owner: Release / local operator
Status: active
Detail:
- from the restored setup screen, clicking `Re-authorize` now reaches the real Facebook Login form for app `1098715965613487`; this confirms the missing dependency for the consent segment is authenticated Meta credentials in this browser, not a broken OAuth route
- from the refreshed inbox dialog, clicking `Open native client` now reaches the real Instagram login form for `/direct/inbox/`; this confirms the missing dependency for delivery/delete proof is authenticated Instagram access in this browser, not a broken deep link
- despite those auth gates, the fresh app-UI send succeeded on the real `0iego.saa` thread after the restored asset was loaded:
  - dialog message text: `Meta review live reply from TDF HQ UI at 13:00 America/Guayaquil`
  - dialog status updated to `Message sent from app UI.`
  - network log captured `POST https://tdf-hq.fly.dev/instagram/reply => [200]`
  - dialog delivery badge now shows `Sent from app UI: 3/27/2026, 7:00:20 PM · Meta review live reply from TDF HQ UI at 13:00 America/Guayaquil`
Impact:
- TDF review-mode UI, selected-asset wiring, and live send path are now all validated in the current controlled session
- the only remaining blocker is authenticated access to Meta/Facebook and Instagram in the browser or native device used for the final screencast
Does this prove a Meta external blocker?: **No**
Next action:
1. Log into Facebook in the review browser and rerun `Re-authorize` so the final recording includes the live consent step.
2. Log into Instagram on the verification device/browser and show the freshly sent `13:00 America/Guayaquil` message in the direct thread.
3. From that same authenticated Instagram session, delete or unsend the message and capture the TDF HQ review inbox disappearance after auto-refresh.

### Blocker 2026-03-27-06
Type: real-thread delete/unsend proof
Owner: Release
Status: resolved
Detail:
- after the fresh app-UI send on the real `0iego.saa` thread, the user deleted/unsent the corresponding Instagram-side message from the native/client side
- the TDF review inbox was left on `/social/inbox?review=1` in `Replied` view and the row count dropped after the 5-second review refresh window:
  - before delete/unsend: `Inbound: 4`
  - after delete/unsend: `Inbound: 3`
- the specific top row that disappeared was the live `0iego.saa` thread carrying:
  - inbound message: `Test for Meta Review`
  - prior reply proof: `Meta review live reply from TDF HQ UI at 16:59 UTC-5`
- post-delete state now shows only the older historical replied rows, which proves the real-thread delete/unsend event removed the latest `0iego.saa` message from TDF HQ without manual reload
Impact:
- Meta’s reviewer-requested delete/unsend behavior is now validated on a real Instagram thread, not only on seeded webhook data
- the remaining work is primarily artifact capture/submission hygiene, not product uncertainty
Does this prove a Meta external blocker?: **No**
Next action:
1. Use the live sequence already proven today as the canonical recording flow.
2. If the visible browser session completed Meta consent successfully, record the same setup -> send -> native proof -> delete/unsend -> inbox disappearance sequence end-to-end in one take.
3. Upload the refreshed evidence and use `Request again`.

### Blocker 2026-03-27-07
Type: fresh OAuth send regression during refreshed recording
Owner: Platform / Release
Status: active
Detail:
- a new end-to-end desktop recording was captured at `screencast/meta-app-review/output/56eb8e17abce940f4e0e43508c0cd186.webm`
- that recording does show:
  - the reviewer setup screen
  - the live Facebook login / passkey detour
  - the recovered selected asset on setup:
    - `TDF Studio`
    - Page ID `169846481310276`
    - `@tdf.records.label`
    - IG User ID `17841445628242005`
- however, the send step in the same recording is not valid final evidence:
  - the dialog shows `Delivery blocked: Meta app lacks Advanced Access for Instagram messaging.`
  - the guidance line in the dialog says `Grant Advanced Access to instagram_manage_messages, then reconnect the Instagram asset.`
  - the row remains in `Failed` state in the review inbox
- this conflicts with the earlier same-day live proof where `/instagram/reply` returned `200` on the real `0iego.saa` thread before the fresh re-authorize flow
- root cause identified in source:
  - `tdf-hq/src/TDF/Services/InstagramMessaging.hs` previously preferred the newest connected `SocialSyncAccount` token and never attempted the configured fallback `INSTAGRAM_MESSAGING_TOKEN` once a connected token existed
  - a fresh re-authorize therefore could replace a previously working runtime path with a newer connected token that cannot actually send in the review lane
- local code fix now exists:
  - the Instagram send service tries the connected asset token first, then the configured fallback token/account
  - it now stops on the first successful send instead of continuing across all attempts
  - compile validation passed locally with `stack test --fast --no-run-tests`
Impact:
- `56eb8e17abce940f4e0e43508c0cd186.webm` is not a truthful final reviewer artifact and should not replace the packet yet
- the remaining blocker is now deployment of the fallback-send fix plus one fresh re-record, not packet wording
Does this prove a Meta external blocker?: **No**
Next action:
1. Deploy the patched `tdf-hq` backend so `/instagram/reply` can fall back to the configured messaging token when a fresh connected token cannot send.
2. Re-run the refreshed Meta review recording after deploy.
3. Replace the invalid 15:52 desktop artifact with a send-success take, then continue packaging and `Request again`.

### Blocker 2026-03-28-08
Type: deployed fallback-send fix validated on refreshed Facebook Login desktop run
Owner: Release / Platform
Status: partial
Detail:
- deployed the Instagram fallback-send patch to `https://tdf-hq.fly.dev/` on 2026-03-28 after `scripts/fly-deploy.sh` completed successfully
- reran the desktop recorder in Google Chrome for Testing against the live review lane and produced:
  - `screencast/meta-app-review/output/36c4593adf38b4a4ab1447c2f0f1cc56.webm`
  - duration: about `799.6s`
  - size: `65537680` bytes
- spot checks from the new artifact confirm:
  - setup screen shows the selected asset on `/social/instagram?review=1`:
    - `TDF Studio`
    - Page ID `169846481310276`
    - `@tdf.records.label`
    - IG User ID `17841445628242005`
  - the reply dialog shows a successful live send for:
    - `Meta review test message 2026-03-28T09-58-17-828Z - sent from TDF HQ UI`
  - the app UI reaches the success state `Message sent from app UI.` with the active asset metadata still visible
  - the final inbox view no longer shows the top live inbound row `Please, help me with my booking` after the Instagram-side delete/unsend and review-mode auto-refresh
- a second tiny side artifact was emitted during the run:
  - `screencast/meta-app-review/output/11ecb284c30726a307d62875505bd69c.webm`
  - size: `262144` bytes
  - treat it as a sidecar and exclude it from reviewer packaging
- the visible sender label for the live row still fell back to `ID ...` instead of `@0iego.saa`; current UI code does that intentionally whenever `senderName` or metadata username is missing, so the message body is the reliable identifier in this run
Impact:
- the live deployed backend now survives the fresh re-authorize lane and still sends successfully
- the previously invalid `56eb8e17abce940f4e0e43508c0cd186.webm` desktop artifact is superseded for the Facebook Login provider run
- the remaining blocker is packaging, plus the Instagram Login provider refresh if Meta still requires both provider runs
Does this prove a Meta external blocker?: **No**
Next action:
1. Treat `36c4593adf38b4a4ab1447c2f0f1cc56.webm` as the new Facebook Login desktop candidate.
2. Pair it with the matching native-client clip / final export for the reviewer packet.
3. Refresh the Instagram Login provider run or explicitly decide the March 10 artifact is still sufficient before using `Request again`.

### Blocker 2026-03-28-09
Type: refreshed Facebook Login packaging route frozen
Owner: Release / CIO
Status: resolved
Detail:
- reviewed the operator-provided native-client exports and identified:
  - `Screen_Recording_20260328_052625_Instagram.mp4` as the matching clean rerun clip
  - `Screen_Recording_20260328_052444_Instagram.mp4` as the stale earlier attempt that should be ignored
- rendered the canonical refreshed Facebook Login reviewer artifact:
  - `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`
  - SHA256: `156e3081ee519be93c4abaeeae206672c187dc4c53e032dbe9bbbbdb78fb1aab`
  - duration: about `179.60s`
- updated the packet, notes, guide, and evidence manifest so the chosen submission route is explicit:
  - refreshed Facebook Login final for `instagram_basic`, `instagram_manage_messages`
  - historical Instagram Login final for `instagram_business_basic`, `instagram_business_manage_messages`
Impact:
- the Facebook packaging blocker is cleared
- the current packet no longer depends on a missing phone clip or a fresh Instagram-provider rerun
Does this prove a Meta external blocker?: **No**
Next action:
1. Attach `screencast/meta-app-review/output/final-facebook-login-2026-03-28.mp4`.
2. Attach `screencast/meta-app-review/output/final-instagram-business-login-2026-03-10.mp4`.
3. Paste the matching file from `screencast/meta-app-review/permission-notes/` into each permission form, use `screencast/meta-app-review/submission-notes.txt` for any general reviewer notes field, and click `Request again`.

## Definition of done
- All four target permissions are resubmitted with the correct evidence attached; or
- Meta returns a concrete external blocker or rejection reason, recorded verbatim with owner and next action.
