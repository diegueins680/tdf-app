# Future human usability-testing protocol

Status: protocol only. **No human participants have taken part in this repository assignment.** No testimony, emotion, survey answer, completion time, SUS, UMUX-Lite or NPS result exists.

## Research questions

1. Can visitors discover a relevant artist/event/service and understand price, privacy and the registration boundary without help?
2. Can customers distinguish pending, cancelled, confirmed, refunded and reconciled financial states and recover without duplicate payment?
3. Can creators and staff with multiple roles predict which records/actions they own and why an action is denied?
4. Can keyboard, screen-reader, zoom, reduced-motion and color-vision users independently complete critical tasks?
5. Do web/native transitions preserve intent and trust on typical Ecuadorian mobile connectivity?

## Participants and sampling

Recruit 12–18 adults in two rounds, balancing Ecuadorian visitors/customers, independent artists, teachers/studio staff, event/marketplace actors and label/distribution clients. Include Spanish-first and English-speaking participants, lower and higher technical proficiency, Android/iOS/desktop users, intermittent connectivity, keyboard-only and at least two people who regularly use screen readers or magnification.

Do not recruit a real minor for the first rounds. Test guardian/consent comprehension with adults acting only as themselves. Any later study with minors requires a separately approved safeguarding plan, guardian permission, minor assent, minimum data collection, a trained moderator and jurisdiction-appropriate legal/privacy review.

Compensation, recruitment channels, recording and data retention require project-owner approval. Avoid recruiting direct reports for evaluative workplace tasks where coercion is possible.

## Safety and data

- Use an isolated test deployment and the deterministic fictional catalog. Never production.
- Give each participant a unique disposable account; never ask for a personal password, real card, real social login, unreleased music, legal identity or private contacts.
- Payment, email, message, WhatsApp, ticket and distribution actions use visible fakes/sandboxes and cannot leave the environment.
- Consent separately for participation, screen/audio recording and quotation. Participation does not require recording.
- Redact faces, voices, tokens and typed personal information from issue/PR evidence. Store raw recordings outside Git with access control and a declared deletion date.
- Stop a task if it could charge, publish, contact, expose data, lock an account or trigger provider security controls.

## Session structure (60–75 minutes)

1. Consent, accessibility/accommodation check and neutral background questions (5–10 minutes).
2. Unscripted first impression and discovery from the public home (5 minutes).
3. Two critical tasks appropriate to the participant role (25–35 minutes).
4. One recovery/edge-case task: expired session, cancelled payment, unavailable slot/item or provider delay (10 minutes).
5. Web/mobile continuation or accessibility-specific task where applicable (5–10 minutes).
6. Post-task questions and standardized questionnaire selected in advance (5–10 minutes).

Moderators should use think-aloud prompts without teaching navigation. Allowed neutral prompts include “What are you looking for?” and “What would you expect to happen?” Record each intervention exactly; do not reinterpret silence as emotion.

## Task families

- Visitor/customer: find a Quito event or service, compare total cost, begin checkout, recover after login/payment interruption and retrieve authoritative status.
- Artist/multi-role: complete a private profile, find a collaborator, book a room, resume across devices and identify which role enabled each action.
- Teacher/student/guardian: find availability, request a trial, handle a conflict/consent state and verify the correct schedule/record scope.
- Reception/studio/operations: convert a consented inquiry, prevent a conflict, assign resources, record completion/return and reconcile the operational handoff.
- Buyer/seller/organizer: transact with a second participant/fixture through handoff, cancellation/refund/validation and closeout.
- Label/distribution: validate and correct a synthetic release in a fake-partner environment only; do not imply production delivery while DDEX is incomplete.
- Accessibility: landmarks/headings, keyboard focus order, error announcement/recovery, 200% text zoom/320 CSS-pixel reflow, reduced motion and non-color status.

## Observable measures

For each task capture completion (success/partial/fail), elapsed time, meaningful steps, backtracks/dead ends, errors, assistance points, recovery outcome, unexpected console/network events and final authoritative state. Record exact participant wording only with consent and identify it as participant feedback, never persona inference.

After tasks, a preselected validated instrument such as SUS or UMUX-Lite may be administered verbatim under its scoring rules. Scores are calculated only from actual complete participant responses and reported with sample size and uncertainty. Do not combine synthetic rubric results with participant scores. NPS is optional and should not substitute for usability evidence.

## Accessibility accommodations

Let participants use their own assistive technology/device when safe, or offer configured VoiceOver, TalkBack, NVDA/VoiceOver, keyboard, browser zoom, high contrast and reduced motion. Ask preferred interaction and breaks before starting. The moderator must not take control unless requested or safety requires it.

## Analysis and reporting

- Triangulate recordings/notes, task events, browser traces and database/audit state.
- Separate observed behavior, participant quotation, moderator interpretation and design recommendation.
- Deduplicate by root cause while retaining all roles/tasks affected.
- Report disconfirming evidence and non-completions; do not average away accessibility blockers.
- Apply the repository finding template and link only sanitized evidence.
- A second reviewer checks severity, quote accuracy and privacy redaction before any GitHub mutation.

## Pilot and stop criteria

Pilot with two consenting adults. Pause recruitment if a task reaches production, a fake adapter can contact a real recipient/provider, consent/recording state is unclear, a participant enters real sensitive data, or a Blocker/Critical defect risks later participants. Fix the environment, invalidate exposed credentials/data if any, update the protocol and re-pilot.
