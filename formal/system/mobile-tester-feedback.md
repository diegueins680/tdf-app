# Mobile tester feedback (MOB-FEEDBACK-001)

The authoritative declaration is `formal/system/requirements.json`. Implementation resides in the pinned Mobile `src/feedback/FeedbackEntry.tsx` and `metadata.ts`.

Submission controls require consent, a description, published category/severity choices, a valid optional image and no pending submission. Technical details are optional. Their field allowlist includes app/build, platform/OS version, locale, fixed entry surface, environment and a coarse phone/tablet/unknown class. The helper never spreads arbitrary input fields or collects the full device model. User text and screenshots can contain personal data; this does not claim automatic anonymization.

Participation storage recovers malformed input, counts opportunities at 30-minute intervals, and honors opt-out and dismissal. Existing `TesterParticipation.test.ts` and `TesterFeedback.test.tsx` provide scoped executable evidence. The feedback request has no new exactly-once delivery or deduplication guarantee.

These client tests do not establish production upload behavior, physical-device acceptance, store distribution or a universal formal proof. Those remain release prerequisites. This mapping preserves the valid metadata change from Mobile #157 in successor #158 and parent #542.
