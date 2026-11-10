# Course registration phone contract

Source: Android user test on 2026-10-07. An enrollment with `0988384849` failed with `phoneE164 inválido`.

## COURSE-PHONE-001
- Public course registration stores phones in E.164 form on both the legacy lead path and the checkout path.
- It accepts international numbers with a leading `+` and a non-zero country code.
- It accepts Ecuador national numbers, `09XXXXXXXX` (mobile) and `0[2-7]XXXXXXX` (landline), with spaces or `-().` separators.
- It rejects free text, wrong lengths, invalid prefixes, non-ASCII digits and control/format separators.
- The checkout idempotency hash keeps the trimmed phone text as earlier releases computed it, so retries keep matching across the rollout.
- Operator WhatsApp and ads-inquiry phone rules are unchanged.
