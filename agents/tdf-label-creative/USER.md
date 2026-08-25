# USER.md — Creative Director Context

## Human

- **Name:** Diego
- **Project:** TDF Records mobile app (tdf-mobile) — React Native / Expo iOS app
- **Audience:** Music artists, producers, label managers in Latin America
- **Current Stage:** Pre-launch / internal testing (19 consecutive Detox PASSes)

## Product Context

### What TDF Records Does
- Music release management (upload, distribute, track)
- Artist roster management
- Royalty tracking and payouts
- Collaboration tools for artists and producers

### Current Features (Known)
- Username/password auth + Google OAuth
- Course registrations (from HQ UI — likely artist education/training)
- File upload to cloud storage
- Event bookings

### Engagement Gaps (Inferred)
- No visible social/sharing features in current codebase
- No referral/invite system detected
- No gamification or habit loops
- No push notification strategy visible

## Creative Director's Mandate

1. **Identify engagement opportunities** in the existing product
2. **Propose experiments** that can be built and tested quickly
3. **Measure real user behavior** — not just "would you use this?"
4. **Steer roadmap** toward features that move retention and virality metrics

## Constraints

- iOS-first (Expo / React Native)
- EAS build pipeline (blocked on credentials — separate track)
- Detox e2e tests must pass for any release
- Physical device testing required for some features

## Success Metrics

| Metric | Current | Target |
|--------|---------|--------|
| D1 Retention | Unknown | >40% |
| D7 Retention | Unknown | >20% |
| Session Length | Unknown | >5 min |
| Share Rate | 0% | >5% |
| Invite Conversion | 0% | >10% |
