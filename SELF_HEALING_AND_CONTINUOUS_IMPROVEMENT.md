# Self-Healing & Continuous Improvement — TDF-Label

## 1. Philosophy

> *"A mobile app in production should heal its own scrapes and learn from every crash, timeout, or failed OAuth flow."*

This document defines the self-healing and continuous-improvement (SHCI) contract for the `tdf-label` (TDF Records) repo. It complements the existing `CHANGELOG.md`, `TESTING_GUIDE.md`, and `DEPLOYMENT_GUIDE.md` with explicit operational resilience and learning loops.

---

## 2. Self-Healing Architecture

### 2.1 Layers

| Layer | Responsibility | Current Implementation | Gap |
|-------|---------------|------------------------|-----|
| **L1: Build** | Recover from transient build failures | EAS build retries; Detox test retries | No automatic retry on `SIMCTL_DAEMON_DEADLOCK` |
| **L2: Test** | Detect regressions before release | Detox E2E (21 consecutive PASSes) | Physical-device Google OAuth not yet automated |
| **L3: Deploy** | Graceful rollback on deploy failure | EAS preview builds; manual RC promotion | No automatic rollback on crash-rate spike |
| **L4: Runtime** | Handle runtime errors without user-visible crash | React Error Boundaries; Sentry (planned) | Sentry not yet integrated |
| **L5: Learning** | Update code / config from incidents | Manual post-mortem in `CHANGELOG.md` | No structured incident artifact directory |

### 2.2 Health Signals & Auto-Actions

| Signal | Threshold | Auto-Action | Escalation |
|--------|-----------|-------------|------------|
| Detox test failure | Any failure in CI | Retry once; if still fail, block release | Slack alert to #mobile-ci |
| EAS build failure | Any non-zero exit | Retry once with clean cache | Alert if retry fails |
| Preview build crash rate | > 1% on TestFlight/Play Console | Halt RC promotion; investigate | PagerDuty if > 5% |
| Google OAuth failure (simulator) | Any failure | Block release; file issue | Alert if persists > 24h |
| Google OAuth failure (physical) | Any failure | Block RC promotion | Alert immediately |
| Stale snapshot in review | > 20% of test screenshots differ | Flag for visual regression review | Block release if > 50% |

---

## 3. Continuous Improvement Loop

### 3.1 The Loop

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   OBSERVE   │───>│   ANALYZE   │───>│   DECIDE    │───>│    ACT      │
│  (crash     │    │  (Detox     │    │  (sprint    │    │  (PR,       │
│   logs,     │    │   reports,  │    │   planning, │    │   release,  │
│   reviews)  │    │   user      │    │   operator) │    │   hotfix)   │
│             │    │   feedback) │    │             │    │             │
└─────────────┘    └─────────────┘    └─────────────┘    └──────┬──────┘
     ^                                                           │
     └───────────────────────────────────────────────────────────┘
                         (measure impact, feed back to OBSERVE)
```

### 3.2 Observability Requirements

Every release must be **verifiable**:

1. **Baseline**: Last known good build ID and test streak count.
2. **Change**: PR description links to ticket and describes user-visible impact.
3. **Verification**: Detox PASS count; simulator + physical device matrix.
4. **Rollback**: Previous build ID kept available for 30 days.

### 3.3 Release Readiness Gate

From `release-readiness.md`:

| Gate | Criteria | Current Status |
|------|----------|----------------|
| Simulator tests | 21 consecutive Detox PASSes | ✓ |
| Physical device auth | Google OAuth PASS on real device | ⏳ Pending (due 2026-05-21) |
| Binary verification | Mach-O universal, ~32 MB | ✓ |
| Documentation | `CHANGELOG.md` updated | ⏳ Unreleased section empty |

---

## 4. Incident Response & Learning

### 4.1 Incident Levels

| Level | Definition | Response Time | Post-Incident Review |
|-------|-----------|---------------|----------------------|
| **Sev 1** | App crash on launch, data loss, security breach | Immediate (< 30 min) | Within 24h; update SECURITY_NOTICE.md |
| **Sev 2** | Feature broken (e.g., OAuth fails), performance regression | < 2 hours | Within 48h; update TESTING_GUIDE.md if test gap |
| **Sev 3** | UI glitch, non-critical deprecation warning | < 24 hours | Next sprint; update relevant docs |

### 4.2 Post-Incident Review Template

Every Sev 1/2 incident produces a file: `artifacts/incidents/INCIDENT_YYYY-MM-DD_{short-name}.md`

```markdown
# INCIDENT_YYYY-MM-DD_{short-name}

## Summary
One-line description.

## Timeline
- HH:MM UTC — Detection (how?)
- HH:MM UTC — Impact start
- HH:MM UTC — Mitigation applied
- HH:MM UTC — Recovery complete

## Root Cause
Technical root cause.

## Affected Users
Approximate count or "all" / "none (caught in preview)."

## Fix
Code/config change.

## Verification
How do we know the fix works? (Test, build, manual verification)

## Prevention
What self-healing or monitoring change prevents recurrence?
```

---

## 5. Current Gaps & Next Actions

| Gap | Priority | Action | Owner | Deadline |
|-----|----------|--------|-------|----------|
| Physical-device Google OAuth | **P0** | Execute `google-oauth-physical-device-review.md` | @diegosaa | 2026-05-21 |
| Sentry integration | P1 | Add Sentry SDK for crash tracking | @diegosaa | 2026-05-31 |
| Auto-rollback on crash spike | P2 | Wire EAS / TestFlight crash rate to release gate | @diegosaa | 2026-06-07 |
| Incident artifact directory | P2 | Create `artifacts/incidents/` and template | @diegosaa | 2026-05-24 |
| Visual regression tests | P3 | Add Percy or Maestro visual diff to CI | @diegosaa | 2026-06-14 |

---

## 6. Metrics & KPIs for SHCI

### 6.1 System Health

| Metric | Target | Measurement |
|--------|--------|-------------|
| Detox consecutive PASS streak | ≥ 21 | CI logs |
| EAS build success rate | > 95% | EAS dashboard |
| Preview-to-RC promotion time | < 48h after physical-device PASS | `CHANGELOG.md` timestamps |
| Crash-free session rate | > 99.5% | Sentry (once integrated) |
| Mean time to hotfix (MTTH) | < 4 hours | Incident log |

### 6.2 Improvement Velocity

| Metric | Target | Measurement |
|--------|--------|-------------|
| Releases per month | ≥ 2 | `CHANGELOG.md` build count |
| Test coverage increase | +2% per month | Coverage reports |
| User-reported bugs closed | > 90% within 2 weeks | Issue tracker |
| Docs updates per release | ≥ 1 | `CHANGELOG.md` doc changes |

---

## 7. References

- `CHANGELOG.md` — Build history and known issues
- `TESTING_GUIDE.md` — Detox and manual testing procedures
- `DEPLOYMENT_GUIDE.md` — EAS and App Store deployment
- `release-readiness.md` — Current release gate status
- `docs/google-oauth-physical-device-review.md` — Pending P0 validation
- `SECURITY_NOTICE.md` — Security incident procedures

---

*Last updated: 2026-05-21*
*Next review: On next release or Sev 1/2 incident*
