# TOOLS.md — Creative Director Specifics

## Experiment Infrastructure

### A/B Test Harness
- **Location:** `tdf-mobile/src/experiments/`
- **Pattern:** Feature flags with variant assignment
- **Storage:** AsyncStorage for local variant persistence
- **Analytics:** Custom events to backend (or PostHog/Mixpanel if integrated)

### Metrics Pipeline
- **Backend:** `tdf-hq` (Haskell server) — add `/api/v1/events` endpoint
- **Events:** `experiment_viewed`, `experiment_converted`, `feature_used`, `shared`, `invited`
- **Dashboard:** Weekly rollup in `reports/creative-metrics-YYYY-MM-DD.md`

## Feature Idea Backlog

**Location:** `agents/tdf-label-creative/backlog.md`

Format:
```markdown
### IDEA-001: Feature Name
- **Hypothesis:** If we X, then Y will happen because Z
- **ICE Score:** Impact X / Confidence Y / Ease Z = Total
- **Experiment:** What to build and how to test
- **Metrics:** What success looks like
- **Effort:** Story points or days
- **Status:** backlog | designing | testing | shipped | killed
```

## Quick Commands

```bash
# Generate new feature ideas (run weekly)
cd /Users/diegosaa/GitHub/tdf-app && node agents/tdf-label-creative/scripts/generate-ideas.js

# Run experiment analysis (run after 7 days of data)
cd /Users/diegosaa/GitHub/tdf-app && node agents/tdf-label-creative/scripts/analyze-experiments.js

# Update backlog priority
cd /Users/diegosaa/GitHub/tdf-app && node agents/tdf-label-creative/scripts/prioritize-backlog.js
```

## Integration Points

- **Release Director (tdf-label-release):** Winning experiments need Detox tests + EAS builds
- **CTO (tdf-label-cto):** Technical feasibility review for complex features
- **Platform (tdf-label-platform):** Backend endpoints for experiment events

## Creative Sprint Cadence

- **Monday:** Generate ideas, score with ICE, pick top 3
- **Tuesday-Thursday:** Build experiments (small, reversible)
- **Friday:** Ship to preview channel, start 7-day measurement window
- **Next Friday:** Analyze results, decide promote/kill/iterate
