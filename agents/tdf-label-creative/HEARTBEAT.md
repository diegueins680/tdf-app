# HEARTBEAT.md — tdf-label-creative

# Keep this file empty (or with only comments) to skip heartbeat API calls.

# Add tasks below when you want the agent to check something periodically.
## Weekly Checks (Every Monday)
- [ ] Run generate-ideas.js
- [ ] Score new ideas with ICE
- [ ] Run prioritize-backlog.js
- [ ] Review top 3 with CTO

## Daily Checks (When experiments are running)
- [ ] Check experiment enrollment (users per variant)
- [ ] Verify event pipeline is receiving data
- [ ] Alert if sample size < 100 after 3 days

## End-of-Week (Friday)
- [ ] Ship experiment to preview channel
- [ ] Start 7-day measurement clock

## Analysis Day (Following Friday)
- [ ] Run analyze-experiments.js
- [ ] Update backlog with PROMOTE/KILL/ITERATE decisions
- [ ] Document learnings
