# Rollout and useful-outcome measurement

All new behavior defaults off. No deployment or activation is part of this work.
Before rollout: verify schema, authorized legacy/client compatibility, both flags,
rollback with post-migration writes, all denied paths, media access and CI. Rehearse
on synthetic or approved non-production data only. Never source production envs.

Preserve additive tables on rollback; disabling behavior is not deleting new
consent/block/preferences. A rollback to a version that ignores newly asserted
blocks is unsafe: retain authoritative deny guards or close affected social
endpoints until the compatible binary returns. Destructive cleanup is separate.

Reconcile missing targets, duplicate intents, self-links, orphan references, stale
revisions, projection lag and blocked-but-delivered events. Alert on any unauthorized
result or stale resurrection, not merely aggregate latency. Query only minimized
counts in routine monitoring. No relationship lists or message text in telemetry.

Post-release plan (not executed): measure accepted relevant connections per exposed
eligible principal, consented collaboration/contact leads, completed bookings and
paid sales attributed through existing conversion identifiers when available.
Mutual first-message participation can be a conversation proxy without reading its
contents; volume alone is not a meaningful conversation. Separate refunds/canceled
bookings and organic conversions. Compare a simple nonpersonalized baseline with
opted-in Discover only after experiment approval. Guardrails: blocks/reports per
exposure, notification opt-outs, error/p95 latency, recommendation repeat rate,
exposure share for top 1% vs smaller relevant artists, and cold-start coverage.
Use existing analytics consent and retention policies; do not add contact imports,
sensitive-trait inference, hidden transactions or private graph explanations.
