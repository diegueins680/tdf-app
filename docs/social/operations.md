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

## Provider preview behavior observed

Opening #355/#356 automatically triggered the repository's Vercel/Cloudflare
integrations; these are provider attempts, not deployment commands issued here.
Their result must be read from actual checks. Subsequent commits use the
Cloudflare-specific `[CF-Pages-Skip]` prefix and branch-scoped Vercel
`git.deploymentEnabled` exclusions for `feat/social-*` (root and UI project roots).
GitHub verification remains enabled. Provider docs accessed 2026-09-14:
[Cloudflare skip builds](https://developers.cloudflare.com/pages/configuration/git-integration/github-integration/),
[Vercel Git configuration](https://vercel.com/docs/project-configuration/git-configuration)
(updated 2026-08-25). Do not use general `[skip ci]` or disable correctness checks.
