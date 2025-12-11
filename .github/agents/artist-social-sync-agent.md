---
name: Artist Social Sync Agent
description: Keeps artist social posts in sync with TDF Records
---

# Artist Social Sync Agent

## Mission
- Keep TDF Records in step with what artists publish on Instagram, TikTok, LinkedIn (extendable to YouTube/Twitter).
- Maintain a clean, canonical feed per artist inside TDF: captions, media, links, hashtags, metrics, and AI-enriched insights.
- Feed other features (artist profile, CRM, Social Events) with fresh social content and structured signals.

## Implementation (Graph API + offline ingest)
- Backend Haskell routes: `POST /social-sync/ingest` ingests push payloads, dedupes by `(platform, externalPostId)`, classifies tags (release/show/merch/press/general), and stores summaries; `GET /social-sync/posts` lists posts with filters.
- Storage: Persistent tables `SocialSyncAccount`, `SocialSyncPost`, `SocialSyncRun` (auto-migrated via `migrateAll`), with minimal metrics (likes/comments/shares/views) and ingest source tracking.
- Classification: Keyword-based tagger + 180-char summary; no LLMs or HTTP clients used.
- Limits: Server-side clamp to ≤500 posts per query; ingest defaults to `"manual"` source when not provided.
- Daily Instagram sync cron: for every party with an Instagram handle, schedule a once-a-day job at a random time; uses Instagram Graph API (`INSTAGRAM_APP_TOKEN` or per-account token) to fetch recent media and upsert into `SocialSyncPost`, logging runs in `SocialSyncRun`.

## Success Criteria
- New posts land in TDF within minutes (webhook-first, polling fallback).
- Zero duplicates: every platform post maps to a single `SocialPost` via `externalPostId`.
- Posts are tagged and summarized automatically (release, show, merch, press, general update).
- Artists can see, filter, and export their posts from TDF without re-authenticating.
- Revocation-safe: tokens rotated and expired tokens do not break other artist features.

## Data Model (to implement in backend)
- `SocialAccount`: `partyId`, `artistProfileId`, `platform` (`instagram|tiktok|linkedin|...`), `externalUserId`, `handle`, `accessToken`, `refreshToken`, `scopes`, `webhookSecret`, `lastSyncedAt`, `status`.
- `SocialPost`: `socialAccountId`, `platform`, `externalPostId`, `permalink`, `caption`, `media` (array with type/urls), `postedAt`, `fetchedAt`, `metrics` (likes/comments/shares/views), `hashtags`, `location`, `language`, `confidence`, `llmSummary`, `llmTags` (release/show/merch/press/other), `entities` (dates/venues/cities/people), `crossLinks` (eventId, releaseId), `visibility` (public/private), `ingestSource` (`webhook|poll|manual`).
- `SocialSyncRun`: audit log per run with `startedAt`, `endedAt`, `platform`, `status`, `error`, `newPosts`, `updatedPosts`, `backfillUntil`.
- Reuse existing `Party`/`ArtistProfile` to attach `SocialAccount` and surface posts per artist.

## Workflows

### Connect Account
1) Artist starts OAuth from TDF UI/mobile (`/social-sync/oauth/:platform/start`).
2) After callback, store tokens in `SocialAccount` and enqueue `SocialSyncRun` (mode: `backfill`).
3) Register webhooks where available (IG Graph, LinkedIn Share webhook analogs); store `webhookSecret`.

### Sync Loop
- **Webhook-first:** Receive platform webhook → verify HMAC/secret → enqueue ingest for the affected `SocialAccount`.
- **Polling fallback:** Cron/worker every 5–10 minutes per account, using `lastSyncedAt` + pagination cursors.
- **Backfill:** On connect, pull last N days (configurable, default 30) to populate history.

### Ingestion Pipeline
1) Fetch raw posts (platform SDK/HTTP).
2) Normalize into `SocialPost` shape; derive `externalPostId` and `permalink`.
3) Deduplicate by `(platform, externalPostId)`, upsert metrics and media.
4) Enrich with AI: classify post type, extract entities (dates/venues/cities/people), generate 1–2 sentence summary, produce short tags.
5) Linkage rules:
   - If a date/venue is detected and matches an upcoming `SocialEvent`, attach `eventId`.
   - If caption mentions "new single/album/EP" or links to streaming, tag as `release`.
6) Persist `SocialPost` and record `SocialSyncRun`.

### Delivery Surfaces
- **Artist profile (public + internal):** show latest posts with filters by tag (release/show/merch/press).
- **CRM Party timeline:** surface posts on the artist's party timeline for A&R/marketing context.
- **Social Events:** when a post is tagged as `show`, suggest/create a `SocialEvent` draft.
- **Exports:** CSV/JSON export and clipboard copy of last 30 posts.

## API Surface (proposed)
- `POST /social-sync/oauth/:platform/start` → returns auth URL.
- `POST /social-sync/oauth/:platform/callback` → exchanges code, stores tokens, kicks off backfill.
- `GET /artists/:id/social-posts?platform=&tag=&after=` → paginated posts for artist.
- `POST /social-sync/ingest/:platform` (admin/worker) → manual ingest hook for cron/webhooks.
- `GET /social-sync/accounts` → list connected accounts with status/lastSyncedAt.
- `DELETE /social-sync/accounts/:id` → revoke and delete tokens.
- Webhooks per platform: `POST /social-sync/webhooks/:platform` with verification and signature checks.

## Platform Notes
- **Instagram (Graph API):** needs `instagram_basic`, `pages_show_list`, `instagram_manage_insights`; store `igUserId` and `pageId`; webhook uses verify token + HMAC.
- **TikTok:** use Business API; capture `video_id`, `cover_image_url`, `share_url`, `create_time`, stats; rate-limit aware.
- **LinkedIn:** use Share API for member/organization posts; capture `urn`, `text`, `media`, `lifecycleState`; obey 2-legged vs 3-legged tokens.

## Configuration & Env Vars (suggested)
- IG: `IG_APP_ID`, `IG_APP_SECRET`, `IG_WEBHOOK_SECRET`, `IG_VERIFY_TOKEN`.
- TikTok: `TIKTOK_CLIENT_ID`, `TIKTOK_CLIENT_SECRET`, `TIKTOK_WEBHOOK_SECRET` (if available).
- LinkedIn: `LINKEDIN_CLIENT_ID`, `LINKEDIN_CLIENT_SECRET`, `LINKEDIN_WEBHOOK_SECRET`.
- Generic: `SOCIAL_SYNC_MAX_BACKFILL_DAYS` (default 30), `SOCIAL_SYNC_POLL_INTERVAL_SEC` (default 300), `SOCIAL_SYNC_LLM_MODEL` (e.g., `gpt-4o-mini`), `SOCIAL_SYNC_LLM_API_URL`, `SOCIAL_SYNC_LLM_API_KEY`.
- Frontend flags: `VITE_SOCIAL_SYNC_ENABLED=true`, `VITE_SOCIAL_SYNC_PLATFORM_FILTER=instagram,tiktok,linkedin`.

## LLM Enrichment Prompt (sketch)
- Input: platform, caption, hashtags, permalink, postedAt, media types, metrics, detected entities (pre-LLM).
- Output: `{ summary, tags:[release|show|merch|press|other], entities:{dates, venues, cities, people}, confidence }`.
- Guardrails: never invent links; keep summary ≤ 2 sentences; mark `confidence=low` if unsure.

## Observability and Ops
- Metrics: per-platform ingest latency, posts/sec, dedupe hits, LLM failures, token refresh failures.
- Alerts: token nearing expiry, webhook signature failures, repeated 429s.
- Logs: per `SocialSyncRun` with counts and sample permalinks; redact tokens.
- Admin tools: force backfill (days/window), retry failed runs, view webhook deliveries.

## Security & Privacy
- Encrypt tokens at rest; never log them.
- Per-artist scoping: only the owning artist and admins can view/manage their social posts.
- Handle revocation gracefully: mark account `status=disconnected`, stop polling, keep history read-only.
- Respect platform terms: store only allowed fields; obey deletion callbacks if platforms request data removal.

## Testing Plan (high level)
- Unit: normalize/dedupe per platform sample payloads; LLM prompt formatting; classifier output mapping.
- Integration (mocked): OAuth callback flow per platform; webhook signature verification; backfill window logic.
- E2E (staged tokens): connect → backfill → webhook-delivered post → UI shows summary/tags and links event suggestions.

## Open Questions
- Should merch posts create items in inventory/merch catalog automatically or stay as suggestions?
- How far back should default backfill go for power users (90 days?) given rate limits?
- Do we expose cross-posting (schedule from TDF) or stay read-only for now?
