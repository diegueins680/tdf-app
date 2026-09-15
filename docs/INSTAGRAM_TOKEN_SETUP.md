# Instagram Token Refresh Automation

## Overview

This automation handles Instagram access token lifecycle:
- **Initial setup**: Exchange short-lived token for long-lived token
- **Periodic refresh**: Refresh token before expiration (every 30 days)
- **Health checks**: Monitor token status and alert on issues

## Architecture

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│  GitHub Actions │────▶│  Token Script    │────▶│   Fly.io App    │
│  (Scheduled)    │     │  (Node.js)       │     │  (tdf-hq)       │
└─────────────────┘     └──────────────────┘     └─────────────────┘
                               │
                               ▼
                        ┌──────────────────┐
                        │  Meta Graph API  │
                        │  (Instagram)     │
                        └──────────────────┘
```

## Setup Instructions

### 1. Store Secrets

Add these secrets to your GitHub repository (Settings → Secrets and variables → Actions):

| Secret | Value | Required |
|--------|-------|----------|
| `INSTAGRAM_ACCESS_TOKEN` | Current token (short or long-lived) | ✅ Yes |
| `INSTAGRAM_APP_SECRET` | Instagram App Secret from Meta Dashboard | ✅ Yes |
| `INSTAGRAM_APP_ID` | App ID for the Meta app that issued the access token | ✅ Yes |
| `FACEBOOK_APP_ID` | Parent Meta app ID used to authenticate the Facebook debugger | ✅ For checks |
| `FACEBOOK_APP_SECRET` | Matching parent Meta app secret, distinct from the Instagram secret | ✅ For checks |
| `FLY_APP_NAME` | Fly.io app name (default: tdf-hq) | ❌ Optional |
| `FLY_API_TOKEN` | Fly.io API token for deployments | ✅ Yes |
| `SLACK_WEBHOOK_URL` | Slack webhook for failure alerts | ❌ Optional |

### 2. Initial Token Exchange

Run the setup command to exchange your short-lived token for a long-lived token:

```bash
# Set environment variables
export INSTAGRAM_ACCESS_TOKEN="your-short-lived-token"
export INSTAGRAM_APP_ID="your-app-id"
export INSTAGRAM_APP_SECRET="your-app-secret"
export FACEBOOK_APP_ID="your-parent-meta-app-id"
export FACEBOOK_APP_SECRET="your-parent-meta-app-secret"

# Run setup
node scripts/refresh-instagram-token.mjs --setup
```

This will:
1. Exchange the short-lived token for a 60-day long-lived token
2. Update the Fly.io secret
3. Restart the app
4. Save token state locally

### 3. GitHub Actions Automation

The workflow runs automatically every 30 days. You can also trigger it manually:

```bash
# Via GitHub UI
# Actions → Refresh Instagram Token → Run workflow

# Via GitHub CLI
gh workflow run refresh-instagram-token.yml \
  --repo diegueins680/tdf-app \
  --ref main \
  -f action=check
```

The `check` action only validates the configured token. The `setup` and `refresh`
actions update the Fly.io secret and restart the application, so run them only as
an approved credential-maintenance operation.

### 4. Manual Commands

```bash
# Check token status
node scripts/refresh-instagram-token.mjs --check

# Refresh token now
node scripts/refresh-instagram-token.mjs --refresh

# Setup (exchange for long-lived)
node scripts/refresh-instagram-token.mjs --setup
```

## Messaging token checks (separate workflow)

`Check Messaging Token` manages `INSTAGRAM_MESSAGING_TOKEN` and
`FACEBOOK_MESSAGING_TOKEN`, not `INSTAGRAM_ACCESS_TOKEN` above. Its manual
`action=check` is read-only:

```bash
node scripts/check-messaging-token.mjs --check
gh workflow run check-messaging-token.yml \
  --repo diegueins680/tdf-app --ref REVIEWED_REF_WITH_READ_ONLY_CHECK -f action=check
```

Replace the ref placeholder only with a reviewed revision containing this fix.
Until it is merged, do not dispatch this workflow on the old `main`: its manual
`check` action still runs credential maintenance. The CLI flag is also unsafe on
older revisions that silently ignore arguments.

Both tokens must pass the existing health checks without needing maintenance.
Missing, invalid, expired, soon-expiring tokens or failed provider checks return
a nonzero status; read-only mode never exchanges tokens, retrieves replacement
Page tokens, or calls Fly. In Actions, this step receives only the messaging
tokens and Meta inspector credentials; Fly credentials and CLI installation are
limited to maintenance. Failure notifications retain their existing behavior.

The hourly schedule and explicit `action=refresh` retain the existing
refresh-when-needed behavior, including final verification and failure exits.
They invoke `node scripts/check-messaging-token.mjs` without arguments, which can
exchange tokens and update both Fly messaging secrets. Manual `refresh` is not
an unconditional rotation and requires separate production-maintenance approval.
Unknown CLI arguments and workflow actions fail instead of falling through to
maintenance. Do not pass token values as arguments or paste them into logs.

No schema migration or application deployment is needed for this change.
Reverting it restores the old, potentially mutating manual `check` behavior;
stop using manual checks on a reverted revision. Reverting code does not undo
any separately authorized credential update.

## Token Lifecycle

```
Short-lived Token (1 hour)
    │
    ▼
Exchange ──────────────────▶ Long-lived Token (60 days)
    │                              │
    │                              │
    │                    Refresh after 30 days
    │                              │
    │                              ▼
    │                    New Long-lived Token (60 days)
    │                              │
    └──────────────────────────────┘
         (Repeat every 30 days)
```

## Monitoring

The script provides detailed logging:
- Token validity status
- Days until expiration
- Scope permissions
- Refresh history

Check the GitHub Actions logs for automated runs, or run locally with `--check`.

## Troubleshooting

### "Token expired" errors
- Run `node scripts/refresh-instagram-token.mjs --refresh`
- Or trigger the GitHub Actions workflow manually

### "Invalid token" errors
- Verify `INSTAGRAM_ACCESS_TOKEN` is set correctly
- Check that the Instagram account hasn't been disconnected
- Re-run setup: `node scripts/refresh-instagram-token.mjs --setup`

### "Error validating application" / API code 190
- The non-mutating check first verifies Instagram account access with the documented [Instagram Login `/me` request](https://developers.facebook.com/documentation/instagram-platform/instagram-api-with-instagram-login/get-started), then validates token metadata. An expired or revoked Instagram token now fails before app authentication can obscure the cause. A successful account request does not bypass metadata validation.
- If the first request reports an expired session, obtain a replacement token through the approved Instagram login process; app-ID changes or retries cannot renew an expired token. Never disclose tokens in logs or issues.
- Ensure the repository-level Actions secret `INSTAGRAM_APP_ID` is configured; the workflow intentionally has no hard-coded fallback
- Verify that `INSTAGRAM_APP_ID` and `INSTAGRAM_APP_SECRET` belong to the same Meta app
- The [Facebook token debugger](https://developers.facebook.com/docs/graph-api/reference/debug_token/) requires an app access token for the associated Meta application. The checker uses `FACEBOOK_APP_ID`/`FACEBOOK_APP_SECRET` for that inspector, while retaining the Instagram child ID/secret for Instagram Login and token exchange. For TDF, these are the parent **TDF Bot** and child **TDF Bot-IG** respectively; do not overwrite one pair with the other.
- A successful debugger response must identify the configured Instagram app and include an authoritative expiration. Expired token or data-access deadlines fail, even if `is_valid` is true. Missing expiry is not treated as a never-expiring token.
- Verify that `INSTAGRAM_ACCESS_TOKEN` was issued for that app and has not been revoked
- Replace the affected repository secrets through GitHub's secret settings; never paste their values into logs or issues

### "The session has been invalidated"

This response from the Instagram account request means the stored token has been
invalidated, for example by a password change or a Meta security action. A retry,
app-ID change, or refresh of that invalidated token cannot restore the session.
An authorized account owner must reauthorize the Instagram professional account
through its configured Instagram Login integration and securely replace the
repository's `INSTAGRAM_ACCESS_TOKEN`. Do not substitute a parent Facebook app's
credentials for the Instagram integration's credentials.

Run `Refresh Instagram Token` with `action=check` on the correction branch after
the replacement. Both account access and token-metadata validation must pass.
Only then follow the normal approved deployment process to update any runtime
consumer. Rolling back application code does not restore an invalidated token.
- Run the workflow with `action=check` and confirm the `Check/Refresh Token` step succeeds before authorizing `setup` or `refresh`

### "Failed to update Fly secret" errors
- Verify `FLY_API_TOKEN` is valid
- Check Fly CLI is installed: `flyctl version`
- Ensure you have access to the app: `flyctl apps list`

## Security Notes

- Tokens and token prefixes are never logged
- Token state file (`.instagram-token-state.json`) is gitignored
- All secrets are stored in GitHub Secrets or Fly.io secrets
- The script uses HTTPS for all API calls
- Meta authentication errors fail closed and are not retried as transient outages

## Files

- `scripts/refresh-instagram-token.mjs` - Main automation script
- `.github/workflows/refresh-instagram-token.yml` - GitHub Actions workflow
- `.instagram-token-state.json` - Local token state (auto-generated, gitignored)
