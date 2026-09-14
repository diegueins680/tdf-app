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
- Verify that `INSTAGRAM_ACCESS_TOKEN` was issued for that app and has not been revoked
- Replace the affected repository secrets through GitHub's secret settings; never paste their values into logs or issues
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
