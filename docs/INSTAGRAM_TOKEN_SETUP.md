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
| `INSTAGRAM_APP_ID` | Instagram App ID (default: 1206294904899273) | ❌ Optional |
| `FLY_APP_NAME` | Fly.io app name (default: tdf-hq) | ❌ Optional |
| `FLY_API_TOKEN` | Fly.io API token for deployments | ✅ Yes |
| `SLACK_WEBHOOK_URL` | Slack webhook for failure alerts | ❌ Optional |

### 2. Initial Token Exchange

Run the setup command to exchange your short-lived token for a long-lived token:

```bash
# Set environment variables
export INSTAGRAM_ACCESS_TOKEN="your-short-lived-token"
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
gh workflow run refresh-instagram-token.yml --repo diegueins680/tdf-app
```

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

### "Failed to update Fly secret" errors
- Verify `FLY_API_TOKEN` is valid
- Check Fly CLI is installed: `flyctl version`
- Ensure you have access to the app: `flyctl apps list`

## Security Notes

- Tokens are never logged in full (only first 10 chars)
- Token state file (`.instagram-token-state.json`) is gitignored
- All secrets are stored in GitHub Secrets or Fly.io secrets
- The script uses HTTPS for all API calls

## Files

- `scripts/refresh-instagram-token.mjs` - Main automation script
- `.github/workflows/refresh-instagram-token.yml` - GitHub Actions workflow
- `.instagram-token-state.json` - Local token state (auto-generated, gitignored)
