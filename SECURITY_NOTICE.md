# ⚠️ SECURITY NOTICE

## Sensitive Files Moved to Archives

The following sensitive files were found in the repository and have been moved to the `archives/` directory:

- **OAuth Client Secrets** - Google OAuth credentials
- **Database Credentials** - Neon database connection strings
- **Environment Files** - Any committed .env files

## Action Required

1. **Review `archives/` directory** - Check for any sensitive data
2. **Rotate compromised credentials:**
   - Generate new OAuth client secrets in Google Cloud Console
   - Rotate database passwords
   - Update any API keys that may have been exposed
3. **Delete archives/** after extracting needed reference data
4. **Never commit secrets again** - All patterns now in `.gitignore`

## Security Audit Findings (2026-02-21)

The following vulnerabilities were identified and **fixed** in this revision:

### Fixed

| Severity | Issue | Fix |
|----------|-------|-----|
| Critical | **CORS bypass**: `allowAll = True` was hardcoded in `Cors.hs`, ignoring all configuration and allowing requests from any origin. | Removed the hardcoded `True ||` — CORS is now controlled entirely by `ALLOW_ALL_ORIGINS` / `ALLOW_ORIGINS` env vars. |
| High | **Production CORS open**: `fly.toml` set `ALLOW_ALL_ORIGINS = "true"`, exposing the live API to cross-origin requests from any website. | Removed the `ALLOW_ALL_ORIGINS` line from `fly.toml`; allowed origins are now restricted to the configured `ALLOW_ORIGINS` value. |
| High | **Auto-seeding in production**: `SEED_DB` defaulted to `true`, causing the app to insert demo accounts (username `admin`, password `password123`) on every fresh deploy without any explicit opt-in. | Changed the compile-time default to `false`. Seeding must now be opted in by setting `SEED_DB=true`. |
| High | **Known seed trigger token**: `SEED_TRIGGER_TOKEN` fell back to the hardcoded well-known value `tdf-bootstrap-seed` when not set, allowing unauthenticated callers who know this value to trigger data seeding. | Changed the default to empty (disabled). The seed endpoint now returns `403 Forbidden` unless a non-empty secret is explicitly set via `SEED_TRIGGER_TOKEN`. |

### Remaining Risks (not fixed here — require broader refactoring)

| Severity | Issue | Recommendation |
|----------|-------|----------------|
| Medium | **Session token in `localStorage`**: The API bearer token is stored in `localStorage`, making it readable by any JavaScript running on the same origin (XSS risk). | Migrate to `HttpOnly` cookies with `SameSite=Strict` and a server-side session invalidation endpoint. |
| Medium | **Weak passwords in seed data**: Dev seed accounts (`admin`, `manager`, etc.) use `password123`/`changeme123`. If `SEED_DB=true` is used in a shared environment these credentials are trivially guessable. | Always change seed passwords before exposing the environment publicly. Do not run with `SEED_DB=true` in production. |
| Low | **No rate limiting on auth endpoints**: `/login`, `/signup`, and `/v1/password-reset` have no brute-force protection. | Add a WAF rule or reverse-proxy rate limit (e.g., nginx `limit_req`, Fly.io rate limiting, or a Haskell middleware). |
| Low | **Warp exception CORS headers use `*`**: Malformed HTTP requests that trigger Warp's own exception handler receive `Access-Control-Allow-Origin: *`, bypassing the CORS policy for that narrow error path. | Evaluate whether a custom Warp `onExceptionResponse` that echoes the request `Origin` is needed. |

## Best Practices Going Forward

### Use Environment Variables
```bash
# Copy example files
cp tdf-hq/config/default.env.example tdf-hq/.env
cp tdf-hq-ui/.env.example tdf-hq-ui/.env

# Edit with your local values (never commit these)
```

### For OAuth Secrets
Store in:
- Local environment variables
- Secure secret management (e.g., GitHub Secrets, Vault)
- Password managers for team sharing

### For Database Credentials
- Use connection string environment variables
- Keep separate credentials for dev/staging/production
- Use least-privilege database users

## Verification

Run this to ensure no secrets in git history:
```bash
git log --all --full-history --source -- '*secret*.json'
git log --all --full-history --source -- '*env*'
```

If this is the initial commit, you're safe. If not, consider:
- BFG Repo-Cleaner to remove from history
- Treating all exposed credentials as compromised

---
Created: 2025-11-05
Last updated: 2026-02-21
