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

## Best Practices Going Forward

### Use Environment Variables
```bash
# Copy example files
cp tdf-hq/config/default.env tdf-hq/.env
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
