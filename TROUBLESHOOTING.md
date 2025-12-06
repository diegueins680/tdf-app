# Troubleshooting Guide - TDF Records Platform

This guide covers common issues and their solutions for all components of the TDF platform.

## Table of Contents

- [Backend Issues](#backend-issues)
- [Frontend Issues](#frontend-issues)
- [Mobile App Issues](#mobile-app-issues)
- [Database Issues](#database-issues)
- [API & CORS Issues](#api--cors-issues)
- [Build & Deployment Issues](#build--deployment-issues)
- [Development Environment](#development-environment)

---

## Backend Issues

### Backend Won't Start

#### Symptom
```
Error: could not connect to database
```

**Solutions:**

1. **Check PostgreSQL is running**
   ```bash
   # Linux
   sudo systemctl status postgresql
   
   # macOS
   brew services list
   
   # Check if port is open
   lsof -i :5432
   ```

2. **Verify database credentials**
   ```bash
   cd tdf-hq
   cat .env | grep DB_
   
   # Test connection
   psql -h $DB_HOST -U $DB_USER -d $DB_NAME
   ```

3. **Check database exists**
   ```bash
   psql -h localhost -U postgres -c "SELECT datname FROM pg_database;"
   
   # Create if missing
   createdb -h localhost -U postgres tdf_hq
   ```

4. **Verify Stack is configured**
   ```bash
   cd tdf-hq
   stack --version  # Should show Stack version
   stack setup      # Install GHC if needed
   stack build      # Should succeed
   ```

#### Symptom
```
Port 8080 already in use
```

**Solutions:**

1. **Find and kill process using port**
   ```bash
   # Find process
   lsof -i :8080
   
   # Kill process
   kill -9 <PID>
   
   # Or use different port
   export APP_PORT=8081
   stack run
   ```

2. **Check for other instances**
   ```bash
   ps aux | grep tdf-hq
   killall tdf-hq
   ```

### Backend Build Fails

#### Symptom
```
Error: Missing dependencies
```

**Solutions:**

1. **Clean and rebuild**
   ```bash
   cd tdf-hq
   stack clean
   stack build --force-dirty
   ```

2. **Update resolver**
   ```bash
   # Edit stack.yaml if needed
   stack update
   stack build
   ```

3. **Check system dependencies**
   ```bash
   # Ubuntu/Debian
   sudo apt-get install libpq-dev

   # macOS
   brew install postgresql
   ```

### Backend Runtime Errors

#### Symptom
```
Exception: connection to server at "localhost", port 5432 failed
```

**Solutions:**

1. **Check PostgreSQL is accessible**
   ```bash
   pg_isready -h localhost -p 5432
   ```

2. **Verify pg_hba.conf allows connections**
   ```bash
   # Find pg_hba.conf
   sudo find / -name pg_hba.conf 2>/dev/null
   
   # Should have line like:
   # host    all    all    127.0.0.1/32    md5
   ```

3. **Check PostgreSQL logs**
   ```bash
   # Linux
   sudo tail -f /var/log/postgresql/postgresql-*.log
   
   # macOS
   tail -f /usr/local/var/log/postgres.log
   ```

---

## Frontend Issues

### Frontend Won't Start

#### Symptom
```
Error: Cannot find module 'vite'
```

**Solutions:**

1. **Install dependencies**
   ```bash
   cd tdf-hq-ui
   rm -rf node_modules
   npm install
   ```

2. **Check Node version**
   ```bash
   node --version  # Should be 18+
   npm --version   # Should be 9+
   
   # Update if needed
   nvm install 18
   nvm use 18
   ```

### API Connection Failed

#### Symptom
```
NetworkError: Failed to fetch
```

**Solutions:**

1. **Verify backend is running**
   ```bash
   curl http://localhost:8080/health
   
   # Should return:
   # {"status":"ok","database":"connected"}
   ```

2. **Check VITE_API_BASE**
   ```bash
   cd tdf-hq-ui
   cat .env | grep VITE_API_BASE
   
   # Should match backend URL
   # Example: VITE_API_BASE=http://localhost:8080
   ```

3. **Restart dev server after .env changes**
   ```bash
   # Vite requires restart for .env changes
   npm run dev
   ```

### White Screen After Build

#### Symptom
- Frontend builds successfully
- Shows white screen in production
- Browser console shows errors

**Solutions:**

1. **Check browser console**
   - Open DevTools (F12)
   - Look for JavaScript errors
   - Check Network tab for failed requests

2. **Verify environment variables**
   ```bash
   # Cloudflare Pages/Vercel must have these set
   VITE_API_BASE=https://your-api.koyeb.app
   VITE_TZ=America/Guayaquil
   ```

3. **Test build locally**
   ```bash
   cd tdf-hq-ui
   npm run build
   npm run preview  # Serves production build locally
   ```

4. **Check for missing dependencies**
   ```bash
   npm install
   npm run build
   ```

### SPA Routing 404s

#### Symptom
- Direct URLs work
- Refresh on nested routes returns 404

**Solutions:**

1. **Cloudflare Pages**: Add `public/_redirects`
   ```
   /*    /index.html   200
   ```

2. **Vercel**: Add `vercel.json`
   ```json
   {
     "rewrites": [
       { "source": "/(.*)", "destination": "/index.html" }
     ]
   }
   ```

3. **Nginx**: Update config
   ```nginx
   location / {
     try_files $uri $uri/ /index.html;
   }
   ```

---

## Mobile App Issues

### Submodule is Empty

#### Symptom
```bash
cd tdf-mobile
ls -la
# Shows empty directory
```

**Solutions:**

1. **Initialize submodule**
   ```bash
   # From repository root
   git submodule update --init --recursive
   cd tdf-mobile
   ls -la  # Should now show files
   ```

2. **Update submodule to latest**
   ```bash
   git submodule update --remote
   ```

3. **Clone with submodules**
   ```bash
   # For fresh clones
   git clone --recursive <repository-url>
   ```

### Expo Won't Start

#### Symptom
```
Error: Metro bundler failed to start
```

**Solutions:**

1. **Clear cache and restart**
   ```bash
   cd tdf-mobile
   npx expo start --clear
   ```

2. **Check Node version**
   ```bash
   node --version  # Should be 18+
   ```

3. **Reinstall dependencies**
   ```bash
   rm -rf node_modules
   npm install
   ```

### API Connection from Mobile

#### Symptom
- API works in browser
- Fails from Expo Go app

**Solutions:**

1. **Use computer's IP, not localhost**
   ```bash
   # Find your IP
   # macOS/Linux
   ifconfig | grep inet
   
   # Update .env
   EXPO_PUBLIC_API_BASE=http://192.168.1.100:8080
   ```

2. **Ensure devices on same network**
   - Computer and phone must be on same WiFi
   - Corporate/public WiFi may block connections

3. **Check firewall**
   ```bash
   # macOS - allow incoming connections
   sudo /usr/libexec/ApplicationFirewall/socketfilterfw --add /path/to/node
   
   # Linux - allow port
   sudo ufw allow 8080
   ```

---

## Database Issues

### Connection Pool Exhausted

#### Symptom
```
Error: connection pool exhausted
```

**Solutions:**

1. **Increase pool size**
   ```env
   # In .env or backend config
   DB_POOL_SIZE=20  # Default is 10
   ```

2. **Check for connection leaks**
   ```sql
   -- Connect to database
   SELECT count(*) FROM pg_stat_activity WHERE datname = 'tdf_hq';
   ```

3. **Restart backend**
   ```bash
   cd tdf-hq
   stack clean
   stack run
   ```

### Migration Errors

#### Symptom
```
Error: relation already exists
```

**Solutions:**

1. **Check migration status**
   ```bash
   psql -d tdf_hq -c "SELECT version FROM schema_migrations;"
   ```

2. **Reset database (development only!)**
   ```bash
   # DANGER: Deletes all data!
   cd tdf-hq
   RESET_DB=true stack run
   ```

3. **Manual migration**
   ```bash
   cd tdf-hq
   psql -d tdf_hq -f sql/001_migration.sql
   ```

### Slow Queries

#### Symptom
- API responses are slow
- Database CPU high

**Solutions:**

1. **Check slow queries**
   ```sql
   SELECT query, mean_exec_time 
   FROM pg_stat_statements 
   ORDER BY mean_exec_time DESC 
   LIMIT 10;
   ```

2. **Add indexes**
   ```sql
   -- Example: Index frequently queried columns
   CREATE INDEX idx_party_email ON party(email);
   CREATE INDEX idx_booking_date ON booking(starts_at);
   ```

3. **Run VACUUM**
   ```sql
   VACUUM ANALYZE;
   ```

---

## API & CORS Issues

### CORS Blocked

#### Symptom
```
Access to fetch at 'https://api...' from origin 'https://app...' 
has been blocked by CORS policy
```

**Solutions:**

1. **Add origin to backend ALLOW_ORIGINS**
   ```env
   # Koyeb environment variables
   ALLOW_ORIGINS=https://your-app.pages.dev,https://your-domain.com
   ```

2. **Verify CORS headers**
   ```bash
   curl -I -X OPTIONS https://your-api.koyeb.app/parties \
     -H "Origin: https://your-app.pages.dev" \
     -H "Access-Control-Request-Method: GET"
   
   # Should see:
   # Access-Control-Allow-Origin: https://your-app.pages.dev
   ```

3. **Restart backend after env changes**
   - Koyeb: Redeploy service
   - Local: Restart `stack run`

### 401 Unauthorized

#### Symptom
```
Response: 401 Unauthorized
```

**Solutions:**

1. **Check auth token**
   ```typescript
   // In frontend
   console.log('Token:', localStorage.getItem('token'));
   ```

2. **Verify token format**
   ```bash
   # Should have Bearer prefix
   Authorization: Bearer <your-token>
   ```

3. **Check token expiration**
   ```bash
   # Decode JWT (online at jwt.io or use tool)
   echo <token> | base64 -d
   ```

### 500 Internal Server Error

#### Symptom
```
Response: 500 Internal Server Error
```

**Solutions:**

1. **Check backend logs**
   ```bash
   # Local
   stack run  # Watch console
   
   # Docker
   docker-compose logs -f app
   
   # Koyeb
   # View logs in dashboard
   ```

2. **Enable debug logging**
   ```env
   LOG_LEVEL=debug
   ```

3. **Test endpoint directly**
   ```bash
   curl -v https://your-api.koyeb.app/parties \
     -H "Authorization: Bearer <token>"
   ```

---

## Build & Deployment Issues

### Stack Build Fails

#### Symptom
```
Error: Setup: Missing dependencies
```

**Solutions:**

1. **Update resolver**
   ```bash
   cd tdf-hq
   stack update
   stack build
   ```

2. **Clean and rebuild**
   ```bash
   stack clean --full
   stack build
   ```

3. **Check cabal file**
   ```bash
   cat tdf-hq.cabal | grep dependencies
   # Ensure all packages are listed
   ```

### npm Build Fails

#### Symptom
```
Error: heap out of memory
```

**Solutions:**

1. **Increase Node memory**
   ```bash
   export NODE_OPTIONS="--max-old-space-size=4096"
   npm run build
   ```

2. **Clear cache**
   ```bash
   npm cache clean --force
   rm -rf node_modules package-lock.json
   npm install
   ```

### Cloudflare Build Fails

#### Symptom
- Build fails in Cloudflare Pages
- Works locally

**Solutions:**

1. **Check build settings**
   - Build command: `npm run build:ui`
   - Output directory: `tdf-hq-ui/dist`
   - Root directory: `.` (repository root)

2. **Check Node version**
   ```env
   # Add to Cloudflare environment variables
   NODE_VERSION=20.19.1
   ```

3. **Check build logs**
   - View in Cloudflare Pages dashboard
   - Look for specific error messages

---

## Development Environment

### Module Not Found (Haskell)

#### Symptom
```
Could not find module 'TDF.API'
```

**Solutions:**

1. **Rebuild from scratch**
   ```bash
   cd tdf-hq
   stack clean
   stack build
   ```

2. **Check module exists**
   ```bash
   ls -la src/TDF/API.hs
   ```

3. **Check cabal file lists module**
   ```bash
   grep "TDF.API" tdf-hq.cabal
   ```

### TypeScript Errors

#### Symptom
```
Cannot find name 'xxx'
```

**Solutions:**

1. **Regenerate API client**
   ```bash
   npm run generate:api:ui
   ```

2. **Check tsconfig.json**
   ```bash
   cat tsconfig.json | grep "paths"
   # Ensure paths are configured correctly
   ```

3. **Restart TypeScript server**
   - VS Code: Cmd+Shift+P → "TypeScript: Restart TS Server"

### Import Path Issues

#### Symptom
```
Module not found: Can't resolve '@/components/...'
```

**Solutions:**

1. **Check path mapping in vite.config.ts**
   ```typescript
   resolve: {
     alias: {
       '@': '/src'
     }
   }
   ```

2. **Use relative imports as fallback**
   ```typescript
   // Instead of:
   import { Button } from '@/components/Button';
   
   // Use:
   import { Button } from '../components/Button';
   ```

---

## Quick Diagnostic Commands

### Check All Services

```bash
# Backend health
curl http://localhost:8080/health

# Database connection
pg_isready -h localhost -p 5432

# Frontend dev server
curl http://localhost:5173

# Check ports in use
lsof -i :8080  # Backend
lsof -i :5432  # PostgreSQL
lsof -i :5173  # Frontend
```

### Reset Everything (Development)

```bash
# WARNING: Deletes all data!

# Stop all services
cd tdf-hq
make down  # If using Docker

# Drop and recreate database
dropdb tdf_hq
createdb tdf_hq

# Clean builds
cd tdf-hq
stack clean

cd ../tdf-hq-ui
rm -rf node_modules dist
npm install

# Restart with fresh seed
cd ../tdf-hq
RESET_DB=true SEED_DB=true stack run
```

---

## Getting Help

### Gather Information

Before asking for help, gather:

1. **Error messages** (full text)
2. **Stack traces** (if available)
3. **Environment details**
   ```bash
   node --version
   npm --version
   stack --version
   psql --version
   ```
4. **Steps to reproduce**
5. **Expected vs actual behavior**

### Where to Get Help

1. **Check documentation**
   - [DOCUMENTATION.md](DOCUMENTATION.md) - Documentation index
   - [DEVELOPMENT.md](DEVELOPMENT.md) - Development guide
   - [README.md](README.md) - Project overview

2. **Review existing issues**
   - Search GitHub issues
   - Check if already reported/resolved

3. **Ask the team**
   - Open new GitHub issue
   - Include gathered information
   - Tag with appropriate labels

---

## Common Patterns

### "It works on my machine"

**Checklist:**
- [ ] Same Node/Stack versions?
- [ ] Same environment variables?
- [ ] Same database state?
- [ ] Same Git branch/commit?
- [ ] Dependencies installed?
- [ ] Caches cleared?

### "It worked yesterday"

**Check:**
- Recent Git commits
- Dependency updates
- Environment variable changes
- Database schema changes
- External service changes (API endpoints)

---

**Still stuck?** Open an issue with full details and we'll help you out!
