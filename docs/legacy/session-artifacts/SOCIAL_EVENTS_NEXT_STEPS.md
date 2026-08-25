# Quick Next Steps: OpenAPI & TypeScript Client Generation

## Prerequisite: Check if Stack can build
```bash
cd /Users/diegosaa/GitHub/tdf-app/tdf-hq
stack build 2>&1 | head -100
```
This will show compilation errors if any exist in the Haskell code.

## Generate OpenAPI Spec

Once the build succeeds, the OpenAPI spec is typically auto-generated or located in:
```bash
# Check for existing docs folder
ls -la /Users/diegosaa/GitHub/tdf-app/tdf-hq/docs/
ls -la /Users/diegosaa/GitHub/tdf-app/docs/openapi/
```

## Generate TypeScript Clients

If the project has npm scripts configured for client generation:
```bash
cd /Users/diegosaa/GitHub/tdf-app

# Check package.json for generate:api scripts
grep "generate:api" package.json

# Run generation (examples - adapt to actual project)
npm run generate:api:ui
npm run generate:api:mobile
```

If not configured, manually use OpenAPI generator:
```bash
npm install -g @openapitools/openapi-generator-cli

openapi-generator-cli generate \
  -i /path/to/openapi.yaml \
  -g typescript-axios \
  -o tdf-hq-ui/src/api/generated

openapi-generator-cli generate \
  -i /path/to/openapi.yaml \
  -g typescript-axios \
  -o tdf-mobile/src/api/generated
```

## Manual Method: Use curl to Download spec

If unclear where the OpenAPI spec lives, visit the running API:
```bash
cd /Users/diegosaa/GitHub/tdf-app/tdf-hq
make up  # Start containers if not running

curl http://localhost:8080/swagger.json > /tmp/openapi.json
# or
curl http://localhost:8080/docs/openapi.yaml > /tmp/openapi.yaml
```

## After Generation: Wire Mobile

1. Verify generated files exist in `tdf-mobile/src/api/generated/`
2. Update mobile screens to import and use generated hooks
3. Remove manual `api.ts` modules and replace with:
   ```typescript
   import { useEventsQuery, useCreateEventMutation } from '../api/generated/hooks'
   ```

---

**Blocker**: If `stack build` fails, review compilation errors and address issues before proceeding.

**Status**: Backend is ready; only needs OpenAPI spec generation and mobile client wiring.
