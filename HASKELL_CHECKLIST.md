# Haskell Code Review Checklist

Use this checklist when reviewing or writing Haskell code for TDF Records.

## Type Safety ✓

- [ ] All top-level functions have explicit type signatures
- [ ] No use of partial functions (`head`, `tail`, `!!`)
- [ ] `Maybe` / `Either` used for optional values instead of `null` checks
- [ ] Record types use `deriving (Generic)` for JSON serialization
- [ ] Enum types use `derivePersistField` for database storage

## Error Handling ✓

- [ ] Database queries use `get` + pattern match (not `head`)
- [ ] API errors return appropriate HTTP status codes (err400, err404, err500)
- [ ] No bare `error` or `fail` calls in production code
- [ ] Exception handling uses `catch` or `throwIO` with proper error responses
- [ ] User-facing errors are meaningful, not stack traces

## Module Organization

- [ ] Module has explicit export list (use `-Wmissing-export-lists`)
- [ ] Imports are organized: local, then standard library, then external
- [ ] Large imports use qualified names (e.g., `import qualified Data.Text as T`)
- [ ] No circular dependencies
- [ ] Public API separated from implementation details

## Data Types & Records

- [ ] Record fields are clearly named (avoid single letters)
- [ ] Use `NamedFieldPuns` for pattern matching on records
- [ ] Use `RecordWildCards` when applicable
- [ ] Type constructors are descriptive (e.g., `PaymentMethod` not `PM`)
- [ ] Enums have explicit show/read instances

## Database (Persistent ORM)

- [ ] Models defined with proper field types and constraints
- [ ] Foreign keys use `Key PartyId` instead of `Int64`
- [ ] Queries use type-safe filters (e.g., `[PartyId ==. pid]`)
- [ ] Migrations run automatically; use `RESET_DB=true` for full reset
- [ ] Database queries return appropriate types (`Maybe`, lists, etc.)

## API & Servant

- [ ] API types defined in `TDF.API.*` modules
- [ ] DTOs separate database models from API responses
- [ ] Authentication checked with `hasModuleAccess` or custom guards
- [ ] CORS headers included in all responses (including errors)
- [ ] Health check endpoint returns JSON status

## Testing & Verification

- [ ] Code compiles with `stack build --fast`
- [ ] Code passes with all warnings enabled
- [ ] Functions tested in REPL (`stack ghci`)
- [ ] Database queries tested with seed data
- [ ] API endpoints tested with `curl` commands

## Common Pitfalls to Avoid

| Antipattern | Correct Pattern |
|-------------|-----------------|
| `head list` | `listToMaybe list` or `case list of ...` |
| `error "msg"` | `throwIO (toException err500)` |
| `foo = undefined` | Either implement or use `_` in pattern |
| `if x then Just a else Nothing` | `guard (condition) >> pure value` |
| `map head lists` | `mapMaybe listToMaybe lists` |
| No type signature | Always add `functionName :: Type -> Type -> Result` |
| `DuplicateRecordFields` | Use newtype wrappers instead |
| No export list | Add `module Foo (Bar, baz) where` |

## Performance Considerations

- [ ] Use `Data.Text` instead of `String` for text data
- [ ] Use `Data.ByteString` for binary data
- [ ] Avoid building large lists; prefer streaming
- [ ] Use `-O2` compiler flag for production builds
- [ ] Profile with `+RTS -s` flag if needed

## Documentation

- [ ] Modules have top-level comments explaining purpose
- [ ] Complex algorithms explained with comments
- [ ] Export lists are complete and ordered
- [ ] Function types are self-documenting (good naming)
- [ ] Error cases documented in types or comments

## Git & Version Control

- [ ] Commits are atomic and well-described
- [ ] Tests pass before committing
- [ ] No sensitive data (credentials, tokens) in commits
- [ ] Branch names follow convention: `feature/...`, `fix/...`, `refactor/...`
- [ ] Pull requests include description of changes

---

## Quick Commands

```bash
# Type check without building
stack ghci

# Build with fast option
stack build --fast

# Run main app
stack run

# Run tests
stack test

# Interactive REPL
stack ghci

# Show function type
:type functionName

# Reload and re-evaluate
:reload
```

---

## Resources

- Module conventions: See `src/TDF/*.hs` files
- API patterns: See `src/TDF/API/*.hs` files
- Database patterns: See `src/TDF/DB.hs` and `src/TDF/Models.hs`
- Error handling: See `Main.hs` and server error responses
