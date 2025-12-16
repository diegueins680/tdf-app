# Haskell Best Practices Applied

This document summarizes best practice improvements made to the TDF Records Haskell backend.

## 1. Eliminated Partial Functions

**Issue**: Unsafe use of `head` function which crashes on empty lists.

**Location**: `src/TDF/Trials/Server.hs:888`

**Before**:
```haskell
dtos <- buildClassSessionDTOs [ent]
pure (head dtos)  -- Unsafe: crashes on empty list
```

**After**:
```haskell
dtos <- buildClassSessionDTOs [ent]
case listToMaybe dtos of
  Just dto -> pure dto
  Nothing -> liftIO $ throwIO err500  -- Handle empty case explicitly
```

**Rationale**: Partial functions like `head`, `tail`, and `!!` should be avoided. Pattern matching with `listToMaybe` is safer and more idiomatic.

---

## 2. Enhanced Compiler Warnings

**Location**: `package.yaml`

**Changes**:
- Added `-Wmissing-signatures` flag to library build options
- Added `-Wmissing-export-lists` flag to library build options

**Rationale**:
- `-Wmissing-signatures`: Ensures all top-level functions have explicit type signatures, improving code documentation and catch errors early
- `-Wmissing-export-lists`: Forces explicit export lists in modules, preventing accidental public APIs and making modules more maintainable

**Benefits**:
- Better code clarity and documentation
- Catch more potential bugs at compile time
- Makes refactoring safer

---

## 3. Code Quality Guidelines

### Type Signatures
- All top-level functions should have explicit type signatures
- Type signatures serve as inline documentation
- Enables compiler to catch type errors earlier

### Import Management
- Use qualified imports for large libraries (e.g., `import qualified Data.Text as T`)
- Group imports logically
- Avoid duplicate imports from different modules

### Error Handling
- Prefer `Maybe`/`Either` over partial functions
- Use `listToMaybe` instead of `head`
- Use `throwIO` with proper error responses instead of `error` or `fail`
- Provide meaningful error responses to API clients

### Record Fields
- Leverage `NamedFieldPuns` extension for cleaner pattern matching
- Use `RecordWildCards` when appropriate
- Avoid `DuplicateRecordFields` when possible (use newtype wrappers)

---

## 4. Module Organization

Current structure follows convention:
```
src/TDF/
├── API/           # API type definitions
├── Models/        # Database models
├── DTO/           # Data transfer objects
├── Server/        # Request handlers
├── DB/            # Database utilities
└── Services/      # Business logic
```

**Best Practices**:
- One public API per module
- Use `-Wmissing-export-lists` to explicitly define module boundaries
- Keep database queries in `DB` or dedicated query modules
- Keep business logic in `Services` or handler modules

---

## 5. Persistent ORM Best Practices

### Enum Patterns
```haskell
data PaymentMethod = CashM | BankTransferM | CardPOSM
  deriving (Show, Read, Eq, Ord, Generic)

derivePersistField "PaymentMethod"
```

**Benefits**: Strongly-typed enums with pattern matching

### Database Queries
- Use `selectList` with filter conditions instead of manual filtering
- Leverage `leftJoin` for complex queries
- Use migrations for schema changes (handled by Persistent)

### Error Handling
- Use `getJust` when you're certain the record exists
- Use `get` and pattern match for optional records
- Return proper HTTP errors (err400, err404, err500) to clients

---

## 6. Servant API Best Practices

### Type Safety
- Define API routes as type-level descriptions
- Use phantom types to enforce constraints at compile time
- Leverage Servant's error handling with `throwError`

### CORS Handling
- Implement fallback CORS middleware for exception responses
- Ensure CORS headers are applied to all responses including errors
- Use environment variables to control CORS origins in production

### Request/Response
- Use DTOs for API responses to decouple API from internal models
- Validate inputs at the boundary
- Provide consistent error response formats

---

## 7. Testing & Development

### REPL Development
```bash
stack ghci  # Interactive development and testing
```

### Type Checking
- Use GHCi's `:type` command to verify types
- Enable all warnings to catch issues early
- Use `stack build --ghc-options="-Werror"` to fail on warnings

### Common Issues
- **Database connection**: Use `makePoolWithRetry` for resilient startup
- **Migrations**: Automatic with Persistent; use `RESET_DB=true` for full reset
- **Admin endpoints**: Restrict to development only

---

## 8. Configuration & Environment

### Best Practices
- Use environment variables for configuration (`.env` file)
- Never commit secrets to version control
- Provide clear `.env.example` with required keys
- Load configuration at startup and fail fast if incomplete

### Current Setup
- `config/default.env` - Default values
- `.env` - Local overrides (gitignored)
- `loadConfig` in `TDF.Config` - Centralized loading

---

## Compiler Flags Reference

### Current Enabled Warnings

| Flag | Purpose |
|------|---------|
| `-Wall` | Enable all warnings |
| `-Wcompat` | Compatibility warnings |
| `-Wincomplete-uni-patterns` | Incomplete patterns |
| `-Wincomplete-record-updates` | Record update warnings |
| `-Wredundant-constraints` | Unused constraints |
| `-Wmissing-signatures` | Missing type signatures (new) |
| `-Wmissing-export-lists` | Missing export lists (new) |

### Recommended Future Additions

- `-Wunused-imports` - Find unused imports
- `-Wunused-binds` - Find unused bindings
- `-Worphans` - Warn about orphan instances
- `-Werror` - Fail on any warning (optional, in CI/CD)

---

## References

- [Haskell Style Guide](https://github.com/tibbe/haskell-style-guide)
- [Servant Documentation](https://docs.servant.dev/)
- [Persistent ORM Guide](https://www.yesodweb.com/book/persistent)
- [Haskell Error Handling](https://wiki.haskell.org/Error_handling)
