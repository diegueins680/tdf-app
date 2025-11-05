# Contributing to TDF Records Platform

Thank you for contributing! This guide will help you get started.

## 🚦 Before You Start

1. **Read the docs:**
   - [README.md](./README.md) - Project overview
   - [DEVELOPMENT.md](./DEVELOPMENT.md) - Development workflow
   - [specs.yaml](./specs.yaml) - Business requirements

2. **Set up your environment:**
   - Follow the setup in DEVELOPMENT.md
   - Ensure all tests pass: `npm run test:ui`
   - Backend builds: `cd tdf-hq && stack build`

3. **Check existing issues/PRs:**
   - Avoid duplicate work
   - Comment on issues you want to tackle

## 📝 Development Workflow

### 1. Create a Branch

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/bug-description
# or
git checkout -b docs/documentation-update
```

**Branch naming:**
- `feature/` - New features
- `fix/` - Bug fixes
- `docs/` - Documentation only
- `refactor/` - Code refactoring
- `test/` - Adding tests
- `chore/` - Maintenance tasks

### 2. Make Your Changes

**Code style:**
- **Haskell:** Follow Servant conventions, use hlint
- **TypeScript/React:** Use existing ESLint/Prettier config
- **Naming:** camelCase for JS/TS, snake_case for SQL, PascalCase for components

**Commit messages:**
```bash
# Format: <type>: <description>

git commit -m "feat: add package expiration alerts"
git commit -m "fix: resolve CORS issue in production"
git commit -m "docs: update API documentation"
git commit -m "refactor: simplify booking validation logic"
```

**Types:**
- `feat` - New feature
- `fix` - Bug fix
- `docs` - Documentation
- `style` - Formatting
- `refactor` - Code restructuring
- `test` - Adding tests
- `chore` - Maintenance

### 3. Test Your Changes

```bash
# Backend
cd tdf-hq
stack build
stack test  # (when tests exist)

# Frontend
npm run test:ui
npm run build:ui  # Ensure it builds

# Manual testing
npm run dev:ui
# Test in browser
```

### 4. Update Documentation

- Update README if adding features
- Document API changes in OpenAPI specs
- Add code comments for complex logic
- Update DEVELOPMENT.md for workflow changes

### 5. Submit Pull Request

```bash
git push origin feature/your-feature-name
# Then create PR on GitHub
```

**PR Description Template:**

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update

## Testing
- [ ] Tested locally
- [ ] Tests pass
- [ ] Builds successfully

## Screenshots (if applicable)
[Add screenshots]

## Checklist
- [ ] Code follows style guidelines
- [ ] Self-reviewed code
- [ ] Commented complex logic
- [ ] Updated documentation
- [ ] No new warnings
- [ ] Added tests (if applicable)
```

## 🎯 Specific Contribution Areas

### Backend (Haskell)

**Adding a new endpoint:**

1. Define route in `src/TDF/API.hs`
2. Implement handler in `src/TDF/Server*.hs`
3. Add database model if needed in `src/TDF/Models*.hs`
4. Update OpenAPI spec in `docs/openapi/`
5. Add to route composition in `src/TDF/Server.hs`

**Database changes:**

1. Modify Persistent model
2. Create migration SQL in `sql/`
3. Update seed data if needed
4. Test with `RESET_DB=true`

### Frontend (React)

**Adding a feature:**

1. Create feature directory in `src/features/`
2. Add API hooks in `src/api/`
3. Create components in feature directory
4. Add route in `src/pages/` or `src/routes/`
5. Update navigation if needed

**API changes:**

1. Backend updates OpenAPI spec
2. Run `npm run generate:api:ui`
3. Update type imports
4. Test API integration

### Mobile (React Native)

**Similar to frontend, but:**

1. Use Expo Router for navigation (file-based)
2. Test on iOS and Android if possible
3. Consider offline scenarios
4. Use React Native components, not web-only

## 🐛 Reporting Bugs

**Before reporting:**
- Search existing issues
- Verify it's reproducible
- Collect error messages/logs

**Bug report should include:**
- Expected behavior
- Actual behavior
- Steps to reproduce
- Environment (OS, browser, versions)
- Error logs/screenshots

**Template:**

```markdown
## Bug Description
[Clear description]

## To Reproduce
1. Go to '...'
2. Click on '...'
3. See error

## Expected Behavior
[What should happen]

## Screenshots
[If applicable]

## Environment
- OS: [e.g. macOS 14.0]
- Browser: [e.g. Chrome 120]
- Version: [e.g. 0.2.0]
```

## 💡 Suggesting Features

**Feature request template:**

```markdown
## Feature Description
[Clear description of the feature]

## Problem It Solves
[Why is this needed?]

## Proposed Solution
[How should it work?]

## Alternatives Considered
[Other approaches]

## Additional Context
[Screenshots, mockups, etc.]
```

## 📋 Code Review Process

**What reviewers look for:**

1. **Functionality**
   - Does it work as intended?
   - Edge cases handled?
   - No regressions?

2. **Code Quality**
   - Readable and maintainable?
   - Follows conventions?
   - Properly tested?

3. **Security**
   - No hardcoded secrets?
   - Input validation?
   - SQL injection prevention?

4. **Performance**
   - Efficient queries?
   - No unnecessary re-renders?
   - Proper caching?

**Addressing feedback:**
- Be responsive and professional
- Ask questions if unclear
- Make requested changes promptly
- Push updates to same branch

## 🔐 Security

**Reporting security issues:**

⚠️ **DO NOT** open public issues for security vulnerabilities.

Instead:
1. Email: security@tdfrecords.com (or project maintainer)
2. Include detailed description
3. Steps to reproduce
4. Potential impact

**Security best practices:**
- Never commit secrets
- Use environment variables
- Validate all inputs
- Use prepared statements
- Follow OWASP guidelines

## 📚 Resources

- [Servant Documentation](https://docs.servant.dev/)
- [React Query](https://tanstack.com/query/latest)
- [MUI Components](https://mui.com/)
- [Expo Docs](https://docs.expo.dev/)
- [PostgreSQL Best Practices](https://wiki.postgresql.org/wiki/Don't_Do_This)

## ❓ Questions?

- Check [DEVELOPMENT.md](./DEVELOPMENT.md)
- Search existing issues
- Ask in discussions/chat
- Contact maintainers

## 📜 License

By contributing, you agree that your contributions will be licensed under the MIT License.

---

Happy coding! 🎵
