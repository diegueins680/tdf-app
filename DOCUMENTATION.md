# TDF Records Platform - Documentation Index

Welcome to the comprehensive documentation for the TDF Records Platform. This index will help you find the information you need quickly.

## 📚 Getting Started

### For New Developers
1. **[README.md](README.md)** - Start here! Project overview and quick setup
2. **[QUICKSTART.md](QUICKSTART.md)** - Minimal steps to get running locally
3. **[QUICK_START_REFERENCE.md](QUICK_START_REFERENCE.md)** - Fast command reference (cheat sheet)
4. **[DEVELOPMENT.md](DEVELOPMENT.md)** - Detailed development workflows and patterns
5. **[CONTRIBUTING.md](CONTRIBUTING.md)** - How to contribute to the project

### For Deployment & Operations
1. **[DEPLOYMENT_GUIDE.md](DEPLOYMENT_GUIDE.md)** - Complete deployment procedures for all platforms
2. **[README.md](README.md#-deployments)** - Quick deployment reference
3. **[BEST_PRACTICES.md](BEST_PRACTICES.md)** - Quality gates and engineering standards
4. **[SECURITY_NOTICE.md](SECURITY_NOTICE.md)** - Security considerations and guidelines
5. **[TROUBLESHOOTING.md](TROUBLESHOOTING.md)** - Common issues and solutions

## 🏗️ Architecture & Design

### System Architecture
- **[README.md](README.md#-architecture)** - High-level architecture overview
- **[ARCHITECTURE_DIAGRAM.md](ARCHITECTURE_DIAGRAM.md)** - Detailed system diagrams and data flows
- **[FEATURES.md](FEATURES.md)** - Complete feature catalog and capabilities

### Code Organization
- **[tdf-hq/README.md](tdf-hq/README.md)** - Backend (Haskell) architecture and structure
- **[tdf-hq-ui/README.md](tdf-hq-ui/README.md)** - Frontend (React) architecture and structure
- **[tdf-mobile/](tdf-mobile/)** - Mobile app (Expo/React Native) - Git submodule

## 💻 Development Guides

### Backend Development (Haskell)
- **[tdf-hq/README.md](tdf-hq/README.md)** - Backend setup, API structure, and conventions
- **[DEVELOPMENT.md](DEVELOPMENT.md#backend-haskell)** - Backend development patterns
- **[BEST_PRACTICES.md](BEST_PRACTICES.md#haskell-backend-tdf-hq)** - Haskell coding standards

### Frontend Development (React)
- **[tdf-hq-ui/README.md](tdf-hq-ui/README.md)** - React UI setup and structure
- **[DEVELOPMENT.md](DEVELOPMENT.md#web-ui-react)** - Frontend development patterns
- **[UI_VISUAL_GUIDE.md](UI_VISUAL_GUIDE.md)** - UI components, mockups, and design specs
- **[BEST_PRACTICES.md](BEST_PRACTICES.md#typescript--react-tdf-hq-ui)** - TypeScript/React standards

### Mobile Development (React Native)
- **[README.md](README.md#mobile-app---tdf-mobile)** - Mobile app overview
- **Note:** Mobile app is a Git submodule - run `git submodule update --init --recursive`

### API Development
- **[DEVELOPMENT.md](DEVELOPMENT.md#making-api-changes)** - API development workflow
- **[QUICK_REFERENCE.md](QUICK_REFERENCE.md)** - User roles endpoints and examples
- **[tdf-hq/docs/openapi/](tdf-hq/docs/openapi/)** - OpenAPI specifications

## 🧪 Testing & Quality

### Testing Guides
- **[TESTING_GUIDE.md](TESTING_GUIDE.md)** - Comprehensive testing procedures (26 test cases)
- **[BEST_PRACTICES.md](BEST_PRACTICES.md#suggested-workflow)** - Quality gates workflow
- **[DEVELOPMENT.md](DEVELOPMENT.md#testing)** - Running tests

### Quality Assurance
- Run `npm run quality` - Consolidated quality gate (lint + typecheck + backend tests)
- Run `npm run lint:ui` - ESLint checks for frontend
- Run `npm run typecheck:ui` - TypeScript type checking
- Run `stack test` (from tdf-hq/) - Backend test suite

## 🔌 API Documentation

### API Reference
- **[QUICK_REFERENCE.md](QUICK_REFERENCE.md)** - User roles API reference with cURL examples
- **[tdf-hq/docs/openapi/api.yaml](tdf-hq/docs/openapi/api.yaml)** - Core API spec (drives TypeScript client generation)
- **[tdf-hq/docs/openapi/lessons-and-receipts.yaml](tdf-hq/docs/openapi/lessons-and-receipts.yaml)** - Lessons, packages, and receipts API
- **[tdf-hq/docs/openapi/user-roles.yaml](tdf-hq/docs/openapi/user-roles.yaml)** - Multi-role user management API

### API Usage Examples
```bash
# Generate API clients after backend changes
npm run generate:api:ui      # Web UI
npm run generate:api:mobile  # Mobile app

# List all users
curl http://localhost:8080/api/users

# Update user roles
curl -X PUT http://localhost:8080/api/users/1/roles \
  -H "Content-Type: application/json" \
  -d '{"roles": ["Teacher", "Artist"]}'
```

## 🎯 Feature Documentation

### Business Features
- **[README.md](README.md#-business-features)** - Overview of business capabilities
- **[FEATURES.md](FEATURES.md)** - Detailed feature descriptions
- **[specs.yaml](specs.yaml)** - Business requirements specification

### Feature Implementations
- **[MULTI_ROLE_IMPLEMENTATION.md](MULTI_ROLE_IMPLEMENTATION.md)** - Multi-role user management system
- **[MULTI_ROLE_CHANGES.md](MULTI_ROLE_CHANGES.md)** - Changes summary for multi-role feature
- **[IMPLEMENTATION_SUMMARY.md](IMPLEMENTATION_SUMMARY.md)** - Implementation details
- **[IMPROVEMENTS_COMPLETED.md](IMPROVEMENTS_COMPLETED.md)** - Completed improvements log

#### CMS (página pública Records)
- Slugs separados por tipo para carga dinámica desde BD (no un solo entry):
  - `records-release-*`: cada release (payload con `title`, `artist`, `releasedOn`, `description/blurb`, `cover`, `links[]`).
  - `records-session-*`: cada video de sesión (`youtubeId`, `title`, `duration?`, `guests?`, `description?`).
  - `records-recording-*`: cada tarjeta de “Grabaciones recientes” (`title`, `image`, `description`, `artist?`, `recordedAt?`, `vibe?`).
- El frontend usa el endpoint público `GET /cms/contents?locale=es&slugPrefix=records-...` para poblar hero, releases, sesiones y grabaciones.

## 🚀 Deployment & Operations

### Deployment Guides
- **[README.md](README.md#-deployments)** - Deployment configurations
  - Cloudflare Pages (Web UI)
  - Vercel (Web UI alternative)
  - Koyeb (Backend API)

### Environment Configuration
- **[README.md](README.md#-environment-variables)** - Environment variable reference
- **[DEVELOPMENT.md](DEVELOPMENT.md#configure-environment)** - Local development setup

### Docker & Containers
- **[tdf-hq/README.md](tdf-hq/README.md#docker-compose)** - Docker Compose setup
- Run `cd tdf-hq && make up` - Start PostgreSQL + API containers
- Run `make logs` - View container logs
- Run `make health` - Check service health

## 🔐 Security

### Security Guidelines
- **[SECURITY_NOTICE.md](SECURITY_NOTICE.md)** - Security considerations
- **[CONTRIBUTING.md](CONTRIBUTING.md#-security)** - Reporting security issues
- **[BEST_PRACTICES.md](BEST_PRACTICES.md)** - Security best practices

### Security Checklist
- ✅ Never commit `.env` files or secrets
- ✅ Use environment variables for sensitive data
- ✅ Validate all user inputs
- ✅ Use prepared statements for database queries
- ✅ Restrict CORS origins in production
- ✅ Review `archives/` directory for historical secrets

## 🤝 Contributing

### Contribution Workflow
1. **[CONTRIBUTING.md](CONTRIBUTING.md)** - Complete contribution guide
2. **[DEVELOPMENT.md](DEVELOPMENT.md#git-workflow)** - Git workflow and branching
3. **[BEST_PRACTICES.md](BEST_PRACTICES.md)** - Code quality standards

### Code Review
- Follow commit message conventions: `feat:`, `fix:`, `docs:`, `refactor:`, etc.
- Ensure all tests pass: `npm run quality`
- Update documentation when adding features
- Request reviews from maintainers

## 🤖 AI Agent Configuration

### Agent Guidelines
- **[.github/copilot-instructions.md](.github/copilot-instructions.md)** - AI agent guidelines for development
- **[.github/agents/my-agent.md](.github/agents/my-agent.md)** - Musician Developer agent configuration
- **[.github/agents/artist-social-sync-agent.md](.github/agents/artist-social-sync-agent.md)** - Agent for syncing artist social posts into TDF

### Agent Capabilities
The Musician Developer agent specializes in:
- Artist profile and portfolio features
- Fan engagement tools
- Music catalog management
- Performance and booking systems
- Integration with streaming platforms

## 📖 Reference Documentation

### Quick References
- **[QUICK_START_REFERENCE.md](QUICK_START_REFERENCE.md)** - Command cheat sheet and common workflows
- **[QUICK_REFERENCE.md](QUICK_REFERENCE.md)** - API quick reference
- **[CLEANUP_SUMMARY.md](CLEANUP_SUMMARY.md)** - Recent cleanup activities

### Project Management
- **[specs.yaml](specs.yaml)** - Business requirements
- **[FEATURES.md](FEATURES.md)** - Feature specifications
- **[IMPROVEMENTS_COMPLETED.md](IMPROVEMENTS_COMPLETED.md)** - Change history

## 🗺️ Documentation Map by Role

### I'm a Backend Developer
1. [tdf-hq/README.md](tdf-hq/README.md) - Backend architecture
2. [DEVELOPMENT.md](DEVELOPMENT.md#backend-haskell) - Backend patterns
3. [BEST_PRACTICES.md](BEST_PRACTICES.md#haskell-backend-tdf-hq) - Standards
4. [QUICK_REFERENCE.md](QUICK_REFERENCE.md) - API reference

### I'm a Frontend Developer
1. [tdf-hq-ui/README.md](tdf-hq-ui/README.md) - Frontend architecture
2. [DEVELOPMENT.md](DEVELOPMENT.md#web-ui-react) - Frontend patterns
3. [UI_VISUAL_GUIDE.md](UI_VISUAL_GUIDE.md) - UI components
4. [BEST_PRACTICES.md](BEST_PRACTICES.md#typescript--react-tdf-hq-ui) - Standards

### I'm a DevOps Engineer
1. [README.md](README.md#-deployments) - Deployment guide
2. [tdf-hq/README.md](tdf-hq/README.md#docker-compose) - Docker setup
3. [BEST_PRACTICES.md](BEST_PRACTICES.md) - Quality gates
4. [SECURITY_NOTICE.md](SECURITY_NOTICE.md) - Security considerations

### I'm a Product Manager
1. [README.md](README.md) - Project overview
2. [FEATURES.md](FEATURES.md) - Feature catalog
3. [specs.yaml](specs.yaml) - Requirements
4. [IMPROVEMENTS_COMPLETED.md](IMPROVEMENTS_COMPLETED.md) - Progress tracking

### I'm a New Contributor
1. [README.md](README.md) - Start here
2. [QUICKSTART.md](QUICKSTART.md) - Quick setup
3. [CONTRIBUTING.md](CONTRIBUTING.md) - How to contribute
4. [DEVELOPMENT.md](DEVELOPMENT.md) - Development guide

## 📊 Documentation Statistics

| Category | Files | Key Documents |
|----------|-------|---------------|
| Getting Started | 4 | README, QUICKSTART, DEVELOPMENT, CONTRIBUTING |
| Architecture | 3 | ARCHITECTURE_DIAGRAM, FEATURES, specs.yaml |
| Implementation | 5 | MULTI_ROLE_IMPLEMENTATION, IMPLEMENTATION_SUMMARY, etc. |
| Testing | 1 | TESTING_GUIDE |
| API Reference | 3 | QUICK_REFERENCE, OpenAPI specs, API docs |
| Best Practices | 2 | BEST_PRACTICES, SECURITY_NOTICE |
| Visual Guides | 1 | UI_VISUAL_GUIDE |
| **Total** | **~20** | Core documentation files |

## 🔄 Keeping Documentation Updated

### When to Update Documentation

**Always update documentation when you:**
- Add new features or endpoints
- Change existing APIs or behaviors
- Modify deployment procedures
- Update dependencies or requirements
- Fix bugs that affect usage
- Add new development workflows

### Documentation Review Checklist
- [ ] README.md reflects current features
- [ ] API documentation matches implementation
- [ ] Environment variables are documented
- [ ] Deployment guides are accurate
- [ ] Code examples work correctly
- [ ] Links are not broken
- [ ] Screenshots are up to date

## 🆘 Getting Help

### Troubleshooting Resources
- **[TROUBLESHOOTING.md](TROUBLESHOOTING.md)** - Comprehensive troubleshooting guide (NEW!)
- **[DEVELOPMENT.md](DEVELOPMENT.md#troubleshooting)** - Development-specific issues
- **[tdf-hq/README.md](tdf-hq/README.md#troubleshooting)** - Backend troubleshooting
- **[DEPLOYMENT_GUIDE.md](DEPLOYMENT_GUIDE.md#troubleshooting-deployment)** - Deployment issues
- **[QUICKSTART.md](QUICKSTART.md#troubleshooting)** - Setup troubleshooting

### Common Issues
1. **Backend won't start**: See [Backend Issues](TROUBLESHOOTING.md#backend-issues)
2. **CORS errors**: See [API & CORS Issues](TROUBLESHOOTING.md#api--cors-issues)
3. **Frontend won't connect**: See [Frontend Issues](TROUBLESHOOTING.md#frontend-issues)
4. **Mobile submodule empty**: See [Mobile App Issues](TROUBLESHOOTING.md#mobile-app-issues)
5. **Database connection fails**: See [Database Issues](TROUBLESHOOTING.md#database-issues)
2. **CORS errors**: Verify `ALLOW_ORIGINS` includes your frontend URL
3. **API generation fails**: Validate OpenAPI spec and check paths
4. **Module not found**: Run `stack clean && stack build` or `npm install`
5. **Submodule issues**: Run `git submodule update --init --recursive`

### Support Channels
- Review existing documentation
- Check GitHub Issues
- Contact maintainers
- Review code comments and examples

## 🎯 Next Steps

After reviewing this documentation:

1. **New to the project?** Start with [README.md](README.md) and [QUICKSTART.md](QUICKSTART.md)
2. **Setting up development?** Follow [DEVELOPMENT.md](DEVELOPMENT.md)
3. **Making changes?** Review [CONTRIBUTING.md](CONTRIBUTING.md) and [BEST_PRACTICES.md](BEST_PRACTICES.md)
4. **Deploying?** Check deployment section in [README.md](README.md#-deployments)
5. **Need API info?** See [QUICK_REFERENCE.md](QUICK_REFERENCE.md) and OpenAPI specs

---

**Last Updated:** 2024-11-14  
**Maintained By:** TDF Records Development Team  
**License:** MIT

For questions or suggestions about documentation, please open an issue or submit a PR.
