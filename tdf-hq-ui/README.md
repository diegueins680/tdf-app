# TDF HQ UI

React web interface for managing user roles in the TDF Records platform.

## Features

- **Multi-role management** - Assign multiple roles to users via intuitive UI
- **Material-UI** - Modern, responsive design with MUI components
- **Type-safe** - Full TypeScript with generated API types
- **Real-time updates** - Immediate UI feedback on role changes
- **Color-coded roles** - Visual distinction between different role types

## Prerequisites

- Node.js 18+
- npm 9+

## Quick Start

```bash
# Install dependencies
npm install

# Copy environment file
cp .env.example .env

# Start development server
npm run dev
```

The UI will be available at `http://localhost:5173`.

## Environment Variables

Create a `.env` file:

```env
VITE_API_BASE=http://localhost:8080
VITE_TZ=America/Guayaquil
```

## Development

### Generate API Client

After updating the OpenAPI specification:

```bash
npm run generate:api
```

This generates TypeScript types from `../tdf-hq/docs/openapi/user-roles.yaml`.

### Project Structure

```
tdf-hq-ui/
├── src/
│   ├── components/
│   │   └── UserRoleManagement.tsx  # Main role management UI
│   ├── api/generated/
│   │   ├── types.ts                # Generated TypeScript types
│   │   └── client.ts               # API client
│   ├── App.tsx                     # Root component
│   └── main.tsx                    # Entry point
├── index.html
├── package.json
├── tsconfig.json
└── vite.config.ts
```

## Components

### UserRoleManagement

Main component for managing user roles.

**Features:**
- Table view of all users with their roles
- Edit button to open role assignment dialog
- Multi-select dropdown for role selection
- Color-coded role chips for visual clarity
- Real-time updates when roles change

**Usage:**
```tsx
import UserRoleManagement from './components/UserRoleManagement';

function App() {
  return <UserRoleManagement />;
}
```

### Role Colors

Roles are displayed with distinct colors:

- `Admin` - Red (error)
- `Manager` - Blue (primary)
- `Engineer` - Light blue (info)
- `Teacher` - Green (success)
- `Reception` - Purple (secondary)
- `Accounting` - Orange (warning)
- `Artist` - Blue (primary)
- `Student` - Gray (default)
- `ReadOnly` - Gray (default)

## Building for Production

```bash
# Build optimized production bundle
npm run build

# Preview production build
npm run preview
```

Build output will be in the `dist/` directory.

## API Integration

The app uses a generated TypeScript client that communicates with the backend API.

### API Client Usage

```typescript
import { apiClient } from './api/generated/client';

// Get all users
const users = await apiClient.getUsers();

// Get roles for a user
const roles = await apiClient.getUserRoles(userId);

// Update roles
await apiClient.updateUserRoles(userId, ['Teacher', 'Artist']);
```

## Testing

```bash
# Run tests (when implemented)
npm test

# Run tests in watch mode
npm test -- --watch
```

## Linting and Formatting

```bash
# Type check
npm run build

# The Vite build includes TypeScript checking
```

## Troubleshooting

### Cannot connect to API

1. Ensure backend is running on `http://localhost:8080`
2. Check `VITE_API_BASE` in `.env` matches backend URL
3. Check browser console for CORS errors

### TypeScript errors after API changes

Regenerate the API client:
```bash
npm run generate:api
```

### Build fails

Clear node_modules and reinstall:
```bash
rm -rf node_modules package-lock.json
npm install
```

## Browser Support

- Chrome/Edge (latest)
- Firefox (latest)
- Safari (latest)

## License

MIT
