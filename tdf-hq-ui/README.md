# TDF HQ UI - Frontend

React + TypeScript web UI for TDF Records Management Platform.

## Tech Stack

- **React 18** - UI library
- **TypeScript** - Type safety
- **Vite** - Build tool
- **Material-UI (MUI)** - Component library
- **React Query** - Data fetching and state management
- **Axios** - HTTP client

## Project Structure

```
tdf-hq-ui/
├── src/
│   ├── components/
│   │   └── UserRoleManagement.tsx  # User role management component
│   ├── api/
│   │   ├── client.ts               # API client & types
│   │   └── generated/              # Auto-generated API clients
│   ├── App.tsx                     # Main app component
│   └── main.tsx                    # Entry point
├── public/                         # Static assets
├── index.html                      # HTML template
├── vite.config.ts                  # Vite configuration
└── package.json                    # Dependencies
```

## Development

### Prerequisites

- Node.js 18+
- npm or yarn

### Setup

1. Install dependencies:
```bash
npm install
```

2. Set up environment variables:
```bash
cp .env.example .env
```

Edit `.env` and set:
```
VITE_API_BASE=http://localhost:8080
```

3. Start development server:
```bash
npm run dev
```

The UI will be available at http://localhost:3000

### Generate API Client

After updating the backend OpenAPI spec:

```bash
npm run generate:api
```

This generates TypeScript types from `../../tdf-hq/openapi.json`.

## Features

### User Role Management

- View all users with their current roles
- Update user roles via dropdown selection
- Real-time feedback on role changes
- Color-coded role chips for easy identification
- Active/Inactive status display

## Building

```bash
npm run build
```

Build output will be in the `dist/` directory.

## Testing

```bash
npm run test
```
