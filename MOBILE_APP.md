# TDF Mobile App

> **Note:** This is a Git submodule. You need to initialize it before development.

## About

The TDF Mobile App is a React Native application built with Expo that provides mobile access to the TDF Records platform. It allows students, teachers, and artists to manage their schedules, view packages, and interact with the platform on the go.

## Tech Stack

- **Framework:** React Native with Expo
- **Language:** TypeScript
- **State Management:** React Query
- **Navigation:** Expo Router (file-based routing)
- **API Client:** Auto-generated from OpenAPI specs
- **UI Components:** React Native Elements / Custom components

## Initial Setup

### 1. Initialize the Submodule

The mobile app is tracked as a Git submodule. Before you can work with it, you need to initialize it:

```bash
# From repository root
git submodule update --init --recursive

# Or if you haven't cloned yet, clone with submodules
git clone --recursive <repository-url>
```

### 2. Verify Submodule Status

```bash
# Check submodule status
git submodule status

# Should show the current commit hash and path
# Example: +abc123def tdf-mobile (heads/main)
```

### 3. Navigate to Mobile Directory

```bash
cd tdf-mobile
```

If the directory is empty or doesn't contain package.json, the submodule needs to be initialized (see step 1).

## Development Setup

### Prerequisites

- Node.js 18+ and npm 9+
- Expo CLI: `npm install -g expo-cli` or `npx expo`
- iOS Simulator (Mac only) or Android Emulator
- Expo Go app on your physical device (optional)

### Environment Configuration

Create a `.env` file in the `tdf-mobile/` directory:

```bash
# API Configuration
EXPO_PUBLIC_API_BASE=http://localhost:8080

# Timezone (optional)
EXPO_PUBLIC_TZ=America/Guayaquil
```

**Important:** Use `EXPO_PUBLIC_` prefix for environment variables in Expo apps. This makes them available to the client-side code.

### Install Dependencies

```bash
cd tdf-mobile
npm install
```

### Generate API Client

The mobile app uses auto-generated TypeScript API clients from the backend's OpenAPI specifications:

```bash
# From repository root
npm run generate:api:mobile

# Or from mobile directory
cd tdf-mobile
npm run generate:api
```

This will:
1. Fetch the latest OpenAPI spec from `tdf-hq/docs/openapi/`
2. Generate TypeScript types and client code
3. Place generated files in `src/api/generated/`

**Never edit generated files manually!** They will be overwritten on next generation.

## Running the App

### Start Development Server

```bash
cd tdf-mobile
npm start
```

This will:
- Start the Expo development server
- Open Expo DevTools in your browser
- Show QR code for testing on physical devices

### Run on Specific Platform

```bash
# iOS Simulator (Mac only)
npm run ios

# Android Emulator
npm run android

# Web (for quick testing)
npm run web
```

### Testing on Physical Device

1. Install **Expo Go** app from App Store (iOS) or Play Store (Android)
2. Scan the QR code shown in the terminal/browser
3. The app will load on your device

**Note:** Your device must be on the same network as your development machine.

## Project Structure

```
tdf-mobile/
├── app/                    # Expo Router pages (file-based routing)
│   ├── (tabs)/            # Tab navigation screens
│   ├── _layout.tsx        # Root layout
│   └── index.tsx          # Landing/home screen
├── src/
│   ├── api/               # API client and hooks
│   │   ├── generated/     # Auto-generated API client (DO NOT EDIT)
│   │   └── hooks.ts       # Custom React Query hooks
│   ├── components/        # Reusable UI components
│   │   ├── common/        # Generic components
│   │   └── features/      # Feature-specific components
│   ├── lib/               # Utilities
│   │   ├── api.ts         # API client configuration
│   │   ├── queryClient.ts # React Query setup
│   │   └── time.ts        # Date/time utilities
│   ├── screens/           # Screen components (if not using Expo Router)
│   └── types/             # TypeScript type definitions
├── assets/                # Images, fonts, icons
├── openapi/               # Local OpenAPI specs (optional)
├── .env                   # Environment variables (not committed)
├── .env.example           # Environment variables template
├── app.json               # Expo configuration
├── package.json           # Dependencies and scripts
└── tsconfig.json          # TypeScript configuration
```

## Key Features

### For Students
- View lesson schedule and upcoming bookings
- Check package balance and hours remaining
- Book studio time and practice rooms
- View payment history and invoices
- Receive notifications for class reminders

### For Teachers
- Manage class schedule
- Mark attendance
- View student roster and progress
- Access teaching materials
- Schedule makeup classes

### For Artists
- View recording session schedule
- Track project progress through pipelines
- Access media assets and files
- Communicate with engineers and producers

## Development Workflows

### Adding a New Feature

1. **Create API endpoints in backend** (if needed)
   ```bash
   cd tdf-hq
   # Update TDF.API, TDF.Server, etc.
   stack build
   ```

2. **Regenerate API client**
   ```bash
   npm run generate:api:mobile
   ```

3. **Create screen/component in mobile app**
   ```bash
   cd tdf-mobile
   # For Expo Router: create file in app/
   # Example: app/(tabs)/bookings.tsx
   ```

4. **Use generated API hooks**
   ```typescript
   import { useBookingsQuery } from '../api/hooks';
   
   export default function BookingsScreen() {
     const { data, isLoading } = useBookingsQuery();
     // ...
   }
   ```

### Making API Changes

1. Update backend API and OpenAPI spec
2. Regenerate mobile client: `npm run generate:api:mobile`
3. Update mobile code to use new types/endpoints
4. Test thoroughly on both iOS and Android

### Styling and Theming

The mobile app uses a consistent theme based on TDF Records brand:

```typescript
// Example theme usage
import { useTheme } from '../lib/theme';

export default function MyComponent() {
  const theme = useTheme();
  
  return (
    <View style={{ backgroundColor: theme.colors.background }}>
      <Text style={{ color: theme.colors.primary }}>
        Hello World
      </Text>
    </View>
  );
}
```

## Testing

### Manual Testing

Currently, testing is done manually with Expo Go:

1. Start dev server: `npm start`
2. Open on physical device or simulator
3. Test user flows and interactions
4. Verify API integration
5. Check error handling

### Testing Checklist

- [ ] Loads correctly on iOS
- [ ] Loads correctly on Android
- [ ] API calls work with backend
- [ ] Authentication flow works
- [ ] Navigation between screens works
- [ ] Forms validate correctly
- [ ] Error messages are clear
- [ ] Loading states are shown
- [ ] **Offline mode works correctly**:
  - [ ] Offline banner appears when disconnected
  - [ ] Schedule and packages load from cache
  - [ ] Bookings can be created offline
  - [ ] Offline mutations sync on reconnect
  - [ ] Failed syncs show appropriate errors

### Offline Support Testing

See [tdf-mobile/README.md](tdf-mobile/README.md#testing-offline-mode) for detailed instructions on:
- Simulating offline mode in Expo Go, iOS Simulator, and Android Emulator
- Manual test checklist for offline features
- Expected behavior for queued mutations and sync

### Automated Testing

Current test coverage (Phase 1):
- **Jest** for unit tests of offline logic
- Tests for storage layer (cache and queue operations)
- Tests for sync engine (retry logic, backoff, error handling)
- Tests for network detection

Run tests:
```bash
cd tdf-mobile
npm test
```

Planned additions:
- React Native Testing Library for component tests
- Detox for E2E tests

## Common Issues and Troubleshooting

### Issue: Submodule is empty or outdated

**Solution:**
```bash
# From repository root
git submodule update --init --recursive
cd tdf-mobile
git checkout main  # or appropriate branch
git pull
```

### Issue: API connection refused

**Symptoms:** Network errors, "Connection refused"

**Solutions:**
1. Verify backend is running: `curl http://localhost:8080/health`
2. Check `EXPO_PUBLIC_API_BASE` in `.env`
3. For physical devices: Use your computer's IP instead of `localhost`
   ```bash
   # Find your IP
   # Mac/Linux: ifconfig | grep inet
   # Windows: ipconfig
   
   # Update .env
   EXPO_PUBLIC_API_BASE=http://192.168.1.100:8080
   ```

### Issue: "Cannot find module" errors

**Solution:**
```bash
# Clear cache and reinstall
rm -rf node_modules
npm install

# Clear Expo cache
npx expo start --clear
```

### Issue: API types are outdated

**Solution:**
```bash
# Regenerate API client
npm run generate:api:mobile

# Restart dev server
npm start
```

### Issue: Changes not reflecting

**Solution:**
```bash
# Clear cache and restart
npx expo start --clear

# Or reload in app: shake device and select "Reload"
```

## Building for Production

### iOS

```bash
# Build for App Store
npx eas build --platform ios

# Build for TestFlight
npx eas build --platform ios --profile preview
```

### Android

```bash
# Build for Play Store
npx eas build --platform android

# Build APK for testing
npx eas build --platform android --profile preview
```

**Note:** You'll need an Expo account and EAS CLI configured for production builds.

## Environment Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `EXPO_PUBLIC_API_BASE` | Backend API URL | `http://localhost:8080` or `https://api.tdfrecords.com` |
| `EXPO_PUBLIC_TZ` | Timezone for date display | `America/Guayaquil` |

## API Integration

The mobile app communicates with the backend via REST API:

- **Base URL:** Configured via `EXPO_PUBLIC_API_BASE`
- **Authentication:** Bearer token in `Authorization` header
- **Content Type:** `application/json`
- **Client:** Auto-generated from OpenAPI specs

### Example API Usage

```typescript
import { api } from './lib/api';
import { useQuery } from '@tanstack/react-query';

// Using generated hooks
function MyComponent() {
  const { data: bookings } = useQuery({
    queryKey: ['bookings'],
    queryFn: () => api.GET('/bookings')
  });
  
  return <BookingList bookings={bookings} />;
}
```

## Contributing

See the main [CONTRIBUTING.md](../CONTRIBUTING.md) for general contribution guidelines.

### Mobile-Specific Guidelines

1. **Platform Considerations**
   - Test on both iOS and Android
   - Handle platform-specific behaviors
   - Use Platform.select() when needed

2. **Code Style**
   - Follow existing TypeScript patterns
   - Use functional components with hooks
   - Keep components small and focused

3. **Navigation**
   - Use Expo Router for navigation
   - Keep navigation structure flat when possible
   - Use type-safe navigation

4. **Performance**
   - Optimize FlatList rendering
   - Use React.memo() for expensive components
   - Lazy load screens when appropriate

## Resources

- [Expo Documentation](https://docs.expo.dev/)
- [React Native Documentation](https://reactnative.dev/)
- [Expo Router](https://docs.expo.dev/router/introduction/)
- [React Query](https://tanstack.com/query/latest)
- [TypeScript](https://www.typescriptlang.org/)

## Support

For issues or questions:
1. Check this documentation
2. Review main project [README](../README.md)
3. Check [DEVELOPMENT.md](../DEVELOPMENT.md)
4. Open an issue on GitHub

---

**Status:** Active Development  
**Platform:** iOS, Android  
**Maintained By:** TDF Records Development Team
