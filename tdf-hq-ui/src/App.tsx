import NavigationScrollRestoration from './routes/NavigationScrollRestoration';
import { Suspense } from 'react';
import { useLocation } from 'react-router-dom';

import AppErrorBoundary from './routes/AppErrorBoundary';
import RouteLoadingFallback from './routes/RouteLoadingFallback';
import RoutedRadioWidget from './components/RoutedRadioWidget';
import OnboardingRecovery from './session/OnboardingRecovery';
import { lazyWithReload } from './utils/lazyWithReload';

const AppRoutes = lazyWithReload(() => import('./routes/AppRoutes'));
const GlobalPlayer = lazyWithReload(() => import('./player/GlobalPlayer'));

export default function App() {
  const location = useLocation();
  return (
    <AppErrorBoundary resetKey={location.pathname}>
      {/* The session-owned player outlives route and query-cache transitions. */}
      <NavigationScrollRestoration />
      <OnboardingRecovery />
      <Suspense fallback={<RouteLoadingFallback />}>
        <AppRoutes />
      </Suspense>
      <Suspense fallback={null}>
        <GlobalPlayer />
      </Suspense>
      <RoutedRadioWidget />
    </AppErrorBoundary>
  );
}
