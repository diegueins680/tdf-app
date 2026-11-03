import NavigationScrollRestoration from './routes/NavigationScrollRestoration';
import { Suspense } from 'react';
import { useLocation } from 'react-router-dom';

import AppErrorBoundary from './routes/AppErrorBoundary';
import RouteLoadingFallback from './routes/RouteLoadingFallback';
import { useSession } from './session/SessionContext';
import OnboardingRecovery from './session/OnboardingRecovery';
import { lazyWithReload } from './utils/lazyWithReload';
import { shouldRenderRadioWidget } from './utils/radioRouteVisibility';

const AppRoutes = lazyWithReload(() => import('./routes/AppRoutes'));
const RadioWidget = lazyWithReload(() => import('./components/RadioWidget'));
const GlobalPlayer = lazyWithReload(() => import('./player/GlobalPlayer'));

function RoutedRadioWidget() {
  const location = useLocation();
  const { session, loading } = useSession();
  if (!shouldRenderRadioWidget(location.pathname, location.hash, Boolean(session), loading)) return null;
  return (
    <Suspense fallback={null}>
      <RadioWidget />
    </Suspense>
  );
}

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
