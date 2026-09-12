import { Suspense } from 'react';
import { useLocation } from 'react-router-dom';

import AppErrorBoundary from './routes/AppErrorBoundary';
import RouteLoadingFallback from './routes/RouteLoadingFallback';
import { useSession } from './session/SessionContext';
import { lazyWithReload } from './utils/lazyWithReload';
import { shouldRenderRadioWidget } from './utils/radioRouteVisibility';

const AppRoutes = lazyWithReload(() => import('./routes/AppRoutes'));
const RadioWidget = lazyWithReload(() => import('./components/RadioWidget'));

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
  return (
    <AppErrorBoundary>
      <Suspense fallback={<RouteLoadingFallback />}>
        <AppRoutes />
      </Suspense>
      <RoutedRadioWidget />
    </AppErrorBoundary>
  );
}
