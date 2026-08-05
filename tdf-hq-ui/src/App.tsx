import { Suspense } from 'react';
import { Routes, useLocation } from 'react-router-dom';

import AppErrorBoundary from './routes/AppErrorBoundary';
import RouteLoadingFallback from './routes/RouteLoadingFallback';
import { renderProtectedRoutes } from './routes/protectedRoutes';
import { renderPublicRoutes } from './routes/publicRoutes';
import { lazyWithReload } from './utils/lazyWithReload';
import { shouldHideRadioForRoute } from './utils/radioRouteVisibility';

const RadioWidget = lazyWithReload(() => import('./components/RadioWidget'));

function RoutedRadioWidget() {
  const location = useLocation();
  if (shouldHideRadioForRoute(location.pathname, location.hash)) return null;
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
        <Routes>
          {renderPublicRoutes()}
          {renderProtectedRoutes()}
        </Routes>
      </Suspense>
      <RoutedRadioWidget />
    </AppErrorBoundary>
  );
}
