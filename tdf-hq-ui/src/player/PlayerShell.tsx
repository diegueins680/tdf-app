import { Suspense } from 'react';
import { Routes } from 'react-router-dom';

import RouteLoadingFallback from '../routes/RouteLoadingFallback';
import { renderProtectedRoutes } from '../routes/protectedRoutes';
import { renderPublicRoutes } from '../routes/publicRoutes';
import { lazyWithReload } from '../utils/lazyWithReload';
import { PlayerProvider } from './PlayerProvider';

const GlobalPlayer = lazyWithReload(() => import('./GlobalPlayer'));
const RoutedRadioWidget = lazyWithReload(() => import('../components/RoutedRadioWidget'));

export default function PlayerShell() {
  return (
    <PlayerProvider>
      <Suspense fallback={<RouteLoadingFallback />}>
        <Routes>
          {renderPublicRoutes()}
          {renderProtectedRoutes()}
        </Routes>
      </Suspense>
      <Suspense fallback={null}>
        <GlobalPlayer />
      </Suspense>
      <Suspense fallback={null}>
        <RoutedRadioWidget />
      </Suspense>
    </PlayerProvider>
  );
}
