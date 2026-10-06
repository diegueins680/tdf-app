import { Suspense } from 'react';

import AppErrorBoundary from './routes/AppErrorBoundary';
import { lazyWithReload } from './utils/lazyWithReload';

const PlayerShell = lazyWithReload(() => import('./player/PlayerShell'));

export default function App() {
  return (
    <AppErrorBoundary>
      <Suspense fallback={null}>
        <PlayerShell />
      </Suspense>
    </AppErrorBoundary>
  );
}
