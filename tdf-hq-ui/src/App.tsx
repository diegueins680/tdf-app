import { Suspense } from 'react';
import { Routes } from 'react-router-dom';

import RadioWidget from './components/RadioWidget';
import RouteLoadingFallback from './routes/RouteLoadingFallback';
import { renderProtectedRoutes } from './routes/protectedRoutes';
import { renderPublicRoutes } from './routes/publicRoutes';

export default function App() {
  return (
    <>
      <Suspense fallback={<RouteLoadingFallback />}>
        <Routes>
          {renderPublicRoutes()}
          {renderProtectedRoutes()}
        </Routes>
      </Suspense>
      <RadioWidget />
    </>
  );
}
