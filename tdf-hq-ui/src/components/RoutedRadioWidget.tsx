import { Suspense } from 'react';
import { useLocation } from 'react-router-dom';

import { useSession } from '../session/SessionContext';
import { lazyWithReload } from '../utils/lazyWithReload';
import { shouldRenderRadioWidget } from '../utils/radioRouteVisibility';

const RadioWidget = lazyWithReload(() => import('./RadioWidget'));

export default function RoutedRadioWidget() {
  const location = useLocation();
  const { session, loading } = useSession();
  if (!shouldRenderRadioWidget(location.pathname, location.hash, Boolean(session), loading)) return null;
  return (
    <Suspense fallback={null}>
      <RadioWidget />
    </Suspense>
  );
}
