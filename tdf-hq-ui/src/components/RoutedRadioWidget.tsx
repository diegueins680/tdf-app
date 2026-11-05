import { Suspense, useEffect, useState } from 'react';
import { useLocation } from 'react-router-dom';

import { useSession } from '../session/SessionContext';
import { lazyWithReload } from '../utils/lazyWithReload';
import { shouldRenderRadioWidget } from '../utils/radioRouteVisibility';

const RadioWidget = lazyWithReload(() => import('./RadioWidget'));

export default function RoutedRadioWidget() {
  const location = useLocation();
  const { session, loading } = useSession();
  const [activated, setActivated] = useState(false);
  const canOpen = shouldRenderRadioWidget(location.pathname, location.hash, Boolean(session), loading);
  useEffect(() => {
    if (!session || loading) setActivated(false);
    else if (canOpen) setActivated(true);
  }, [canOpen, session, loading]);

  // Route policy controls initial discovery only. Removing an already mounted
  // widget also removes its audio element and interrupts the listener's stream.
  if (!session || loading || (!canOpen && !activated)) return null;
  return (
    <Suspense fallback={null}>
      <RadioWidget />
    </Suspense>
  );
}
