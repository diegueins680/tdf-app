import { Navigate, useLocation } from 'react-router-dom';

import PublicBranding from '../components/PublicBranding';
import { useSession } from '../session/SessionContext';
import { buildLoginRedirectPath } from '../utils/loginRouting';
import { lazyWithReload as lazy } from '../utils/lazyWithReload';
import RouteLoadingFallback from './RouteLoadingFallback';

const Shell = lazy(() => import('./AppShell').then((module) => ({ default: module.Shell })));
const DirectoryPublicDetailPage = lazy(() => import('../pages/DirectoryPublicDetailPage'));

/** Public event metadata precedes sign-in; source content/actions keep their existing authority. */
export default function EventCommunityEntry() {
  const { session, loading } = useSession();
  const location = useLocation();
  const search = new URLSearchParams(location.search);
  const login = buildLoginRedirectPath(`${location.pathname}${location.search}${location.hash}`);
  if (loading) return <RouteLoadingFallback />;
  if (session) return <Shell />;
  // An explicit action retains its exact return target, including attribution.
  if (search.get('compose') === '1' || search.has('moment') || search.has('tarea')) {
    return <Navigate to={login} replace />;
  }
  return <PublicBranding><DirectoryPublicDetailPage kind="event" authenticationFallback={login} /></PublicBranding>;
}
