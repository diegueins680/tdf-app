import { lazy, Suspense } from 'react';
const Content = lazy(() => import('./MobilePromoContent'));
export default function MobilePromo(props: { surface: string; banner?: boolean; compact?: boolean }) {
  return <Suspense fallback={null}><Content {...props} /></Suspense>;
}
