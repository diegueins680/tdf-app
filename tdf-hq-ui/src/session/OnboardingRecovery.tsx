import { useEffect, useRef } from 'react';
import { useSession } from './SessionContext';

// Mounted above public and protected routes so reconnecting on a public fan
// page can reconcile an already-completed durable action with the server.
export default function OnboardingRecovery() {
  const { session, loading } = useSession();
  const recoveryRef = useRef<{ partyId: number; promise: Promise<void> } | null>(null);

  useEffect(() => {
    if (loading || !session?.partyId) return;
    const partyId = session.partyId;
    const replay = (): Promise<void> => {
      if (recoveryRef.current?.partyId === partyId) return recoveryRef.current.promise;
      const promise = (async () => {
        const [intents, progress, analytics] = await Promise.all([
          import('./onboardingIntentRecovery'),
          import('../analytics/onboardingProgress'),
          import('../analytics/posthog'),
        ]);
        await Promise.all([
          intents.retryPendingOnboardingIntent(partyId),
          progress.retryPendingFirstValueCompletion(analytics.getAnalyticsClient(), partyId),
        ]);
      })()
        .catch(() => undefined)
        .finally(() => {
          if (recoveryRef.current?.promise === promise) recoveryRef.current = null;
        });
      recoveryRef.current = { partyId, promise };
      return promise;
    };
    const handleOnline = () => { void replay(); };
    void replay();
    window.addEventListener('online', handleOnline);
    return () => window.removeEventListener('online', handleOnline);
  }, [loading, session?.partyId]);

  return null;
}
