import type { ReactNode } from 'react';
import { useEffect, useState } from 'react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ApiError } from '../api/client';
import { useSession } from './SessionContext';
import { getAuthSessionEpoch } from './authEvents';

function QueryScope({ children }: { children: ReactNode }) {
  const [client] = useState(() => new QueryClient({
    defaultOptions: {
      queries: {
        staleTime: 30_000,
        refetchOnWindowFocus: true,
        retry: (failureCount, error) => {
          if (error instanceof ApiError && error.status >= 400 && error.status < 500) return false;
          return failureCount < 3;
        },
        retryDelay: (attempt) => Math.min(1000 * 2 ** attempt, 30_000),
      },
    },
  }));
  useEffect(() => () => client.clear(), [client]);
  return <QueryClientProvider client={client}>{children}</QueryClientProvider>;
}

/** Each security-scope occurrence gets new caches and component state. */
export function SessionQueryProvider({ children }: { children: ReactNode }) {
  const { session } = useSession();
  const scope = JSON.stringify([
    getAuthSessionEpoch(),
    session?.partyId ?? null, session?.username ?? null, session?.apiToken ?? null,
    [...(session?.roles ?? [])].sort(), [...(session?.modules ?? [])].sort(),
    [...(session?.featureFlags ?? [])].sort(),
  ]);
  // Adjust during render, before children can commit under another principal.
  // Use an occurrence number as the React key: never retain per-user clients or
  // put credentials into keys, and A -> B -> A must allocate a fresh cache.
  const [boundary, setBoundary] = useState({ scope, epoch: 0 });
  if (boundary.scope !== scope) {
    setBoundary({ scope, epoch: boundary.epoch + 1 });
    return null;
  }
  return <QueryScope key={boundary.epoch}>{children}</QueryScope>;
}
