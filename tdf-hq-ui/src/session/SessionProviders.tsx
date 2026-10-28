import type { ReactNode } from 'react';
import { AppThemeProvider } from '../theme/AppThemeProvider';
import { ToastProvider } from '../contexts/ToastContext';
import { PlayerProvider } from '../player/PlayerProvider';
import { SessionProvider, useSession } from './SessionContext';
import { SessionQueryProvider } from './SessionQueryProvider';

/** Production composition: every query consumer lives inside its session cache. */
export function SessionProviders({ children }: { children: ReactNode }) {
  return (
    <SessionProvider>
      <SessionPlayer>{children}</SessionPlayer>
    </SessionProvider>
  );
}

function SessionPlayer({ children }: { children: ReactNode }) {
  const { session, loading } = useSession();
  return (
    <PlayerProvider identity={{ ready: !loading, partyId: session ? session.partyId : null }}>
      {/* Cache occurrences can change while the same listener keeps playing. */}
      <SessionQueryProvider>
        <AppThemeProvider>
          <ToastProvider>{children}</ToastProvider>
        </AppThemeProvider>
      </SessionQueryProvider>
    </PlayerProvider>
  );
}
