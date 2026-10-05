import type { ReactNode } from 'react';
import { AppThemeProvider } from '../theme/AppThemeProvider';
import { ToastProvider } from '../contexts/ToastContext';
import { SessionProvider } from './SessionContext';
import { SessionQueryProvider } from './SessionQueryProvider';

/** Production composition: every query consumer lives inside its session cache. */
export function SessionProviders({ children }: { children: ReactNode }) {
  return (
    <SessionProvider>
      <SessionQueryProvider>
        <AppThemeProvider>
          <ToastProvider>{children}</ToastProvider>
        </AppThemeProvider>
      </SessionQueryProvider>
    </SessionProvider>
  );
}
