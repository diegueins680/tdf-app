import React from 'react';
import ReactDOM from 'react-dom/client';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ApiError } from './api/client';
import { BrowserRouter } from 'react-router-dom';
import App from './App';
import './i18n';
import { SessionProvider } from './session/SessionContext';
import { AppThemeProvider } from './theme/AppThemeProvider';
import { reportMissingEnv } from './utils/env';
import { getAnalyticsClient } from './analytics/posthog';
import { startWebVitalsTracking } from './analytics/webVitals';
import { LocalePreferencesProvider } from './contexts/LocalePreferencesContext';
import { CurrencyProvider } from './contexts/CurrencyContext';
import { ToastProvider } from './contexts/ToastContext';

const qc = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: 1000 * 60 * 2,
      refetchOnWindowFocus: false,
      retry: (failureCount, error) => {
        if (error instanceof ApiError && error.status >= 400 && error.status < 500) {
          return false;
        }
        return failureCount < 3;
      },
    },
  },
});

reportMissingEnv(['VITE_PAYPAL_CLIENT_ID']);

// Initialize analytics as early as possible so pageviews captured by
// posthog-js include the landing route. If VITE_POSTHOG_KEY is unset
// this is a no-op.
const analytics = getAnalyticsClient();
startWebVitalsTracking(analytics);

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <QueryClientProvider client={qc}>
      <AppThemeProvider>
        <ToastProvider>
          <BrowserRouter>
            <SessionProvider>
              <LocalePreferencesProvider>
                <CurrencyProvider>
                  <App />
                </CurrencyProvider>
              </LocalePreferencesProvider>
            </SessionProvider>
          </BrowserRouter>
        </ToastProvider>
      </AppThemeProvider>
    </QueryClientProvider>
  </React.StrictMode>
);
