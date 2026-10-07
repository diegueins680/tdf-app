import React from 'react';
import ReactDOM from 'react-dom/client';
import { BrowserRouter } from 'react-router-dom';
import App from './App';
import './i18n';
import { SessionProviders } from './session/SessionProviders';
import { reportMissingEnv } from './utils/env';
import { LocalePreferencesProvider } from './contexts/LocalePreferencesContext';
import { CurrencyProvider } from './contexts/CurrencyContext';
import './styles/print.css';
import { BootErrorBoundary, installGlobalErrorHandlers } from './bootSafety';

reportMissingEnv(['VITE_PAYPAL_CLIENT_ID']);
installGlobalErrorHandlers();

// Initialize analytics as early as possible so pageviews captured by
// posthog-js include the landing route. If VITE_POSTHOG_KEY is unset
// this is a no-op.
void import('./analytics/posthog')
  .then(({ getAnalyticsClient }) => {
    const analytics = getAnalyticsClient();
    if (!analytics.ready) return;
    void import('./analytics/webVitals')
      .then(({ startWebVitalsTracking }) => startWebVitalsTracking(analytics))
      .catch(() => undefined);
  })
  .catch(() => undefined);

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <BootErrorBoundary>
      <BrowserRouter>
        <SessionProviders>
          <LocalePreferencesProvider>
            <CurrencyProvider>
              <App />
            </CurrencyProvider>
          </LocalePreferencesProvider>
        </SessionProviders>
      </BrowserRouter>
    </BootErrorBoundary>
  </React.StrictMode>
);
