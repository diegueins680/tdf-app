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

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <QueryClientProvider client={qc}>
      <AppThemeProvider>
        <BrowserRouter
          future={{ v7_startTransition: true, v7_relativeSplatPath: true }}
        >
          <SessionProvider>
            <App />
          </SessionProvider>
        </BrowserRouter>
      </AppThemeProvider>
    </QueryClientProvider>
  </React.StrictMode>
);
