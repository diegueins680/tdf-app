import React from 'react';
import ReactDOM from 'react-dom/client';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { BrowserRouter } from 'react-router-dom';
import App from './App';
import './i18n';
import { SessionProvider } from './session/SessionContext';
import { AppThemeProvider } from './theme/AppThemeProvider';
import { reportMissingEnv } from './utils/env';

const qc = new QueryClient();

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
