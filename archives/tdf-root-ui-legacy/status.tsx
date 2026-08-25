import * as React from 'react';
import { createRoot } from 'react-dom/client';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ApiStatusChip } from './components/ApiStatusChip';

const mount = document.getElementById('api-status-root');

if (mount) {
  const root = createRoot(mount);
  const qc = new QueryClient();
  root.render(
    <React.StrictMode>
      <QueryClientProvider client={qc}>
        <div
          style={{
            position: 'fixed',
            bottom: 16,
            right: 16,
            zIndex: 2000,
          }}
        >
          <ApiStatusChip />
        </div>
      </QueryClientProvider>
    </React.StrictMode>,
  );
}
