import React from 'react';
import { createRoot } from 'react-dom/client';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import Workspace from '../../tdf-hq-ui/src/features/social/SocialWorkspace';
createRoot(document.getElementById('root')!).render(
  <QueryClientProvider client={new QueryClient()}>
    <MemoryRouter><Workspace /></MemoryRouter>
  </QueryClientProvider>,
);
