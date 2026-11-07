import { jest } from '@jest/globals';
import { fireEvent, render, screen, within } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import ArtistVideoLibrary from './ArtistVideoLibrary';

it('keeps the featured session credits without adding a second copy of its player', async () => {
  const original = globalThis.fetch;
  const url = 'https://www.youtube.com/watch?v=uRNyIMLiqU8';
  globalThis.fetch = jest.fn<typeof fetch>().mockResolvedValue({ ok: true, status: 200, json: async () => [{ id: 'session', url, title: 'Sesión completa', description: 'Edición: Maya Ponce', poster: '' }] } as Response);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  const view = render(<QueryClientProvider client={client}><ArtistVideoLibrary artistId={520} featuredUrl="https://youtu.be/uRNyIMLiqU8" /></QueryClientProvider>);
  try {
    expect(await screen.findByText('Edición: Maya Ponce')).toBeTruthy();
    expect(screen.getByText('Sesión completa')).toBeTruthy();
    expect(screen.getAllByRole('button', { name: 'Ver video aquí' })).toHaveLength(1);
  } finally { view.unmount(); client.clear(); globalThis.fetch = original; }
});

it('keeps a shared session selected after the artist changes the featured video', async () => {
  const original = globalThis.fetch;
  const sessionUrl = 'https://www.youtube.com/watch?v=uRNyIMLiqU8';
  globalThis.fetch = jest.fn<typeof fetch>().mockResolvedValue({ ok: true, status: 200, json: async () => [{ id: 'a'.repeat(32), url: sessionUrl, title: 'Sesión compartida', description: 'Créditos confirmados', poster: '' }] } as Response);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  const view = render(<QueryClientProvider client={client}><ArtistVideoLibrary artistId={520} featuredUrl="https://youtu.be/pUtIlM8Bic4" requestedVideo="youtube:uRNyIMLiqU8" /></QueryClientProvider>);
  try {
    const region = await screen.findByRole('region', { name: 'Sesión compartida' });
    fireEvent.click(within(region).getByRole('button', { name: 'Ver video aquí' }));
    expect(region.querySelector('iframe')?.src).toContain('/embed/uRNyIMLiqU8');
    expect(region.querySelector('iframe')?.src).not.toContain('pUtIlM8Bic4');
    expect(screen.getByText('Créditos confirmados')).toBeTruthy();
  } finally { view.unmount(); client.clear(); globalThis.fetch = original; }
});

it('does not replace an unavailable shared session with a different featured video', async () => {
  const original = globalThis.fetch;
  globalThis.fetch = jest.fn<typeof fetch>().mockResolvedValue({ ok: true, status: 200, json: async () => [] } as Response);
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  const view = render(<QueryClientProvider client={client}><ArtistVideoLibrary artistId={520} featuredUrl="https://youtu.be/pUtIlM8Bic4" requestedVideo="youtube:uRNyIMLiqU8" /></QueryClientProvider>);
  try {
    expect(await screen.findByText(/No pudimos encontrar esta sesión/)).toBeTruthy();
    expect(screen.queryByRole('button', { name: 'Ver video aquí' })).toBeNull();
  } finally { view.unmount(); client.clear(); globalThis.fetch = original; }
});
