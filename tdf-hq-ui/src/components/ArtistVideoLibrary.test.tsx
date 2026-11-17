import { jest } from '@jest/globals';
import { act, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import ArtistVideoLibrary from './ArtistVideoLibrary';

it.each([undefined, 'https://youtu.be/uRNyIMLiqU8'])('shows loading feedback until videos arrive (featuredUrl=%s)', async (featuredUrl) => {
  const original = globalThis.fetch;
  let finish!: (response: Response) => void;
  globalThis.fetch = jest.fn<typeof fetch>().mockImplementation(() => new Promise(resolve => { finish = resolve; }));
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  const view = render(<QueryClientProvider client={client}><ArtistVideoLibrary artistId={520} featuredUrl={featuredUrl} /></QueryClientProvider>);
  try {
    expect(screen.getByRole('status').textContent).toBe('Cargando videos del artista…');
    if (featuredUrl) expect(screen.getByRole('button', { name: 'Ver video aquí' })).toBeTruthy();
    await act(async () => {
      finish({ ok: true, status: 200, json: async () => [{ id: 'session', url: 'https://youtu.be/uRNyIMLiqU8', title: 'Sesión completa', description: '', poster: '' }] } as Response);
    });
    expect(await screen.findByRole('heading', { name: 'Sesión completa' })).toBeTruthy();
    expect(screen.queryByRole('status')).toBeNull();
  } finally { view.unmount(); client.clear(); globalThis.fetch = original; }
});

it('marks cached videos as updating while keeping them playable', async () => {
  const original = globalThis.fetch;
  let finish!: (response: Response) => void;
  globalThis.fetch = jest.fn<typeof fetch>().mockImplementation(() => new Promise(resolve => { finish = resolve; }));
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  const videos = [{ id: 'session', url: 'https://youtu.be/uRNyIMLiqU8', title: 'Sesión guardada', description: '', poster: '' }];
  client.setQueryData(['public-artist-videos', 520], videos);
  const view = render(<QueryClientProvider client={client}><ArtistVideoLibrary artistId={520} /></QueryClientProvider>);
  try {
    expect(screen.getByRole('status').textContent).toBe('Actualizando videos del artista…');
    fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
    const player = view.container.querySelector('iframe');
    expect(player?.src).toContain('/embed/uRNyIMLiqU8');
    await act(async () => { finish({ ok: true, status: 200, json: async () => videos } as Response); });
    await waitFor(() => expect(screen.queryByRole('status')).toBeNull());
    expect(view.container.querySelector('iframe')).toBe(player);
  } finally { view.unmount(); client.clear(); globalThis.fetch = original; }
});

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
