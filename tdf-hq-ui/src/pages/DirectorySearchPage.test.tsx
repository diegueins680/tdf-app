import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { waitFor } from '@testing-library/react';
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';

import type { DirectoryFavorite } from '../api/directory';
import type { SessionUser } from '../session/SessionContext';
import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

const searchResponse = {
  items: [{
    id: '11111111-1111-4111-8111-111111111111', type: 'profile', slug: 'synthetic-bassist',
    title: 'Synthetic Bassist', summary: 'Fixture público de prueba.',
    imageUrl: '/assets/serve/directory/profiles/synthetic-bassist.webp',
    location: { city: 'Quito', countryCode: 'EC', precision: 'city' },
    sponsored: false, score: 0.8,
  }, {
    id: '44444444-4444-4444-8444-444444444444', type: 'profile', slug: 'profile-without-photo',
    title: 'Profile Without Photo', summary: 'Fixture sin una foto cargada.',
    imageUrl: null,
    location: { city: 'Quito', countryCode: 'EC', precision: 'city' },
    sponsored: false, score: 0.7,
  }],
  sponsoredItems: [], facets: { entityTypes: { profile: 2 }, cities: [], total: 2 },
};

const favoritesMock = jest.fn<() => Promise<DirectoryFavorite[]>>();
const addFavoriteMock = jest.fn<(targetKind: string, targetId: string) => Promise<void>>();
const removeFavoriteMock = jest.fn<(targetKind: string, targetId: string) => Promise<void>>();
let currentSession: SessionUser | null = null;

jest.unstable_mockModule('../api/directory', () => ({
  Directory: {
    search: jest.fn(async () => searchResponse),
    suggestions: jest.fn(async () => []),
    taxonomies: jest.fn(async () => ({
      locale: 'es', professions: [], classifiedCategories: [], compensationTypes: [], serviceOfferings: [], currencies: [], instruments: [], genres: [],
      cities: [{ id: '22222222-2222-4222-8222-222222222222', code: 'quito-ec-p', name: 'Quito', countryId: '33333333-3333-4333-8333-333333333333' }],
    })),
    favorites: favoritesMock,
    addFavorite: addFavoriteMock,
    removeFavorite: removeFavoriteMock,
  },
}));
jest.unstable_mockModule('../session/SessionContext', () => ({
  getStoredSessionToken: () => null,
  useSession: () => ({ session: currentSession }),
}));
jest.unstable_mockModule('../hooks/useMetaTags', () => ({ useMetaTags: jest.fn() }));
jest.unstable_mockModule('../components/directory/OpenStreetMapResults', () => ({ default: () => <div>Mapa OSM aproximado</div> }));
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ capture: jest.fn() }) }));
jest.unstable_mockModule('../api/client', () => ({ API_BASE_URL: 'https://tdf-hq.fly.dev' }));

const { default: DirectorySearchPage, DirectoryResultCard } = await import('./DirectorySearchPage');

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

describe('DirectorySearchPage', () => {
  beforeEach(() => {
    (globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
    window.localStorage.clear();
    currentSession = null;
    favoritesMock.mockReset().mockResolvedValue([]);
    addFavoriteMock.mockReset().mockResolvedValue(undefined);
    removeFavoriteMock.mockReset().mockResolvedValue(undefined);
  }, 15_000);

  it('renders the public Quito-first search as the dominant accessible experience', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => {
        root.render(
          <QueryClientProvider client={queryClient}>
            <MemoryRouter initialEntries={['/buscar']}><DirectorySearchPage /></MemoryRouter>
          </QueryClientProvider>,
        );
        await new Promise((resolve) => setTimeout(resolve, 0));
      });
      await act(async () => { await new Promise((resolve) => setTimeout(resolve, 100)); });
      expect(container.querySelector('h1')?.textContent).toContain('Encuentra a la gente');
      expect(container.textContent).toContain('Encuentra a la gente y las oportunidades que hacen música');
      expect(container.textContent).toContain('Quito');
      expect(container.textContent).toContain('Servicio');
      expect(container.textContent).toContain('Resultados orgánicos');
      await waitFor(() => {
        expect(container.textContent).toContain('Ingresar para guardar');
        const profileImage = container.querySelector<HTMLImageElement>('img[alt="Foto de Synthetic Bassist"]');
        expect(profileImage?.src).toBe('https://tdf-hq.fly.dev/assets/serve/directory/profiles/synthetic-bassist.webp');
        const fallbackImage = container.querySelector<HTMLImageElement>('img[alt="Imagen de referencia de Profile Without Photo"]');
        expect(fallbackImage?.src).toBe('http://localhost/artist-fallback.svg');
      });
      await expectNoSeriousAccessibilityViolations(container);
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
    }
  }, 15_000);

  it('returns an anonymous user to the filtered search before asking them to save explicitly', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => {
        root.render(
          <QueryClientProvider client={queryClient}>
            <MemoryRouter initialEntries={['/buscar?q=bajista']}><DirectorySearchPage /></MemoryRouter>
          </QueryClientProvider>,
        );
      });

      const loginLink = await waitFor(() => {
        const link = container.querySelector<HTMLAnchorElement>('a[href^="/login?"]');
        expect(link?.textContent).toContain('Ingresar para guardar');
        return link!;
      });
      const loginUrl = new URL(loginLink.href, window.location.origin);
      expect(loginUrl.pathname).toBe('/login');
      expect(loginUrl.searchParams.get('redirect')).toContain('/buscar?');
      expect(loginUrl.searchParams.get('redirect')).toContain('q=bajista');
      expect(favoritesMock).not.toHaveBeenCalled();
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
    }
  }, 15_000);

  it('removes a current Party favorite through the desired-state API', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    queryClient.setQueryData<DirectoryFavorite[]>(['directory', 'favorites', 42], [{
      targetKind: 'profile',
      targetId: searchResponse.items[0].id,
      createdAt: '2026-09-08T18:00:00Z',
    }]);
    try {
      await act(async () => {
        root.render(
          <QueryClientProvider client={queryClient}>
            <MemoryRouter initialEntries={['/buscar']}>
              <DirectoryResultCard
                item={searchResponse.items[0]}
                sessionActive
                partyId={42}
                isFavorite
                favoriteStateReady
                loginReturnPath="/buscar"
                layout="list"
              />
            </MemoryRouter>
          </QueryClientProvider>,
        );
      });

      await waitFor(() => {
        const button = container.querySelector<HTMLButtonElement>('[aria-label="Quitar de guardados Synthetic Bassist"]');
        expect(button).not.toBeNull();
        expect(button?.getAttribute('aria-pressed')).toBe('true');
        expect(button?.disabled).toBe(false);
      });
      await act(async () => {
        container.querySelector<HTMLButtonElement>('[aria-label="Quitar de guardados Synthetic Bassist"]')?.click();
        await Promise.resolve();
      });
      await waitFor(
        () => expect(removeFavoriteMock).toHaveBeenCalledWith('profile', searchResponse.items[0].id),
        { timeout: 5_000 },
      );
      await waitFor(() => expect(queryClient.getQueryData(['directory', 'favorites', 42])).toEqual([]));
      expect(addFavoriteMock).not.toHaveBeenCalled();
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
    }
  }, 15_000);

  it('does not expose the previous Party favorite while the next account loads', async () => {
    currentSession = { username: 'first', displayName: 'First', roles: [], partyId: 42 };
    favoritesMock
      .mockResolvedValueOnce([{
        targetKind: 'profile',
        targetId: searchResponse.items[0].id,
        createdAt: '2026-09-08T18:00:00Z',
      }])
      .mockImplementationOnce(() => new Promise<DirectoryFavorite[]>(() => undefined));
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    const renderPage = () => root.render(
      <QueryClientProvider client={queryClient}>
        <MemoryRouter initialEntries={['/buscar']}><DirectorySearchPage /></MemoryRouter>
      </QueryClientProvider>,
    );
    try {
      await act(async () => renderPage());
      await waitFor(() => expect(container.querySelector('[aria-label="Quitar de guardados Synthetic Bassist"]')).not.toBeNull());

      currentSession = { username: 'second', displayName: 'Second', roles: [], partyId: 84 };
      await act(async () => renderPage());

      await waitFor(() => expect(container.textContent).toContain('Consultando guardados…'));
      expect(container.querySelector('[aria-label="Quitar de guardados Synthetic Bassist"]')).toBeNull();
      expect(favoritesMock).toHaveBeenCalledTimes(2);
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
    }
  }, 15_000);

  it('keeps a failed save visible and retryable', async () => {
    addFavoriteMock.mockRejectedValueOnce(new Error('offline'));
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
    try {
      await act(async () => {
        root.render(
          <QueryClientProvider client={queryClient}>
            <MemoryRouter initialEntries={['/buscar']}>
              <DirectoryResultCard
                item={searchResponse.items[0]}
                sessionActive
                partyId={42}
                isFavorite={false}
                favoriteStateReady
                loginReturnPath="/buscar"
                layout="list"
              />
            </MemoryRouter>
          </QueryClientProvider>,
        );
      });

      await waitFor(() => {
        const button = container.querySelector<HTMLButtonElement>('[aria-label="Guardar Synthetic Bassist"]');
        expect(button?.disabled).toBe(false);
      });
      await act(async () => {
        container.querySelector<HTMLButtonElement>('[aria-label="Guardar Synthetic Bassist"]')?.dispatchEvent(
          new MouseEvent('click', { bubbles: true, cancelable: true }),
        );
        await Promise.resolve();
      });
      await waitFor(() => expect(addFavoriteMock).toHaveBeenCalledWith('profile', searchResponse.items[0].id), { timeout: 5_000 });
      await waitFor(
        () => expect(container.textContent).toContain('No se pudo actualizar este guardado. Intenta de nuevo.'),
        { timeout: 5_000 },
      );
      expect(container.querySelector<HTMLButtonElement>('[aria-label="Guardar Synthetic Bassist"]')?.disabled).toBe(false);
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
    }
  }, 15_000);
});
