import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { fireEvent, waitFor } from '@testing-library/react';
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';

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

const eventSearchResponse = {
  items: [{
    id: '42', type: 'event', slug: '42', title: 'Synthetic Event', summary: 'Fixture público de prueba.',
    imageUrl: null, location: { city: 'Quito', countryCode: 'EC', precision: 'city' },
    sponsored: false, score: 0.9,
  }],
  sponsoredItems: [], facets: { entityTypes: { event: 1 }, cities: [], total: 1 },
};

let sessionFixture: { partyId: number } | null = null;
const searchMock = jest.fn(async () => searchResponse);
const favoritesMock = jest.fn(async () => []);
const addFavoriteMock = jest.fn(async () => undefined);
const removeFavoriteMock = jest.fn(async () => undefined);
const captureFirstValueOnceMock = jest.fn(async () => true);
const analyticsCaptureMock = jest.fn();

jest.unstable_mockModule('../api/directory', () => ({
  Directory: {
    search: searchMock,
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
  useSession: () => ({ session: sessionFixture }),
}));
jest.unstable_mockModule('../hooks/useMetaTags', () => ({ useMetaTags: jest.fn() }));
jest.unstable_mockModule('../components/directory/OpenStreetMapResults', () => ({ default: () => <div>Mapa OSM aproximado</div> }));
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ capture: analyticsCaptureMock }) }));
jest.unstable_mockModule('../analytics/onboardingProgress', () => ({ captureFirstValueOnce: captureFirstValueOnceMock }));
jest.unstable_mockModule('../api/client', () => ({ API_BASE_URL: 'https://tdf-hq.fly.dev' }));

const { default: DirectorySearchPage } = await import('./DirectorySearchPage');

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

describe('DirectorySearchPage', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    window.localStorage.clear();
    window.localStorage.setItem('tdf.directory.cityId', '22222222-2222-4222-8222-222222222222');
    sessionFixture = null;
    searchMock.mockResolvedValue(searchResponse);
    favoritesMock.mockResolvedValue([]);
  });

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

  it('hydrates account favorites and removes an event with an accessible desired-state control', async () => {
    sessionFixture = { partyId: 42 };
    searchMock.mockResolvedValue(eventSearchResponse);
    favoritesMock
      .mockResolvedValueOnce([{
        targetKind: 'event',
        targetId: '42',
        createdAt: '2026-09-07T10:00:00Z',
        result: null,
      }])
      .mockResolvedValue([]);
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => {
        root.render(
          <QueryClientProvider client={queryClient}>
            <MemoryRouter initialEntries={['/buscar?entityType=event']}><DirectorySearchPage /></MemoryRouter>
          </QueryClientProvider>,
        );
      });
      const removeButton = await waitFor(() => {
        const button = container.querySelector<HTMLButtonElement>('[aria-label="Quitar Synthetic Event de tus guardados"]');
        expect(button?.getAttribute('aria-pressed')).toBe('true');
        return button!;
      });
      fireEvent.click(removeButton);
      await waitFor(() => {
        expect(removeFavoriteMock).toHaveBeenCalledWith('event', '42');
      });
      expect(captureFirstValueOnceMock).not.toHaveBeenCalled();
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
    }
  }, 15_000);

  it('records event first value only after a server-acknowledged save', async () => {
    sessionFixture = { partyId: 42 };
    searchMock.mockResolvedValue(eventSearchResponse);
    favoritesMock
      .mockResolvedValueOnce([])
      .mockResolvedValue([{
        targetKind: 'event',
        targetId: '42',
        createdAt: '2026-09-07T10:00:00Z',
        result: null,
      }]);
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => {
        root.render(
          <QueryClientProvider client={queryClient}>
            <MemoryRouter initialEntries={['/buscar?entityType=event']}><DirectorySearchPage /></MemoryRouter>
          </QueryClientProvider>,
        );
      });
      const saveButton = await waitFor(() => {
        const button = container.querySelector<HTMLButtonElement>('[aria-label="Guardar Synthetic Event en tu cuenta"]');
        expect(button?.disabled).toBe(false);
        return button!;
      });
      fireEvent.click(saveButton);
      await waitFor(() => {
        expect(addFavoriteMock).toHaveBeenCalledWith('event', '42');
        expect(captureFirstValueOnceMock).toHaveBeenCalledWith(
          expect.objectContaining({ capture: analyticsCaptureMock }),
          42,
          'event_saved',
        );
      });
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
    }
  }, 15_000);
});
