import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { fireEvent, waitFor } from '@testing-library/react';
import { act, Component, type ReactNode } from 'react';
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
  getActiveSession: () => sessionFixture,
  useSession: () => ({ session: sessionFixture }),
}));
jest.unstable_mockModule('../hooks/useMetaTags', () => ({ useMetaTags: jest.fn() }));
jest.unstable_mockModule('../components/directory/OpenStreetMapResults', () => ({ default: () => <div>Mapa OSM aproximado</div> }));
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ capture: analyticsCaptureMock }) }));
jest.unstable_mockModule('../analytics/onboardingProgress', () => ({ captureFirstValueOnce: captureFirstValueOnceMock }));
jest.unstable_mockModule('../api/client', () => ({ API_BASE_URL: 'https://tdf-hq.fly.dev' }));

const { default: DirectorySearchPage } = await import('./DirectorySearchPage');

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

class ArrivalBoundary extends Component<{ children: ReactNode }, { failed: boolean }> {
  state = { failed: false };
  static getDerivedStateFromError() { return { failed: true }; }
  render() { return this.state.failed ? <p role="alert">Arrival failed</p> : this.props.children; }
}

describe('DirectorySearchPage', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    window.localStorage.clear();
    window.localStorage.setItem('tdf.directory.cityId', '22222222-2222-4222-8222-222222222222');
    sessionFixture = null;
    searchMock.mockResolvedValue(searchResponse);
    favoritesMock.mockResolvedValue([]);
  });

  it.each(['getter', 'getItem', 'setItem'] as const)('keeps public search usable when optional city storage denies %s', async (operation) => {
    const storageDescriptor = Object.getOwnPropertyDescriptor(window, 'localStorage')!;
    const denial = () => { throw new DOMException('Storage denied', 'SecurityError'); };
    const storageSpy = operation === 'getter' ? undefined : jest.spyOn(Storage.prototype, operation).mockImplementation(denial);
    if (operation === 'getter') Object.defineProperty(window, 'localStorage', { configurable: true, get: denial });
    const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => undefined);
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => {
        root.render(<ArrivalBoundary><QueryClientProvider client={queryClient}>
          <MemoryRouter initialEntries={['/buscar?q=music']}><DirectorySearchPage /></MemoryRouter>
        </QueryClientProvider></ArrivalBoundary>);
      });
      await waitFor(() => expect(container.textContent).toContain('Synthetic Bassist'));
      expect(container.querySelector('input')?.value).toBe('music');
      expect(container.textContent).not.toContain('Arrival failed');
      expect(searchMock).toHaveBeenCalledWith(expect.objectContaining({ q: 'music' }));
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
      Object.defineProperty(window, 'localStorage', storageDescriptor);
      storageSpy?.mockRestore();
      consoleSpy.mockRestore();
    }
  });

  it.each(['before-dispatch', 'before-response', 'same-party-return'] as const)('rejects obsolete favorite authority %s', async (scenario) => {
    sessionFixture = { partyId: 42 };
    searchMock.mockResolvedValue(eventSearchResponse);
    let finish!: () => void;
    addFavoriteMock.mockImplementationOnce(() => new Promise<void>((resolve) => { finish = resolve; }));
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    const render = async () => { await act(async () => {
      root.render(<QueryClientProvider client={queryClient}>
        <MemoryRouter initialEntries={['/buscar?entityType=event']}><DirectorySearchPage /></MemoryRouter>
      </QueryClientProvider>);
    }); };
    try {
      await render();
      const saveButton = await waitFor(() => {
        const button = container.querySelector<HTMLButtonElement>('[aria-label="Guardar Synthetic Event en tu cuenta"]');
        expect(button?.disabled).toBe(false);
        return button!;
      });
      if (scenario === 'before-dispatch') sessionFixture = null;
      await act(async () => { fireEvent.click(saveButton); });
      if (scenario === 'before-dispatch') {
        expect(addFavoriteMock).not.toHaveBeenCalled();
      } else {
        await waitFor(() => expect(addFavoriteMock).toHaveBeenCalledTimes(1));
        sessionFixture = { partyId: 84 };
        if (scenario === 'same-party-return') {
          await render();
          sessionFixture = { partyId: 42 };
          await render();
        }
        await act(async () => { finish(); await new Promise((resolve) => setTimeout(resolve, 20)); });
        expect(captureFirstValueOnceMock).not.toHaveBeenCalled();
        expect(analyticsCaptureMock).not.toHaveBeenCalledWith('feature_favorite_changed', expect.anything());
        expect(container.querySelector('[aria-label="Quitar Synthetic Event de tus guardados"]')).toBeNull();
      }
    } finally {
      finish?.();
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
      addFavoriteMock.mockReset().mockResolvedValue(undefined);
    }
  });

  it('keeps the new session isolated from a late favorites read for the same party', async () => {
    sessionFixture = { partyId: 42 };
    searchMock.mockResolvedValue(eventSearchResponse);
    let finish!: (value: never[]) => void;
    favoritesMock.mockImplementationOnce(() => new Promise((resolve) => { finish = resolve; }));
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    const render = async () => { await act(async () => {
      root.render(<QueryClientProvider client={queryClient}><MemoryRouter><DirectorySearchPage /></MemoryRouter></QueryClientProvider>);
    }); };
    try {
      await render();
      await waitFor(() => expect(favoritesMock).toHaveBeenCalledTimes(1));
      sessionFixture = { partyId: 84 }; await render();
      sessionFixture = { partyId: 42 }; await render();
      await act(async () => { finish([{ targetKind: 'event', targetId: '42', createdAt: '', result: null }] as never[]); });
      await waitFor(() => expect(container.querySelector<HTMLButtonElement>('[aria-label="Guardar Synthetic Event en tu cuenta"]')?.disabled).toBe(false));
      expect(container.querySelector('[aria-label="Quitar Synthetic Event de tus guardados"]')).toBeNull();
    } finally {
      await act(async () => root.unmount()); container.remove(); queryClient.clear();
    }
  });

  it.each([false, true])('offers an authoritative refresh after an ambiguous favorite failure (refresh fails first: %s)', async (failRefreshFirst) => {
    sessionFixture = { partyId: 42 };
    searchMock.mockResolvedValue(eventSearchResponse);
    addFavoriteMock.mockRejectedValueOnce(new Error('Connection interrupted after dispatch'));
    favoritesMock.mockResolvedValueOnce([]).mockResolvedValue([{ targetKind: 'event', targetId: '42', createdAt: '', result: null }]);
    if (failRefreshFirst) favoritesMock.mockRejectedValueOnce(new Error('Synthetic refresh unavailable'));
    const container = document.createElement('div'); document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => { root.render(<QueryClientProvider client={queryClient}><MemoryRouter><DirectorySearchPage /></MemoryRouter></QueryClientProvider>); });
      const save = await waitFor(() => {
        const button = container.querySelector<HTMLButtonElement>('[aria-label="Guardar Synthetic Event en tu cuenta"]');
        expect(button?.disabled).toBe(false); return button!;
      });
      fireEvent.click(save);
      const refresh = await waitFor(() => {
        expect(container.textContent).toContain('No pudimos confirmar el cambio');
        return Array.from(container.querySelectorAll('button')).find(button => button.textContent === 'Consultar guardados')!;
      });
      expect(container.textContent).not.toContain('Tu cuenta no cambió');
      fireEvent.click(refresh);
      if (failRefreshFirst) {
        await waitFor(() => expect(container.textContent).toContain('No pudimos consultar tus guardados'));
        expect(container.textContent).toContain('No pudimos confirmar el cambio');
        fireEvent.click(refresh);
      }
      await waitFor(() => expect(container.querySelector('[aria-label="Quitar Synthetic Event de tus guardados"]')?.getAttribute('aria-pressed')).toBe('true'));
      await waitFor(() => expect(container.textContent).not.toContain('No pudimos confirmar el cambio'));
      expect(addFavoriteMock).toHaveBeenCalledTimes(1);
      expect(captureFirstValueOnceMock).not.toHaveBeenCalled();
    } finally {
      await act(async () => root.unmount()); container.remove(); queryClient.clear();
    }
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
          undefined,
          expect.any(Function),
        );
      });
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
    }
  }, 15_000);
});
