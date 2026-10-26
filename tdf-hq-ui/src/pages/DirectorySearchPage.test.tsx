import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';
import esDirectory from '../i18n/locales/directorySearch.es';
import enDirectory from '../i18n/locales/directorySearch.en';
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
jest.unstable_mockModule('../api/client', () => ({ API_BASE_URL: 'https://api.tdfrecords.net' }));

await i18n.use(initReactI18next).init({ lng: 'es', fallbackLng: 'es', interpolation: { escapeValue: false }, resources: { es: { translation: { directorySearch: esDirectory } }, en: { translation: { directorySearch: enDirectory } } } });

const { default: DirectorySearchPage } = await import('./DirectorySearchPage');

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

class ArrivalBoundary extends Component<{ children: ReactNode }, { failed: boolean }> {
  state = { failed: false };
  static getDerivedStateFromError() { return { failed: true }; }
  render() { return this.state.failed ? <p role="alert">Arrival failed</p> : this.props.children; }
}

describe('DirectorySearchPage', () => {
  beforeEach(async () => {
    await i18n.changeLanguage('es');
    jest.clearAllMocks();
    window.localStorage.clear();
    window.localStorage.setItem('tdf.directory.cityId', '22222222-2222-4222-8222-222222222222');
    sessionFixture = null;
    searchMock.mockResolvedValue(searchResponse);
    favoritesMock.mockResolvedValue([]);
  });

  it.each(['/inicio?q=bass', '/inicio/?q=bass', '/Inicio?q=bass', '/buscar?q=bass'])('welcomes visitors on the start page while preserving direct search at %s', async (entry) => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => {
        root.render(<QueryClientProvider client={queryClient}><MemoryRouter initialEntries={[entry]}><DirectorySearchPage /></MemoryRouter></QueryClientProvider>);
      });
      await waitFor(() => expect(container.textContent).toContain('Synthetic Bassist'));
      const welcome = container.querySelector('#public-welcome-title');
      expect(Boolean(welcome)).toBe(entry.toLowerCase().startsWith('/inicio'));
      expect(container.querySelector('input')?.value).toBe('bass');
      expect(searchMock).toHaveBeenCalledWith(expect.objectContaining({ q: 'bass' }), expect.any(AbortSignal));
      expect(container.textContent).toContain('Estos filtros describen lo que buscas, no quién eres.');
      expect(container.querySelector('h1')?.textContent).toBe(entry.toLowerCase().startsWith('/inicio') ? 'Bienvenido a TDF Records' : esDirectory.heading);
      expect(addFavoriteMock).not.toHaveBeenCalled();
      if (welcome) {
        const link = container.querySelector<HTMLAnchorElement>('a[href="/records"]')!;
        expect(link).not.toBeNull();
        await act(async () => { fireEvent.click(link); });
        expect(analyticsCaptureMock).toHaveBeenCalledWith('public_discovery_cta_clicked', { route: '/inicio', destination: '/records' });
      }
    } finally {
      await act(async () => root.unmount()); container.remove(); queryClient.clear();
    }
  });

  it('keeps public exploration available when the directory fails', async () => {
    searchMock.mockRejectedValue(new Error('Synthetic directory outage'));
    const container = document.createElement('div'); document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => { root.render(<QueryClientProvider client={queryClient}><MemoryRouter initialEntries={['/inicio']}><DirectorySearchPage /></MemoryRouter></QueryClientProvider>); });
      await waitFor(() => expect(container.textContent).toContain(esDirectory.searchError));
      expect(container.querySelector('a[href="/tdf"]')).not.toBeNull();
      expect(container.querySelector('a[href="/records"]')).not.toBeNull();
      expect(container.querySelector('#public-welcome-title')?.textContent).toBe('Bienvenido a TDF Records');
      expect(container.querySelector('form[role="search"]')?.getAttribute('aria-label')).toBe('Buscar artistas y profesionales');
      expect(addFavoriteMock).not.toHaveBeenCalled();
    } finally {
      await act(async () => root.unmount()); container.remove(); queryClient.clear();
    }
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
      expect(searchMock).toHaveBeenCalledWith(expect.objectContaining({ q: 'music' }), expect.any(AbortSignal));
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
      Object.defineProperty(window, 'localStorage', storageDescriptor);
      storageSpy?.mockRestore();
      consoleSpy.mockRestore();
    }
  });

  it('links derived artist listings to their canonical profile and shows the real profile image', async () => {
    searchMock.mockResolvedValue({
      items: [{
        id: '55555555-5555-4555-8555-555555555555', type: 'classified', slug: 'synthetic-band-perfil',
        title: 'Synthetic Band', summary: 'Banda derivada de su perfil público.',
        imageUrl: 'https://cdn.example.test/synthetic-band.jpg',
        location: { city: 'Quito', countryCode: 'EC', precision: 'city' },
        sponsored: false, score: 0.8,
        sourceProfile: {
          id: '66666666-6666-4666-8666-666666666666', slug: 'synthetic-band', name: 'Synthetic Band',
          canonicalUrl: '/directorio/synthetic-band',
        },
      }],
      sponsoredItems: [], facets: { entityTypes: { classified: 1 }, cities: [], total: 1 },
    });
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => {
        root.render(<QueryClientProvider client={queryClient}>
          <MemoryRouter initialEntries={['/buscar?entityType=classified']}><DirectorySearchPage /></MemoryRouter>
        </QueryClientProvider>);
      });
      const link = await waitFor(() => {
        const anchor = Array.from(container.querySelectorAll('a')).find((element) => element.textContent === 'Ver perfil');
        expect(anchor).toBeDefined();
        return anchor!;
      });
      expect(link.getAttribute('href')).toBe('/directorio/synthetic-band');
      const image = container.querySelector<HTMLImageElement>('img[alt="Foto de Synthetic Band"]');
      expect(image?.getAttribute('src')).toBe('https://cdn.example.test/synthetic-band.jpg');
      expect(image?.getAttribute('loading')).toBe('lazy');
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
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

  it.each(['native', 'cancel', 'denied', 'clipboard', 'clipboard-denied', 'unavailable'] as const)('settles public sharing with accurate feedback: %s', async (scenario) => {
    const shareDescriptor = Object.getOwnPropertyDescriptor(navigator, 'share');
    const clipboardDescriptor = Object.getOwnPropertyDescriptor(navigator, 'clipboard');
    const nativeShare = jest.fn(async () => {
      if (scenario === 'cancel') throw new DOMException('Cancelled', 'AbortError');
      if (scenario === 'denied') throw new DOMException('Denied', 'NotAllowedError');
    });
    const writeText = jest.fn(async () => {
      if (scenario === 'clipboard-denied') throw new DOMException('Denied', 'NotAllowedError');
    });
    Object.defineProperty(navigator, 'share', { configurable: true, value: ['native', 'cancel', 'denied'].includes(scenario) ? nativeShare : undefined });
    Object.defineProperty(navigator, 'clipboard', { configurable: true, value: scenario === 'unavailable' ? undefined : { writeText } });
    const container = document.createElement('div'); document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => { root.render(<QueryClientProvider client={queryClient}><MemoryRouter><DirectorySearchPage /></MemoryRouter></QueryClientProvider>); });
      const share = await waitFor(() => {
        const button = Array.from(container.querySelectorAll('button')).find(button => button.textContent === 'Compartir');
        expect(button).toBeDefined(); return button!;
      });
      await act(async () => { fireEvent.click(share); });
      const expected = scenario === 'native' ? 'Acción de compartir completada.' : scenario === 'cancel' ? 'El contenido no se compartió.' : scenario === 'clipboard' ? 'Enlace copiado.' : 'No se pudo compartir el enlace.';
      await waitFor(() => expect(container.textContent).toContain(expected));
      if (scenario === 'clipboard') expect(writeText).toHaveBeenCalledWith('http://localhost/directorio/synthetic-bassist');
      if (scenario === 'native') expect(nativeShare).toHaveBeenCalledWith(expect.objectContaining({ title: 'Synthetic Bassist', url: 'http://localhost/directorio/synthetic-bassist' }));
      if (['cancel', 'denied', 'clipboard-denied', 'unavailable'].includes(scenario)) {
        expect(container.textContent).not.toContain('Enlace copiado.');
        expect(container.textContent).not.toContain('Acción de compartir completada.');
      }
      expect(share.disabled).toBe(false);
    } finally {
      await act(async () => root.unmount()); container.remove(); queryClient.clear();
      if (shareDescriptor) Object.defineProperty(navigator, 'share', shareDescriptor); else Reflect.deleteProperty(navigator, 'share');
      if (clipboardDescriptor) Object.defineProperty(navigator, 'clipboard', clipboardDescriptor); else Reflect.deleteProperty(navigator, 'clipboard');
    }
  });

  it('uses English copy and accessible names while preserving query and public identities across language changes', async () => {
    await i18n.changeLanguage('en');
    const container = document.createElement('div'); document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => { root.render(<QueryClientProvider client={queryClient}>
        <MemoryRouter initialEntries={['/buscar?q=music']}><DirectorySearchPage /></MemoryRouter>
      </QueryClientProvider>); });
      await waitFor(() => expect(container.textContent).toContain('Find the people and opportunities that make music'));
      expect(container.querySelector('input')?.value).toBe('music');
      expect(container.textContent).toContain('Service');
      expect(container.textContent).toContain('Organic results');
      await waitFor(() => expect(container.querySelector('img[alt="Photo of Synthetic Bassist"]')).not.toBeNull());
      expect(container.textContent).toContain('Fixture público de prueba.');
      expect(container.textContent).toContain('Quito');
      await act(async () => { await i18n.changeLanguage('es'); });
      expect(container.textContent).toContain('Encuentra a la gente y las oportunidades que hacen música');
      expect(container.querySelector('input')?.value).toBe('music');
      expect(container.querySelector('img[alt="Foto de Synthetic Bassist"]')).not.toBeNull();
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
        expect(profileImage?.src).toBe('https://api.tdfrecords.net/assets/serve/directory/profiles/synthetic-bassist.webp');
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

  it('hydrates Eventos and Quito from a deep link with every tab counted and the event date shown', async () => {
    const quito = '22222222-2222-4222-8222-222222222222';
    const startsAt = '2026-10-24T19:00:00Z';
    searchMock.mockResolvedValue({
      items: [{
        ...eventSearchResponse.items[0],
        subtitle: 'Andes Brewing — La Pradera',
        effectiveAt: startsAt,
        location: { city: 'Quito', cityId: quito, countryCode: 'EC', precision: 'city' },
      }],
      sponsoredItems: [],
      facets: { entityTypes: { event: 1, profile: 3, classified: 2, all: 6 }, cities: [], total: 1 },
    });
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => {
        root.render(<QueryClientProvider client={queryClient}>
          <MemoryRouter initialEntries={[`/buscar?entityType=event&cityId=${quito}&profileKind=band`]}><DirectorySearchPage /></MemoryRouter>
        </QueryClientProvider>);
      });
      await waitFor(() => expect(container.textContent).toContain('Synthetic Event'));
      expect(searchMock).toHaveBeenCalledWith(expect.objectContaining({ entityType: 'event', cityId: quito, profileKind: undefined }), expect.any(AbortSignal));
      const selectedTab = container.querySelector('[role="tab"][aria-selected="true"]');
      expect(selectedTab?.textContent).toBe('Eventos (1)');
      const tabLabels = Array.from(container.querySelectorAll('[role="tab"]')).map((tab) => tab.textContent);
      expect(tabLabels).toEqual(['Todo (6)', 'Artistas y proyectos', 'Perfiles (3)', 'Clasificados (2)', 'Eventos (1)', 'Venues']);
      expect(container.textContent).toContain('Resultados orgánicos · 1');
      expect(container.textContent).not.toContain('Todavía no hay coincidencias');
      expect(container.textContent).toContain('Andes Brewing — La Pradera');
      const expectedDate = new Intl.DateTimeFormat('es', { dateStyle: 'medium', timeStyle: 'short' }).format(new Date(startsAt));
      expect(container.querySelector(`[aria-label="Fecha del evento: ${expectedDate}"]`)).not.toBeNull();
      expect(container.querySelector('a[href="/eventos/42"]')).not.toBeNull();
      // The city select shows Quito once the taxonomy has loaded.
      await waitFor(() => expect(container.querySelector('#directory-city-label + div')?.textContent).toContain('Quito'));
      // People-only filters are not offered for events; the genre filter stays.
      expect(container.querySelector('#directory-profession-label')).toBeNull();
      expect(container.querySelector('#directory-genre-label')).not.toBeNull();
      expect(container.textContent).toContain('no aplican a eventos ni venues');
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
    }
  }, 15_000);

  it.each(['constructor', 'toString', 'events'])('falls back to Todo for the unknown entityType %s', async (requested) => {
    const quito = '22222222-2222-4222-8222-222222222222';
    const { useLocation } = await import('react-router-dom');
    function LocationProbe() { return <span data-testid="search">{useLocation().search}</span>; }
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    try {
      await act(async () => {
        root.render(<QueryClientProvider client={queryClient}>
          <MemoryRouter initialEntries={[`/buscar?entityType=${requested}&cityId=${quito}`]}><DirectorySearchPage /><LocationProbe /></MemoryRouter>
        </QueryClientProvider>);
      });
      await waitFor(() => expect(container.textContent).toContain('Synthetic Bassist'));
      expect(container.querySelector('[role="tab"][aria-selected="true"]')?.textContent).toContain('Todo');
      expect(container.querySelector('[data-testid="search"]')?.textContent).toBe(`?cityId=${quito}`);
      expect(searchMock.mock.calls.every(([query]) => (query as { entityType?: string }).entityType === undefined)).toBe(true);
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
    }
  });

  it('pushes tab changes into history so back restores the previous search', async () => {
    const quito = '22222222-2222-4222-8222-222222222222';
    const { useLocation, useNavigate } = await import('react-router-dom');
    function HistoryProbe() {
      const location = useLocation();
      const navigate = useNavigate();
      return <div><span data-testid="search">{location.search}</span><button type="button" onClick={() => navigate(-1)}>history back</button></div>;
    }
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    const currentSearch = () => container.querySelector('[data-testid="search"]')?.textContent ?? '';
    try {
      await act(async () => {
        root.render(<QueryClientProvider client={queryClient}>
          <MemoryRouter initialEntries={[`/buscar?cityId=${quito}`]}><DirectorySearchPage /><HistoryProbe /></MemoryRouter>
        </QueryClientProvider>);
      });
      await waitFor(() => expect(container.textContent).toContain('Synthetic Bassist'));
      const eventsTab = Array.from(container.querySelectorAll<HTMLElement>('[role="tab"]')).find((tab) => tab.textContent?.startsWith('Eventos'))!;
      await act(async () => { fireEvent.click(eventsTab); });
      await waitFor(() => expect(currentSearch()).toBe(`?entityType=event&cityId=${quito}`));
      await waitFor(() => expect(searchMock).toHaveBeenCalledWith(expect.objectContaining({ entityType: 'event', cityId: quito }), expect.any(AbortSignal)));
      expect(container.querySelector('[role="tab"][aria-selected="true"]')?.textContent).toContain('Eventos');
      const back = Array.from(container.querySelectorAll('button')).find((button) => button.textContent === 'history back')!;
      await act(async () => { fireEvent.click(back); });
      await waitFor(() => expect(currentSearch()).toBe(`?cityId=${quito}`));
      // The previous (mixed) search is restored from the URL; its results come
      // from the query cache, so no third request is needed.
      await waitFor(() => expect(container.querySelector('[role="tab"][aria-selected="true"]')?.textContent).toContain('Todo'));
      expect(container.querySelector('#directory-profession-label')).not.toBeNull();
      expect(searchMock.mock.calls.filter(([query]) => (query as { entityType?: string }).entityType === 'event')).toHaveLength(1);
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
    }
  }, 15_000);

  it('drops nearby coordinates when back restores an earlier city search', async () => {
    const quito = '22222222-2222-4222-8222-222222222222';
    const { useLocation, useNavigate } = await import('react-router-dom');
    function HistoryProbe() {
      const location = useLocation();
      const navigate = useNavigate();
      return <div><span data-testid="search">{location.search}</span><button type="button" onClick={() => navigate(-1)}>history back</button></div>;
    }
    const geolocation = Object.getOwnPropertyDescriptor(navigator, 'geolocation');
    Object.defineProperty(navigator, 'geolocation', {
      configurable: true,
      value: { getCurrentPosition: (success: PositionCallback) => success({ coords: { latitude: -0.18, longitude: -78.48 } } as GeolocationPosition) },
    });
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    const currentSearch = () => container.querySelector('[data-testid="search"]')?.textContent ?? '';
    const button = (label: string) => Array.from(container.querySelectorAll('button')).find((candidate) => candidate.textContent?.includes(label))!;
    try {
      await act(async () => {
        root.render(<QueryClientProvider client={queryClient}>
          <MemoryRouter initialEntries={[`/buscar?cityId=${quito}`]}><DirectorySearchPage /><HistoryProbe /></MemoryRouter>
        </QueryClientProvider>);
      });
      await waitFor(() => expect(container.textContent).toContain('Synthetic Bassist'));
      // An older city entry stays in history behind the one nearby mode rewrites.
      const eventsTab = Array.from(container.querySelectorAll<HTMLElement>('[role="tab"]')).find((tab) => tab.textContent?.startsWith('Eventos'))!;
      await act(async () => { fireEvent.click(eventsTab); });
      await waitFor(() => expect(currentSearch()).toBe(`?entityType=event&cityId=${quito}`));

      // Entering nearby mode on a city URL keeps the coordinates and replaces the entry.
      await act(async () => { fireEvent.click(button(esDirectory.useLocation)); });
      await waitFor(() => expect(currentSearch()).toBe('?entityType=event&radiusKm=25'));
      await waitFor(() => expect(searchMock).toHaveBeenCalledWith(expect.objectContaining({ latitude: -0.18, longitude: -78.48, radiusKm: 25 }), expect.any(AbortSignal)));
      expect(container.querySelector('input[type="number"]')).not.toBeNull();

      // Back restores the earlier city search instead of rewriting it as nearby.
      await act(async () => { fireEvent.click(button('history back')); });
      await waitFor(() => expect(currentSearch()).toBe(`?cityId=${quito}`));
      await waitFor(() => expect(container.querySelector('[role="tab"][aria-selected="true"]')?.textContent).toContain('Todo'));
      await new Promise((resolve) => setTimeout(resolve, 50));
      expect(currentSearch()).toBe(`?cityId=${quito}`);
      // The city results come from the query cache; the radius field, shown
      // only while coordinates are held, is gone.
      expect(container.querySelector('input[type="number"]')).toBeNull();
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
      if (geolocation) Object.defineProperty(navigator, 'geolocation', geolocation);
      else delete (navigator as { geolocation?: unknown }).geolocation;
    }
  }, 15_000);
});
