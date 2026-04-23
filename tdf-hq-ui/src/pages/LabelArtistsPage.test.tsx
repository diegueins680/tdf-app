import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import type { ArtistProfileDTO, PartyDTO } from '../api/types';

const listArtistProfilesMock = jest.fn<() => Promise<ArtistProfileDTO[]>>();
const upsertArtistProfileMock = jest.fn<(payload: unknown) => Promise<ArtistProfileDTO | null>>();
const listPartiesMock = jest.fn<() => Promise<PartyDTO[]>>();
const updatePartyMock = jest.fn<(partyId: number, payload: unknown) => Promise<PartyDTO | null>>();

jest.unstable_mockModule('../api/admin', () => ({
  Admin: {
    listArtistProfiles: () => listArtistProfilesMock(),
    upsertArtistProfile: (payload: unknown) => upsertArtistProfileMock(payload),
  },
}));

jest.unstable_mockModule('../api/parties', () => ({
  Parties: {
    list: () => listPartiesMock(),
    update: (partyId: number, payload: unknown) => updatePartyMock(partyId, payload),
  },
}));

jest.unstable_mockModule('../components/GoogleDriveUploadWidget', () => ({
  default: () => null,
}));

const { default: LabelArtistsPage } = await import('./LabelArtistsPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const waitForExpectation = async (assertion: () => void, attempts = 12) => {
  let lastError: unknown;
  for (let index = 0; index < attempts; index += 1) {
    try {
      assertion();
      return;
    } catch (error) {
      lastError = error;
      await act(async () => {
        await flushPromises();
      });
    }
  }
  throw lastError;
};

const renderPage = async (container: HTMLElement) => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <QueryClientProvider client={qc}>
        <LabelArtistsPage />
      </QueryClientProvider>,
    );
    await flushPromises();
    await flushPromises();
  });

  return {
    cleanup: async () => {
      if (!root) return;
      await act(async () => {
        root?.unmount();
        await flushPromises();
      });
      root = null;
      qc.clear();
      document.body.removeChild(container);
    },
  };
};

const buildArtist = (overrides: Partial<ArtistProfileDTO> = {}): ArtistProfileDTO => ({
  apArtistId: 101,
  apDisplayName: 'La Ruta',
  apSlug: 'la-ruta',
  apBio: null,
  apCity: 'Quito',
  apHeroImageUrl: null,
  apSpotifyArtistId: null,
  apSpotifyUrl: null,
  apYoutubeChannelId: null,
  apYoutubeUrl: null,
  apWebsiteUrl: null,
  apFeaturedVideoUrl: null,
  apGenres: 'Indie',
  apHighlights: null,
  apFollowerCount: 12,
  apHasUserAccount: true,
  ...overrides,
});

const buildParty = (overrides: Partial<PartyDTO> = {}): PartyDTO => ({
  partyId: 101,
  displayName: 'La Ruta',
  isOrg: false,
  legalName: null,
  roles: ['Artist'],
  taxId: null,
  primaryEmail: 'laruta@example.com',
  primaryPhone: null,
  whatsapp: null,
  instagram: null,
  emergencyContact: null,
  notes: 'Pendiente bio',
  hasUserAccount: true,
  ...overrides,
});

const buttonText = (element: Element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim();

const getButtonsByText = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll<HTMLElement>('button')).filter(
    (button) => buttonText(button) === labelText,
  );

const hasTableHeader = (root: ParentNode, labelText: string) =>
  Array.from(root.querySelectorAll('th')).some((cell) => buttonText(cell) === labelText);

describe('LabelArtistsPage', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
    if (!window.matchMedia) {
      Object.defineProperty(window, 'matchMedia', {
        writable: true,
        value: () => ({
          matches: false,
          media: '',
          onchange: null,
          addListener: () => undefined,
          removeListener: () => undefined,
          addEventListener: () => undefined,
          removeEventListener: () => undefined,
          dispatchEvent: () => false,
        }),
      });
    }
  });

  beforeEach(() => {
    listArtistProfilesMock.mockReset();
    upsertArtistProfileMock.mockReset();
    listPartiesMock.mockReset();
    updatePartyMock.mockReset();
    listArtistProfilesMock.mockResolvedValue([]);
    listPartiesMock.mockResolvedValue([]);
    upsertArtistProfileMock.mockResolvedValue(null);
    updatePartyMock.mockResolvedValue(null);
  });

  it('keeps the first artist setup free of list-only search, refresh, notes, and table chrome', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('Label / Artistas');
        expect(getButtonsByText(container, 'Nuevo perfil')).toHaveLength(1);
        expect(container.textContent).toContain(
          'Todavía no hay perfiles de artista. Usa Nuevo perfil para enlazar el primer contacto del CRM; la búsqueda, notas rápidas, refresco y tabla aparecerán cuando exista al menos un perfil.',
        );
        expect(container.querySelector('input[aria-label="Buscar artistas"]')).toBeNull();
        expect(container.querySelector('button[aria-label="Refrescar artistas"]')).toBeNull();
        expect(container.textContent).not.toContain('Notas rápidas por artista');
        expect(hasTableHeader(container, 'Artista')).toBe(false);
        expect(hasTableHeader(container, 'Acciones')).toBe(false);
      });
    } finally {
      await cleanup();
    }
  });

  it('restores search, refresh, quick notes, and the comparison table once an artist exists', async () => {
    listArtistProfilesMock.mockResolvedValue([buildArtist()]);
    listPartiesMock.mockResolvedValue([buildParty()]);

    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPage(container);

    try {
      await waitForExpectation(() => {
        expect(container.textContent).toContain('La Ruta');
        expect(container.querySelector('input[aria-label="Buscar artistas"]')).not.toBeNull();
        expect(container.querySelector('button[aria-label="Refrescar artistas"]')).not.toBeNull();
        expect(container.textContent).toContain('Notas rápidas por artista');
        expect(hasTableHeader(container, 'Artista')).toBe(true);
        expect(hasTableHeader(container, 'Acciones')).toBe(true);
        expect(container.textContent).not.toContain('Todavía no hay perfiles de artista.');
      });
    } finally {
      await cleanup();
    }
  });
});
