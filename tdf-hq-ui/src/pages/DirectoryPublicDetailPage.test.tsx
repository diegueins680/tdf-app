import { jest } from '@jest/globals';
jest.unstable_mockModule('../features/interactions/InteractionPanel', () => ({ InteractionPanel: () => null }));
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { MemoryRouter, Route, Routes, useLocation } from 'react-router-dom';

import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

const profileMock = jest.fn<(slug: string) => Promise<Record<string, unknown>>>();
const profileReviewsMock = jest.fn(async () => ({ items: [], nextCursor: null }));
const reviewEligibilityMock = jest.fn(async () => []);
const directoryRsvpFeedMock = jest.fn(async () => ({ feedItems: [], feedNextCursor: null }));
let sessionMock: {
  username: string;
  displayName: string;
  roles: string[];
  modules: string[];
  partyId: number;
} | null = null;

jest.unstable_mockModule('../api/directory', () => ({
  Directory: {
    profile: profileMock,
    event: profileMock,
    venue: profileMock,
    profileReviews: profileReviewsMock,
    reviewEligibility: reviewEligibilityMock,
    createReview: jest.fn(),
  },
}));

jest.unstable_mockModule('../api/socialEvents', () => ({
  SocialEventsAPI: {
    deleteMyRsvp: jest.fn(),
    listDirectoryProfileRsvpFeed: jest.fn(async () => ({
      feedItems: [],
      feedNextCursor: null,
    })),
    listRsvpFeed: jest.fn(async () => ({
      feedItems: [],
      feedNextCursor: null,
    })),
  },
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  getActiveSession: () => sessionMock,
  getStoredSessionToken: () => null,
  useSession: () => ({ session: sessionMock }),
}));

jest.unstable_mockModule('../hooks/useMetaTags', () => ({ useMetaTags: jest.fn() }));
// Keep the real feed component while isolating its explicit domain API boundary.
// Do not invent successful low-level transport methods merely to satisfy imports.
jest.unstable_mockModule('../api/socialEvents', () => ({
  SocialEventsAPI: { listDirectoryProfileRsvpFeed: directoryRsvpFeedMock },
}));
const { ApiError } = await import('../api/client');
jest.unstable_mockModule('../api/client', () => ({ ApiError, API_BASE_URL: 'https://tdf-hq.example.test' }));

const { default: DirectoryPublicDetailPage } = await import('./DirectoryPublicDetailPage');

const profile = {
  id: 'profile-17',
  slug: 'ana',
  name: 'Ana Sintética',
  bio: 'Perfil profesional sintético para pruebas.',
  canonicalUrl: '/directorio/ana',
  kind: 'person',
  portfolio: [],
  professions: [],
  instruments: [],
  genres: [],
  reputation: { reviewAverage: null, reviewCount: 0 },
};

function LocationProbe() {
  const location = useLocation();
  return <output aria-label="Destino protegido">{`${location.pathname}${location.search}`}</output>;
}

function renderPage(initialEntry: string) {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false },
    },
  });
  const view = render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={[initialEntry]}>
        <Routes>
          <Route path="/directorio/:slug" element={<DirectoryPublicDetailPage kind="profile" />} />
          <Route path="/eventos/:eventId" element={<DirectoryPublicDetailPage kind="event" />} />
          <Route path="/venues/:venueId" element={<DirectoryPublicDetailPage kind="venue" />} />
          <Route path="/social/eventos/:eventId" element={<LocationProbe />} />
          <Route path="/mis-clasificados" element={<LocationProbe />} />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  );
  return { ...view, queryClient };
}

describe('DirectoryPublicDetailPage contact continuity', () => {
  beforeEach(() => {
    jest.spyOn(navigator, 'language', 'get').mockReturnValue('es-EC');
    sessionMock = null;
    profileMock.mockReset().mockResolvedValue(profile);
    profileReviewsMock.mockReset().mockResolvedValue({ items: [], nextCursor: null });
    reviewEligibilityMock.mockReset().mockResolvedValue([]);
    directoryRsvpFeedMock.mockReset().mockResolvedValue({ feedItems: [], feedNextCursor: null });
  });

  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('routes a public moment discussion to the existing media page', async () => {
    const view = renderPage('/eventos/121?moment=987');
    expect((await screen.findByLabelText('Destino protegido')).textContent).toBe('/social/eventos/121?moment=987');
    expect(profileMock).toHaveBeenCalledWith('121');
    view.unmount();
    view.queryClient.clear();
  });

  it('does not redirect an unavailable public event to its protected contents', async () => {
    profileMock.mockRejectedValue(new Error('unavailable'));
    const view = renderPage('/eventos/121?moment=987');
    expect(await screen.findByText('Este contenido no está publicado, vigente o disponible.')).toBeTruthy();
    expect(screen.queryByLabelText('Destino protegido')).toBeNull();
    view.unmount();
    view.queryClient.clear();
  });

  it('gives profile, review, and eligibility loading states distinct accessible names', async () => {
    sessionMock = {
      username: 'ana-fan',
      displayName: 'Ana Fan',
      roles: ['customer'],
      modules: [],
      partyId: 42,
    };
    let resolveProfile: ((value: Record<string, unknown>) => void) | undefined;
    profileMock.mockImplementation(() => new Promise((resolve) => {
      resolveProfile = resolve;
    }));
    profileReviewsMock.mockImplementation(() => new Promise(() => undefined));
    reviewEligibilityMock.mockImplementation(() => new Promise(() => undefined));
    const view = renderPage('/directorio/ana');

    expect(await screen.findByRole('progressbar', { name: 'Cargando perfil del directorio' })).toBeTruthy();
    await act(async () => {
      resolveProfile?.(profile);
    });
    expect(await screen.findByRole('progressbar', { name: 'Cargando reseñas' })).toBeTruthy();
    expect(screen.getByRole('progressbar', { name: 'Comprobando interacciones elegibles' })).toBeTruthy();
    view.queryClient.clear();
  });

  it('sends a guest through authentication with an exact profile-bound return path', async () => {
    const view = renderPage('/directorio/ana');

    const contact = await screen.findByRole('link', { name: 'Ingresar para contactar' });
    expect(contact.getAttribute('href')).toBe(
      '/login?redirect=%2Fdirectorio%2Fana%3Fresume%3Dcontact%26profileId%3Dprofile-17&intent=professional_tools',
    );
    expect(profileMock).toHaveBeenCalledWith('ana');
    expect(reviewEligibilityMock).not.toHaveBeenCalled();
    expect(directoryRsvpFeedMock).not.toHaveBeenCalled();
    await waitFor(() => expect(screen.queryByRole('progressbar')).toBeNull());
    await expectNoSeriousAccessibilityViolations(view.container);
    view.queryClient.clear();
  });

  it('offers an explicit protected composer transition only for the exact authenticated target', async () => {
    sessionMock = {
      username: 'ana-fan',
      displayName: 'Ana Fan',
      roles: ['customer'],
      modules: [],
      partyId: 42,
    };
    const view = renderPage('/directorio/ana?resume=contact&profileId=profile-17');

    expect(await screen.findByText('Continúa tu contacto con Ana Sintética')).toBeTruthy();
    expect(screen.getByText('Nada se enviará automáticamente.', { exact: false })).toBeTruthy();
    const continueLink = screen.getByRole('link', { name: 'Revisar y escribir mensaje' });
    expect(continueLink.getAttribute('href')).toBe(
      '/mis-clasificados?contact=profile-17&contextKind=profile',
    );

    fireEvent.click(continueLink);
    expect((await screen.findByRole('status', { name: 'Destino protegido' })).textContent).toBe(
      '/mis-clasificados?contact=profile-17&contextKind=profile',
    );
    view.queryClient.clear();
  });

  it('fails closed for a mismatched target and keeps contact explicit', async () => {
    sessionMock = {
      username: 'ana-fan',
      displayName: 'Ana Fan',
      roles: ['customer'],
      modules: [],
      partyId: 42,
    };
    const view = renderPage('/directorio/ana?resume=contact&profileId=profile-99');

    expect(await screen.findByText('¿Quieres contactar este perfil?')).toBeTruthy();
    expect(screen.queryByText('Continúa tu contacto con Ana Sintética')).toBeNull();
    expect(screen.getByRole('link', { name: 'Contactar desde uno de mis perfiles' }).getAttribute('href')).toBe(
      '/mis-clasificados?contact=profile-17&contextKind=profile',
    );
    expect(await screen.findByText('Todavía no hay actividad de RSVP visible.')).toBeTruthy();
    expect(directoryRsvpFeedMock).toHaveBeenCalledWith('ana', undefined, 20);
    view.queryClient.clear();
  });

  it('does not resume contact or read account activity for a guest with forged resume parameters', async () => {
    const view = renderPage('/directorio/ana?resume=contact&profileId=profile-17');

    expect(await screen.findByRole('link', { name: 'Ingresar para contactar' })).toBeTruthy();
    expect(screen.queryByText('Continúa tu contacto con Ana Sintética')).toBeNull();
    expect(screen.queryByRole('link', { name: 'Revisar y escribir mensaje' })).toBeNull();
    expect(directoryRsvpFeedMock).not.toHaveBeenCalled();
    expect(reviewEligibilityMock).not.toHaveBeenCalled();
    view.queryClient.clear();
  });

  it('offers the same explicit target-bound continuation in English', async () => {
    jest.spyOn(navigator, 'language', 'get').mockReturnValue('en-US');
    sessionMock = { username: 'ana-fan', displayName: 'Ana Fan', roles: ['customer'], modules: [], partyId: 42 };
    const view = renderPage('/directorio/ana?resume=contact&profileId=profile-17');

    expect(await screen.findByText('Continue your contact with Ana Sintética')).toBeTruthy();
    expect(screen.getByRole('link', { name: 'Review and write a message' }).getAttribute('href')).toBe(
      '/mis-clasificados?contact=profile-17&contextKind=profile',
    );
    expect(screen.getByRole('link', { name: 'Not now' }).getAttribute('href')).toBe('/directorio/ana');
    expect(screen.getByText('Nothing will be sent automatically.', { exact: false })).toBeTruthy();
    view.queryClient.clear();
  });

  it('cancels the resumed action by removing its query state', async () => {
    sessionMock = {
      username: 'ana-fan',
      displayName: 'Ana Fan',
      roles: ['customer'],
      modules: [],
      partyId: 42,
    };
    const view = renderPage('/directorio/ana?resume=contact&profileId=profile-17');

    const cancel = await screen.findByRole('link', { name: 'Ahora no' });
    fireEvent.click(cancel);

    await waitFor(() => {
      expect(screen.queryByText('Continúa tu contacto con Ana Sintética')).toBeNull();
      expect(screen.getByText('¿Quieres contactar este perfil?')).toBeTruthy();
    });
    expect(cancel.getAttribute('href')).toBe('/directorio/ana');
    view.queryClient.clear();
  });

  it('shows the kind placeholder on the detail page when the profile has no media', async () => {
    const view = renderPage('/directorio/ana');
    const image = await screen.findByRole('img', { name: 'Imagen de referencia de Ana Sintética' });
    expect(image.getAttribute('data-preview-source')).toBe('placeholder');
    expect(image.getAttribute('src')).toContain('/artist-fallback.svg');
    view.unmount();
    view.queryClient.clear();
  });

  it('shows the canonical preview image on the profile detail page', async () => {
    profileMock.mockResolvedValue({ ...profile, previewImageUrl: 'https://cdn.example.test/ana.jpg' });
    const view = renderPage('/directorio/ana');
    const image = await screen.findByRole('img', { name: 'Foto de Ana Sintética' });
    expect(image.getAttribute('data-preview-source')).toBe('media');
    expect(image.getAttribute('src')).toBe('https://cdn.example.test/ana.jpg');
    view.unmount();
    view.queryClient.clear();
  });

  it('shows the venue image from the public venue projection', async () => {
    profileMock.mockResolvedValue({
      id: 22,
      name: 'Venue Sintético',
      capacity: 120,
      location: { city: 'Quito', countryCode: 'EC', precision: 'city' },
      imageUrl: 'https://cdn.example.test/venue.jpg',
      canonicalUrl: '/venues/22',
    });
    const view = renderPage('/venues/22');
    const image = await screen.findByRole('img', { name: 'Foto de Venue Sintético' });
    expect(image.getAttribute('src')).toBe('https://cdn.example.test/venue.jpg');
    view.unmount();
    view.queryClient.clear();
  });
});
