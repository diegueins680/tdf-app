import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { MemoryRouter, Route, Routes, useLocation } from 'react-router-dom';

import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

const profileMock = jest.fn<(slug: string) => Promise<Record<string, unknown>>>();
const profileReviewsMock = jest.fn(async () => ({ items: [], nextCursor: null }));
const reviewEligibilityMock = jest.fn(async () => []);
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
jest.unstable_mockModule('../api/client', () => ({
  API_BASE_URL: 'https://tdf-hq.example.test',
  ApiError: class ApiError extends Error {},
  del: jest.fn(),
  get: jest.fn(),
  getPendingApiRequestCount: jest.fn(() => 0),
  patch: jest.fn(),
  post: jest.fn(),
  postEmpty: jest.fn(),
  postForm: jest.fn(),
  postText: jest.fn(),
  put: jest.fn(),
  subscribeToApiActivity: jest.fn(() => jest.fn()),
}));

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
          <Route path="/mis-clasificados" element={<LocationProbe />} />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  );
  return { ...view, queryClient };
}

describe('DirectoryPublicDetailPage contact continuity', () => {
  beforeEach(() => {
    sessionMock = null;
    profileMock.mockReset().mockResolvedValue(profile);
    profileReviewsMock.mockReset().mockResolvedValue({ items: [], nextCursor: null });
    reviewEligibilityMock.mockReset().mockResolvedValue([]);
  });

  afterEach(() => {
    cleanup();
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
});
