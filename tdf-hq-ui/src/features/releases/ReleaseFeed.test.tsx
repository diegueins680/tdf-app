import { jest } from '@jest/globals';
import { createRef } from 'react';
import { fireEvent, render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import { ReleaseFeed } from './ReleaseFeed';
import type { ReleaseFeedItem } from './ReleasePlayerActions';

const release: ReleaseFeedItem = {
  arArtistId: 7,
  arReleaseId: 42,
  arTitle: 'Luna Baja',
  arReleaseDate: '2026-07-01',
  arDescription: 'Single nuevo',
  arCoverImageUrl: null,
  arSpotifyUrl: 'https://open.spotify.com/track/luna',
  arYoutubeUrl: null,
  artistName: 'Sofia Marquez',
};

const renderReleaseFeed = (overrides: Partial<Parameters<typeof ReleaseFeed>[0]> = {}) => {
  const onPlayRelease = jest.fn();
  const props: Parameters<typeof ReleaseFeed>[0] = {
    audioFileInputRef: createRef<HTMLInputElement>(),
    canManageReleases: false,
    canSeeReleaseFeed: true,
    enableFanRolePending: false,
    feedLimit: 4,
    hasAuthToken: true,
    hasFollows: true,
    hasReleaseTargets: true,
    isAuthenticated: true,
    isFan: true,
    isHomeManagerView: false,
    loading: false,
    loginPath: '/login?redirect=/fans',
    pendingUploadRelease: null,
    releaseAudioMap: {},
    releaseFeed: [release],
    releaseLinkDraft: '',
    streamingFallbacks: new Map(),
    uploadError: null,
    uploadingReleaseId: null,
    visibleFeed: [release],
    onCancelUpload: jest.fn(),
    onDriveUploadComplete: jest.fn(),
    onEnableFanRole: jest.fn(),
    onPlayRelease,
    onReleaseLinkDraftChange: jest.fn(),
    onSaveReleaseLink: jest.fn(),
    onShowLess: jest.fn(),
    onShowMore: jest.fn(),
    onUploadTrigger: jest.fn(),
    ...overrides,
  };

  render(
    <MemoryRouter future={{ v7_relativeSplatPath: true, v7_startTransition: true }}>
      <ReleaseFeed {...props} />
    </MemoryRouter>,
  );

  return { onPlayRelease };
};

describe('ReleaseFeed', () => {
  it('renders release cards and forwards play actions', () => {
    const { onPlayRelease } = renderReleaseFeed();

    expect(screen.getByText('Novedades de tus artistas')).not.toBeNull();
    expect(screen.getAllByText('Luna Baja').length).toBeGreaterThan(0);
    fireEvent.click(screen.getByRole('button', { name: 'Escuchar' }));

    expect(onPlayRelease).toHaveBeenCalledWith(release);
  });

  it('renders login calls to action for signed-out visitors', () => {
    renderReleaseFeed({
      canSeeReleaseFeed: false,
      hasAuthToken: false,
      isAuthenticated: false,
      releaseFeed: [],
      visibleFeed: [],
    });

    expect(screen.getByText('Ingresa con tu cuenta para ver lanzamientos personalizados y seguir artistas.')).not.toBeNull();
    expect(screen.getByRole('link', { name: 'Inicia sesión' }).getAttribute('href')).toBe('/login?redirect=/fans');
    expect(screen.getByRole('link', { name: 'Crear cuenta fan' }).getAttribute('href')).toBe(
      '/login?signup=1&roles=Fan&redirect=/fans',
    );
  });
});
