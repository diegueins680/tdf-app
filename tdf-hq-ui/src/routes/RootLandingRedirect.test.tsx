import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { MemoryRouter, Route, Routes, useLocation } from 'react-router-dom';

const sessionState: { session: { username: string } | null; loading: boolean } = { session: null, loading: true };
jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => sessionState,
}));

// The shell carousel reaches the API client, which this routing test does not exercise.
jest.unstable_mockModule('../components/events/UpcomingEventsCarousel', () => ({
  default: () => null,
  EVENTS_PATH: '/social/eventos',
}));

const { RootLandingRedirect } = await import('./publicRoutes');

function LocationProbe() {
  const location = useLocation();
  return <output data-testid="location">{location.pathname}</output>;
}

async function renderRoot() {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);
  const render = async () => {
    await act(async () => {
      root.render(
        <MemoryRouter initialEntries={['/']}>
          <Routes>
            <Route path="/" element={<RootLandingRedirect />} />
            <Route path="*" element={<LocationProbe />} />
          </Routes>
        </MemoryRouter>,
      );
    });
  };
  await render();
  return {
    container,
    render,
    cleanup: async () => {
      await act(async () => root.unmount());
      container.remove();
    },
  };
}

describe('RootLandingRedirect', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('waits for session bootstrap when only a server cookie exists, then lands on Comunidad', async () => {
    sessionState.session = null;
    sessionState.loading = true;
    const view = await renderRoot();
    try {
      // No premature redirect to the guest start page while /session is pending.
      expect(view.container.querySelector('[data-testid="location"]')).toBeNull();
      expect(view.container.querySelector('[aria-busy="true"]')).not.toBeNull();
      sessionState.session = { username: 'llamaestepez@gmail.com' };
      sessionState.loading = false;
      await view.render();
      expect(view.container.querySelector('[data-testid="location"]')?.textContent).toBe('/fans');
    } finally {
      await view.cleanup();
    }
  });

  it('does not trust an expired cached session before bootstrap verifies it', async () => {
    sessionState.session = { username: 'expired@example.com' };
    sessionState.loading = true;
    const view = await renderRoot();
    try {
      expect(view.container.querySelector('[data-testid="location"]')).toBeNull();
      expect(view.container.querySelector('[aria-busy="true"]')).not.toBeNull();
      // The server rejects the cached session: the visitor is a guest.
      sessionState.session = null;
      sessionState.loading = false;
      await view.render();
      expect(view.container.querySelector('[data-testid="location"]')?.textContent).toBe('/inicio');
    } finally {
      await view.cleanup();
    }
  });

  it('sends guests to the public start page once bootstrap finds no session', async () => {
    sessionState.session = null;
    sessionState.loading = false;
    const view = await renderRoot();
    try {
      expect(view.container.querySelector('[data-testid="location"]')?.textContent).toBe('/inicio');
    } finally {
      await view.cleanup();
    }
  });
});
