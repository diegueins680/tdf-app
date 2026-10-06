import i18n from '../i18n';
import { jest } from '@jest/globals';
import { act } from 'react';
import { waitFor } from '@testing-library/dom';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

let activeSession: {
  username: string;
  displayName: string;
  roles: string[];
  partyId: number;
} | null = null;
const logoutMock = jest.fn();

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({
    session: activeSession,
    loading: false,
    login: jest.fn(),
    logout: logoutMock,
    setApiToken: jest.fn(),
  }),
}));



jest.unstable_mockModule('./BrandLogo', () => ({
  default: () => <span>TDF Records</span>,
}));

const { default: PublicBranding } = await import('./PublicBranding');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderBranding = async (container: HTMLElement, route: string | { pathname: string; state: { mobileInvitation: boolean } }) => {
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(
      <MemoryRouter initialEntries={[route]}>
        <PublicBranding>
          <div>Contenido publico</div>
        </PublicBranding>
      </MemoryRouter>,
    );
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
      document.body.removeChild(container);
    },
  };
};

const linkHrefByText = (container: HTMLElement, label: string) => {
  const banner = container.querySelector<HTMLElement>('[aria-label="Opciones para visitantes desde Instagram"]');
  if (!banner) throw new Error('Instagram banner not found');
  const link = Array.from(banner.querySelectorAll<HTMLAnchorElement>('a')).find(
    (candidate) => candidate.textContent?.trim() === label,
  );
  if (!link) throw new Error(`Link not found: ${label}`);
  return link.getAttribute('href');
};

describe('PublicBranding', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(async () => {
    await i18n.changeLanguage('es');
    activeSession = null;
    logoutMock.mockClear();
    window.sessionStorage.clear();
  });

  it('shows service and course links for Instagram-tagged traffic', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderBranding(container, '/tdf?utm_source=instagram');

    try {
      expect(container.textContent).toContain('TDF desde Instagram');
      expect(container.textContent).toContain('Reservas, cursos y clases en un solo lugar.');
      expect(linkHrefByText(container, 'Reservar servicios')).toBe(
        '/reservar?utm_source=instagram&utm_medium=social&utm_campaign=instagram_public_links',
      );
      expect(linkHrefByText(container, 'DJ Booth')).toBe(
        '/dj-booth?utm_source=instagram&utm_medium=social&utm_campaign=instagram_public_links',
      );
      expect(linkHrefByText(container, 'Inscribirme: cursos')).toBe(
        '/curso/produccion-musical?utm_source=instagram&utm_medium=social&utm_campaign=instagram_public_links',
      );
      expect(linkHrefByText(container, 'Clases de prueba')).toBe(
        '/trials?utm_source=instagram&utm_medium=social&utm_campaign=instagram_public_links',
      );
    } finally {
      await cleanup();
    }
  });

  it('keeps the Instagram links visible for the rest of the browser session', async () => {
    const firstContainer = document.createElement('div');
    document.body.appendChild(firstContainer);
    const first = await renderBranding(firstContainer, '/tdf?utm_source=ig');
    await first.cleanup();

    const secondContainer = document.createElement('div');
    document.body.appendChild(secondContainer);
    const second = await renderBranding(secondContainer, '/records');

    try {
      expect(secondContainer.textContent).toContain('TDF desde Instagram');
    } finally {
      await second.cleanup();
    }
  });

  it('does not show Instagram-specific links for normal public traffic', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderBranding(container, '/tdf');

    try {
      expect(container.textContent).not.toContain('TDF desde Instagram');
      expect(container.textContent).not.toContain('Reservar servicios');
    } finally {
      await cleanup();
    }
  });

  it('provides semantic landmarks and a keyboard skip target', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderBranding(container, '/tdf');

    try {
      const skipLink = container.querySelector<HTMLAnchorElement>('a[href="#main-content"]');
      const main = container.querySelector<HTMLElement>('main#main-content');
      expect(skipLink?.textContent).toContain('Saltar al contenido principal');
      expect(main?.getAttribute('tabindex')).toBe('-1');
      expect(container.querySelector('header')).not.toBeNull();
      expect(container.querySelector('nav[aria-label="Navegación principal"]')).not.toBeNull();
      expect(container.querySelector('footer')).not.toBeNull();
    } finally {
      await cleanup();
    }
  });

  it('shows the session menu instead of a login action for authenticated visitors', async () => {
    activeSession = {
      username: 'fabro',
      displayName: 'Fabro',
      roles: ['artist'],
      partyId: 42,
    };
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderBranding(container, '/fans');

    try {
      expect(container.textContent).not.toContain('Ingresar');
      const sessionButton = container.querySelector<HTMLButtonElement>(
        'button[aria-label="Abrir menú de sesión"]',
      );
      expect(sessionButton).not.toBeNull();

      await act(async () => {
        sessionButton?.dispatchEvent(new MouseEvent('click', { bubbles: true }));
        await flushPromises();
      });

      expect(document.body.textContent).toContain('Cerrar sesión');
      const logoutItem = Array.from(document.body.querySelectorAll<HTMLElement>('[role="menuitem"]'))
        .find((item) => item.textContent?.includes('Cerrar sesión'));
      expect(logoutItem).not.toBeUndefined();

      await act(async () => {
        logoutItem?.dispatchEvent(new MouseEvent('click', { bubbles: true }));
        await flushPromises();
      });

      expect(logoutMock).toHaveBeenCalledTimes(1);
    } finally {
      await cleanup();
    }
  });

  it('has no critical or serious automated accessibility violations', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderBranding(container, '/tdf');

    try {
      await expectNoSeriousAccessibilityViolations(container);
    } finally {
      await cleanup();
    }
  });
});

it('does not duplicate profile recruitment with a generic mobile banner', async () => {
  Object.defineProperty(navigator, 'userAgent', { configurable: true, value: 'Android' });
  const container = document.createElement('div'); document.body.appendChild(container);
  const view = await renderBranding(container, '/artista/demo');
  try {
    await waitFor(() => expect(container.querySelector('main aside[aria-label="TDF Mobile"]')).toBeTruthy());
    expect(container.querySelectorAll('main aside[aria-label="TDF Mobile"]').length).toBe(1);
    expect(Array.from(container.querySelectorAll('button')).some(button => button.textContent === 'Ahora no')).toBe(false);
  } finally { await view.cleanup(); }
});
it('places the signup invitation before public destination content inside main', async () => {
  activeSession = { username: 'tester', displayName: 'Tester', roles: ['fan'], partyId: 1 };
  const container = document.createElement('div'); document.body.appendChild(container);
  const view = await renderBranding(container, { pathname: '/fans', state: { mobileInvitation: true } });
  try {
    await waitFor(() => expect(container.querySelector('main aside[aria-label="TDF Mobile"]')).toBeTruthy());
    const invitation = container.querySelector('main aside[aria-label="TDF Mobile"]')!;
    const content = Array.from(container.querySelectorAll('main div')).find(node => node.textContent === 'Contenido publico')!;
    expect(invitation.compareDocumentPosition(content) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(container.querySelectorAll('main aside[aria-label="TDF Mobile"]').length).toBe(1);
  } finally { await view.cleanup(); activeSession = null; }
});
