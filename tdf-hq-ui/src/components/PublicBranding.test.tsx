import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';

jest.unstable_mockModule('./BrandLogo', () => ({
  default: () => <span>TDF Records</span>,
}));

const { default: PublicBranding } = await import('./PublicBranding');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const renderBranding = async (container: HTMLElement, route: string) => {
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(
      <MemoryRouter initialEntries={[route]} future={{ v7_startTransition: true, v7_relativeSplatPath: true }}>
        <PublicBranding>
          <main>Contenido publico</main>
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

describe('PublicBranding Instagram entry links', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
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
});
