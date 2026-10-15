import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes, useLocation } from 'react-router-dom';

import { LegacyDjBoothRedirect, renderPublicRoutes } from './publicRoutes';

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

function LocationProbe() {
  const location = useLocation();
  return (
    <output data-testid="location">
      {`${location.pathname}${location.search}${location.hash}`}
    </output>
  );
}

describe('public routes', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('redirects the legacy events URL before the public wildcard route', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    let root: Root | null = createRoot(container);

    try {
      await act(async () => {
        root?.render(
          <MemoryRouter initialEntries={['/social/events?city=Quito#agenda']}>
            <Routes>
              {renderPublicRoutes()}
              <Route path="/social/eventos/*" element={<LocationProbe />} />
            </Routes>
          </MemoryRouter>,
        );
        await flushPromises();
      });

      expect(container.querySelector('[data-testid="location"]')?.textContent)
        .toBe('/social/eventos?city=Quito#agenda');
    } finally {
      await act(async () => {
        root?.unmount();
        await flushPromises();
      });
      root = null;
      document.body.removeChild(container);
    }
  });

  it.each([
    '/dj-booth?utm_source=instagram#horario',
    '/dj-booth?service=recording&servicio=mixing&utm_source=instagram#horario',
    '/dj-booth?service=recording&service=mixing&utm_source=instagram#horario',
  ])('preselects DJ booth and preserves tracking for %s', async (legacyUrl) => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    let root: Root | null = createRoot(container);

    try {
      await act(async () => {
        root?.render(
          <MemoryRouter initialEntries={[legacyUrl]}>
            <Routes>
              <Route path="/dj-booth" element={<LegacyDjBoothRedirect />} />
              <Route path="/reservar" element={<LocationProbe />} />
            </Routes>
          </MemoryRouter>,
        );
        await flushPromises();
      });

      const target = new URL(container.querySelector('[data-testid="location"]')?.textContent ?? '', 'https://example.test');
      expect(target.pathname).toBe('/reservar');
      expect(target.searchParams.getAll('servicio')).toEqual(['dj-booth-practice']);
      expect(target.searchParams.has('service')).toBe(false);
      expect(target.searchParams.get('utm_source')).toBe('instagram');
      expect(target.hash).toBe('#horario');
    } finally {
      await act(async () => {
        root?.unmount();
        await flushPromises();
      });
      root = null;
      document.body.removeChild(container);
    }
  });

});
