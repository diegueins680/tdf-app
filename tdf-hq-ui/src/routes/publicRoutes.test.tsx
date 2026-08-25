import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter, Route, Routes, useLocation } from 'react-router-dom';

import { renderPublicRoutes } from './publicRoutes';

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
});
