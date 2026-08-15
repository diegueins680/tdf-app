import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';

import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

const searchResponse = {
  items: [{
    id: '11111111-1111-4111-8111-111111111111', type: 'profile', slug: 'synthetic-bassist',
    title: 'Synthetic Bassist', summary: 'Fixture público de prueba.',
    location: { city: 'Quito', countryCode: 'EC', precision: 'city' },
    sponsored: false, score: 0.8,
  }],
  sponsoredItems: [], facets: { entityTypes: { profile: 1 }, cities: [], total: 1 },
};

jest.unstable_mockModule('../api/directory', () => ({
  Directory: {
    search: jest.fn(async () => searchResponse),
    suggestions: jest.fn(async () => []),
    taxonomies: jest.fn(async () => ({
      locale: 'es', professions: [], classifiedCategories: [], compensationTypes: [], serviceOfferings: [], currencies: [], instruments: [], genres: [],
      cities: [{ id: '22222222-2222-4222-8222-222222222222', code: 'quito-ec-p', name: 'Quito', countryId: '33333333-3333-4333-8333-333333333333' }],
    })),
    addFavorite: jest.fn(async () => undefined),
  },
}));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: null }) }));
jest.unstable_mockModule('../hooks/useMetaTags', () => ({ useMetaTags: jest.fn() }));
jest.unstable_mockModule('../components/directory/OpenStreetMapResults', () => ({ default: () => <div>Mapa OSM aproximado</div> }));
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ capture: jest.fn() }) }));

const { default: DirectorySearchPage } = await import('./DirectorySearchPage');

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

describe('DirectorySearchPage', () => {
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
      expect(container.querySelector('main#main-content')).not.toBeNull();
      expect(container.textContent).toContain('Encuentra a la gente y las oportunidades que hacen música');
      expect(container.textContent).toContain('Quito');
      expect(container.textContent).toContain('Servicio');
      expect(container.textContent).toContain('Resultados orgánicos');
      await expectNoSeriousAccessibilityViolations(container);
    } finally {
      await act(async () => root.unmount());
      container.remove();
      queryClient.clear();
    }
  }, 15_000);
});
