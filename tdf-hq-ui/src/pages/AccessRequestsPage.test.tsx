import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';

import type { CreateFeatureAccessRequest, FeatureAccessRequestDTO } from '../api/accessRequests';
import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

const createMock = jest.fn<(payload: CreateFeatureAccessRequest) => Promise<FeatureAccessRequestDTO>>();
const captureMock = jest.fn();

jest.unstable_mockModule('../api/accessRequests', () => ({
  AccessRequests: {
    listMine: jest.fn(),
    create: (payload: CreateFeatureAccessRequest) => createMock(payload),
    listReview: jest.fn(),
    decide: jest.fn(),
    cancel: jest.fn(),
  },
}));

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({
    session: {
      username: 'readonly',
      displayName: 'Read only',
      roles: ['readonly'],
      modules: ['crm', 'catalog'],
      partyId: 42,
    },
  }),
}));

jest.unstable_mockModule('../analytics/posthog', () => ({
  getAnalyticsClient: () => ({
    ready: true,
    capture: captureMock,
    identify: jest.fn(),
    reset: jest.fn(),
    page: jest.fn(),
  }),
}));

jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({ i18n: { language: 'es', resolvedLanguage: 'es' } }),
}));

const { NewAccessRequestPage } = await import('./AccessRequestsPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const buildRequest = (): FeatureAccessRequestDTO => ({
  id: 7,
  requesterPartyId: 42,
  featureId: 'label.ddex.inbox',
  action: 'import',
  roleContext: ['ReadOnly'],
  moduleContext: ['CRM', 'Catalog'],
  status: 'pending',
  reviewerGroup: 'label-reviewers',
  justification: 'Necesito importar un ERN.',
  reviewerNotes: null,
  requestedAt: '2026-08-06T12:00:00Z',
  updatedAt: '2026-08-06T12:00:00Z',
  decidedAt: null,
  cancelledAt: null,
  expiresAt: '2026-09-05T12:00:00Z',
  history: [],
});

async function renderPage(path: string) {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const queryClient = new QueryClient({ defaultOptions: { mutations: { retry: false } } });
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(
      <MemoryRouter initialEntries={[path]}>
        <QueryClientProvider client={queryClient}>
          <NewAccessRequestPage />
        </QueryClientProvider>
      </MemoryRouter>,
    );
    await flushPromises();
  });
  return {
    container,
    cleanup: async () => {
      await act(async () => {
        root?.unmount();
        await flushPromises();
      });
      root = null;
      queryClient.clear();
      container.remove();
    },
  };
}

describe('internal access request flow', () => {
  beforeEach(() => {
    createMock.mockReset();
    captureMock.mockReset();
  });

  it('submits the exact locked feature action without broad role data in telemetry', async () => {
    createMock.mockResolvedValue(buildRequest());
    const view = await renderPage('/solicitudes-acceso/nueva?feature=label.ddex.inbox&action=import');
    try {
      expect(view.container.textContent).toContain('DDEX / Bandeja');
      expect(view.container.textContent).toContain('Categoría de acceso faltante');
      const textArea = view.container.querySelector<HTMLTextAreaElement>('textarea');
      const submit = Array.from(view.container.querySelectorAll<HTMLButtonElement>('button'))
        .find((button) => button.textContent?.includes('Enviar solicitud'));
      expect(textArea).not.toBeNull();
      expect(submit).not.toBeUndefined();
      await act(async () => {
        if (textArea) {
          const setter = Object.getOwnPropertyDescriptor(HTMLTextAreaElement.prototype, 'value')?.set;
          setter?.call(textArea, 'Necesito importar un ERN.');
          textArea.dispatchEvent(new Event('input', { bubbles: true }));
          textArea.dispatchEvent(new Event('change', { bubbles: true }));
        }
        submit?.click();
        await flushPromises();
        await flushPromises();
      });
      expect(createMock).toHaveBeenCalledWith({
        featureId: 'label.ddex.inbox',
        action: 'import',
        justification: 'Necesito importar un ERN.',
      });
      expect(captureMock).toHaveBeenCalledWith('feature_access_request_submitted', {
        feature_id: 'label.ddex.inbox',
        feature_action: 'import',
      });
    } finally {
      await view.cleanup();
    }
  });

  it('does not create requests for technical routes', async () => {
    const view = await renderPage('/solicitudes-acceso/nueva?feature=technical.auth-login&action=view');
    try {
      expect(view.container.textContent).toContain('no admite solicitudes de acceso');
      expect(view.container.querySelector('form')).toBeNull();
      expect(createMock).not.toHaveBeenCalled();
    } finally {
      await view.cleanup();
    }
  });

  it('has no serious automated accessibility violations in the request form', async () => {
    const view = await renderPage('/solicitudes-acceso/nueva?feature=label.ddex.inbox&action=import');
    try {
      await expectNoSeriousAccessibilityViolations(view.container);
    } finally {
      await view.cleanup();
    }
  });
});
