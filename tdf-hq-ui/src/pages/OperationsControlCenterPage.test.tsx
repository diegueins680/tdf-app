import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';
import '../i18n';

const item = {
  id: '11111111-1111-4111-8111-111111111111',
  organizationId: '00000000-0000-4000-8000-000000000001',
  branchId: '00000000-0000-4000-8000-000000000002',
  sourceSystem: 'tdf-hq', sourceChannel: 'web', entityType: 'course_registration', entityId: '7',
  uncorrelated: false, correlationKey: 'course_registration:7', titleEs: 'Inscripción requiere atención',
  titleEn: 'Registration needs attention', descriptionEs: 'Revisar inscripción', descriptionEn: 'Review registration',
  status: 'new', priority: 'high', recommendedPriority: 'high', severity: 'warning', seen: false,
  firstSeenBy: null, firstSeenAt: null, assigneePartyId: null, responsibleTeam: null, customerPartyId: null,
  serviceKey: null, amountMinor: null, currency: null, paymentState: null,
  createdAt: '2026-08-09T12:00:00Z', updatedAt: '2026-08-09T12:00:00Z', dueAt: '2026-08-10T12:00:00Z',
  snoozedUntil: null, waitingReason: null, waitingExternalDependency: false, resumeAt: null,
  resolvedAt: null, archivedAt: null, slaState: 'on_track', version: 1, metadata: {},
} as const;

jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({ locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' }),
}));

jest.unstable_mockModule('../api/operations', () => ({
  Operations: {
    metrics: () => Promise.resolve({
      newRegistrations: 1, registrationsRequiringAttention: 1, reservationsAwaitingConfirmation: 0,
      todaySessions: 0, schedulingConflicts: 0, unpaidInvoices: 0, overdueInvoices: 0,
      paymentsAwaitingVerification: 0, revenueReceivedTodayMinor: 0, unassignedWork: 1,
      slaBreaches: 0, averageFirstResponseSeconds: null, averageResolutionSeconds: null,
      integrationFailures: 0, currency: 'USD', calculatedAt: '2026-08-09T12:00:00Z',
    }),
    list: () => Promise.resolve({ items: [item], nextCursor: null, hasMore: false }),
    detail: () => Promise.resolve({ workItem: item, events: [], notes: [], allowedTransitions: ['seen'], sourceRecordUrl: null, quickActions: ['review'] }),
    markSeen: () => new Promise(() => undefined),
    events: () => Promise.resolve({ events: [], lastEventId: null, retryAfterMs: 15000 }),
    savedViews: () => Promise.resolve([]),
    transition: jest.fn(), assign: jest.fn(), addNote: jest.fn(), saveView: jest.fn(),
  },
}));

const { default: OperationsControlCenterPage } = await import('./OperationsControlCenterPage');

describe('OperationsControlCenterPage', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('renders persisted work in KPI, Kanban, and inbox with accessible semantics', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root: Root = createRoot(container);
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false, refetchInterval: false, networkMode: 'always' } } });
    queryClient.setQueryData(['operations', 'work-items', { limit: 60 }], { items: [item], nextCursor: null, hasMore: false });
    queryClient.setQueryData(['operations', 'metrics'], {
      newRegistrations: 1, registrationsRequiringAttention: 1, reservationsAwaitingConfirmation: 0,
      todaySessions: 0, schedulingConflicts: 0, unpaidInvoices: 0, overdueInvoices: 0,
      paymentsAwaitingVerification: 0, revenueReceivedTodayMinor: 0, unassignedWork: 1,
      slaBreaches: 0, averageFirstResponseSeconds: null, averageResolutionSeconds: null,
      integrationFailures: 0, currency: 'USD', calculatedAt: '2026-08-09T12:00:00Z',
    });
    try {
      await act(async () => {
        root.render(
          <QueryClientProvider client={queryClient}>
            <MemoryRouter><OperationsControlCenterPage /></MemoryRouter>
          </QueryClientProvider>,
        );
        await new Promise((resolve) => setTimeout(resolve, 50));
      });
      expect(container.textContent).toContain('Centro de control de operaciones');
      expect(container.textContent).toContain('Inscripción requiere atención');
      expect(container.textContent).toContain('Bandeja operativa');
      await expectNoSeriousAccessibilityViolations(container);
    } finally {
      await act(async () => root.unmount());
      queryClient.clear();
      container.remove();
    }
  });
});
