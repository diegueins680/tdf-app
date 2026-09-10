import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import type { MerchDispute, MerchRefund, MerchSettlement, MerchStorefront } from '../api/merch';
import { expectNoSeriousAccessibilityViolations } from '../test/accessibility';

(globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

const updateSettlementStatusMock = jest.fn<(
  settlementId: string,
  status: 'approved' | 'held',
  reason?: string | null,
) => Promise<MerchSettlement>>();
let settlementStatus: MerchSettlement['status'] = 'held';
let financialOperationsEnabled = false;

const refund = {
  id: '9f000000-0000-4000-8000-000000000001',
  orderId: '94000000-0000-4000-8000-000000000001',
  orderNumber: 'TDF-SYNTHETIC-0001',
  storeId: '92000000-0000-4000-8000-000000000001',
  storeName: 'Synthetic Pilot Band',
  issueId: '9e000000-0000-4000-8000-000000000001',
  status: 'requested',
  amountMinor: 5500,
  currency: 'USD',
  reasonCode: 'customer_request',
  requestNote: 'Synthetic refund request for interface verification.',
  provider: 'paypal',
  providerRefundId: null,
  requestedBy: 900005,
  requestedByName: 'Synthetic Requester',
  approvedBy: null,
  approvedByName: null,
  createdAt: '2026-09-10T14:00:00Z',
  completedAt: null,
  settlementStatus: 'paid',
  executionAvailable: false,
  executionMessage: 'Provider execution is not available.',
} satisfies MerchRefund;

const dispute = {
  id: '9a000000-0000-4000-8000-000000000001',
  orderId: refund.orderId,
  orderNumber: refund.orderNumber,
  storeId: refund.storeId,
  storeName: refund.storeName,
  providerDisputeId: 'synthetic-provider-dispute',
  kind: 'inquiry',
  status: 'needs_response',
  amountMinor: 5500,
  currency: 'USD',
  reasonCode: 'product_not_received',
  openedAt: '2026-09-10T14:05:00Z',
  dueAt: null,
  closedAt: null,
  readOnly: true,
} satisfies MerchDispute;

const store = {
  id: '92000000-0000-4000-8000-000000000001',
  displayName: 'Synthetic Pilot Band',
  slug: 'synthetic-pilot-band',
  operationalStatus: 'active',
} as unknown as MerchStorefront;

const buildSettlement = (): MerchSettlement => ({
  id: '9d000000-0000-4000-8000-000000000001',
  storeId: store.id,
  storeName: store.displayName,
  periodStart: '2026-09-01T00:00:00Z',
  periodEnd: '2026-10-01T00:00:00Z',
  currency: 'USD',
  status: settlementStatus,
  grossProductMinor: 2000,
  discountsMinor: 0,
  taxesMinor: 0,
  shippingMinor: 200,
  processorFeesMinor: 0,
  tdfCommissionMinor: 0,
  refundsMinor: 0,
  adjustmentsMinor: 0,
  sellerNetMinor: 2200,
  preparedBy: 900005,
  preparedByName: 'Synthetic Preparer',
  approvedBy: null,
  approvedByName: null,
  paidBy: null,
  paidByName: null,
  approvedAt: null,
  paidAt: null,
  evidenceObjectKey: null,
  evidenceMimeType: null,
  evidenceByteSize: null,
  evidenceChecksumSha256: null,
  externalReference: null,
  orderCount: 1,
});

jest.unstable_mockModule('../api/merch', () => ({
  createMerchIdempotencyKey: () => 'settlement-payment:synthetic-idempotency-key',
  Merch: {
    capabilities: () => Promise.resolve({ features: { refundOperations: financialOperationsEnabled, disputeMonitoring: financialOperationsEnabled } }),
    adminStores: () => Promise.resolve([store]),
    adminProducts: () => Promise.resolve([]),
    adminIssues: () => Promise.resolve([]),
    adminRefunds: () => Promise.resolve(financialOperationsEnabled ? [refund] : []),
    adminDisputes: () => Promise.resolve(financialOperationsEnabled ? [dispute] : []),
    adminSettlements: () => Promise.resolve([buildSettlement()]),
    settlementEligibleOrders: () => Promise.resolve([]),
    reviewStore: jest.fn(),
    reviewProduct: jest.fn(),
    updateAdminIssue: jest.fn(),
    createAdminRefund: jest.fn(),
    reviewAdminRefund: jest.fn(),
    createSettlement: jest.fn(),
    updateSettlementStatus: (
      settlementId: string,
      status: 'approved' | 'held',
      reason?: string | null,
    ) => updateSettlementStatusMock(settlementId, status, reason),
    recordSettlementPayment: jest.fn(),
  },
}));

jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({ i18n: { language: 'es', resolvedLanguage: 'es' } }),
}));

const { default: MerchAdminPage } = await import('./MerchAdminPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

async function renderPage() {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(<QueryClientProvider client={queryClient}><MerchAdminPage /></QueryClientProvider>);
  });
  for (let attempt = 0; attempt < 20 && !container.textContent?.includes('Revisión del piloto de merch'); attempt += 1) {
    await act(async () => {
      await flushPromises();
    });
  }
  if (!container.textContent?.includes('Revisión del piloto de merch')) {
    throw new Error('Merch administration queries did not finish rendering');
  }
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

describe('merch settlement administration', () => {
  beforeEach(() => {
    settlementStatus = 'held';
    financialOperationsEnabled = false;
    updateSettlementStatusMock.mockReset();
    updateSettlementStatusMock.mockResolvedValue(buildSettlement());
  });

  it('allows an independent reviewer to resolve a hold without implying a payout', async () => {
    const view = await renderPage();
    try {
      expect(view.container.textContent).toContain('TDF nunca envía dinero desde esta pantalla');
      const approve = Array.from(view.container.querySelectorAll<HTMLButtonElement>('button'))
        .find((button) => button.textContent?.includes('Resolver espera y aprobar'));
      expect(approve).toBeDefined();
      await act(async () => {
        approve?.click();
        await flushPromises();
      });
      expect(updateSettlementStatusMock).toHaveBeenCalledWith(
        '9d000000-0000-4000-8000-000000000001',
        'approved',
        null,
      );
    } finally {
      await view.cleanup();
    }
  });

  it('labels approved evidence recording as non-transferring and has no serious axe violations', async () => {
    settlementStatus = 'approved';
    const view = await renderPage();
    try {
      expect(view.container.textContent).toContain('Este formulario registra evidencia; nunca envía dinero');
      const submit = Array.from(view.container.querySelectorAll<HTMLButtonElement>('button'))
        .find((button) => button.textContent?.includes('Registrar pago verificado'));
      expect(submit?.disabled).toBe(true);
      await expectNoSeriousAccessibilityViolations(view.container);
    } finally {
      await view.cleanup();
    }
  });

  it('keeps refund approval non-executing and provider disputes read-only', async () => {
    financialOperationsEnabled = true;
    const view = await renderPage();
    try {
      expect(view.container.textContent).toContain('Aprobar solo la solicitud');
      expect(view.container.textContent).toContain('este flujo no ha movido dinero');
      expect(view.container.textContent).toContain('Evidencia de solo lectura');
      expect(view.container.textContent).toContain('Consulta');
      expect(view.container.textContent).toContain('Requiere respuesta');
      expect(view.container.textContent).not.toContain('Resolver disputa');
      await expectNoSeriousAccessibilityViolations(view.container);
    } finally {
      await view.cleanup();
    }
  });
});
