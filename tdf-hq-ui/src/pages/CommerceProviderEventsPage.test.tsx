import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { CommercePaymentOverview, CommerceProviderEvent } from '../api/commerceOperations';

const getPaymentOverviewMock = jest.fn<() => Promise<CommercePaymentOverview>>();
const listProviderEventsMock = jest.fn<() => Promise<CommerceProviderEvent[]>>();
const replayProviderEventMock = jest.fn<(eventId: string, reason: string) => Promise<CommerceProviderEvent>>();

jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({ i18n: { language: 'es', resolvedLanguage: 'es' } }),
}));

jest.unstable_mockModule('../api/commerceOperations', () => ({
  CommerceOperations: {
    getPaymentOverview: () => getPaymentOverviewMock(),
    listProviderEvents: () => listProviderEventsMock(),
    replayProviderEvent: (eventId: string, reason: string) => replayProviderEventMock(eventId, reason),
  },
}));

const { default: CommerceProviderEventsPage } = await import('./CommerceProviderEventsPage');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const changeInputValue = async (input: HTMLInputElement, value: string) => {
  const valueDescriptor = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value');
  if (!valueDescriptor?.set) throw new Error('Input value setter not found');
  await act(async () => {
    valueDescriptor.set.call(input, value);
    input.dispatchEvent(new Event('input', { bubbles: true }));
    input.dispatchEvent(new Event('change', { bubbles: true }));
    await flushPromises();
  });
};

const waitFor = async (assertion: () => void, attempts = 20) => {
  let lastError: unknown;
  for (let index = 0; index < attempts; index += 1) {
    try {
      assertion();
      return;
    } catch (error) {
      lastError = error;
      await act(async () => {
        await flushPromises();
      });
    }
  }
  throw lastError;
};

const buildEvent = (overrides: Partial<CommerceProviderEvent> = {}): CommerceProviderEvent => ({
  cpeId: '00000000-0000-4000-8000-000000000010',
  cpeProvider: 'paypal',
  cpeEnvironment: 'sandbox',
  cpeProviderEventId: 'WH-FAILED-1',
  cpeEventType: 'PAYMENT.CAPTURE.COMPLETED',
  cpeProviderResourceId: 'CAPTURE-1',
  cpeStatus: 'dead_letter',
  cpeAttemptCount: 8,
  cpeCheckoutId: '00000000-0000-4000-8000-000000000011',
  cpePaymentAttemptId: null,
  cpeRefundId: null,
  cpeReceivedAt: '2026-08-14T12:00:00Z',
  cpeProviderCreatedAt: '2026-08-14T11:59:59Z',
  cpeProcessingStartedAt: null,
  cpeLastAttemptAt: '2026-08-14T12:10:00Z',
  cpeNextAttemptAt: null,
  cpeProcessedAt: null,
  cpeErrorSummary: 'Provider binding mismatch',
  ...overrides,
});

const buildOverview = (): CommercePaymentOverview => ({
  cpoGeneratedAt: '2026-09-11T12:00:00Z',
  cpoProviderAccounts: [
    {
      cpaProvider: 'datafast',
      cpaEnvironment: 'sandbox',
      cpaStatus: 'ready',
      cpaContractStatus: 'approved',
      cpaCredentialStatus: 'validated',
      cpaSettlementCurrency: 'USD',
      cpaEnabled: true,
      cpaFeatureEnabled: true,
      cpaVerifiedAt: '2026-09-11T11:00:00Z',
      cpaDisabledReason: null,
      cpaCapabilities: [{
        cpcPaymentMethod: 'card',
        cpcCapability: 'one_time',
        cpcVerificationStatus: 'sandbox_verified',
        cpcVerifiedAt: '2026-09-11T11:00:00Z',
      }],
    },
    {
      cpaProvider: 'paypal',
      cpaEnvironment: 'production',
      cpaStatus: 'disabled',
      cpaContractStatus: 'unverified',
      cpaCredentialStatus: 'absent',
      cpaSettlementCurrency: 'USD',
      cpaEnabled: false,
      cpaFeatureEnabled: false,
      cpaVerifiedAt: null,
      cpaDisabledReason: 'Activation requires contract and credential verification',
      cpaCapabilities: [],
    },
  ],
  cpoPaymentIntents: [{
    cpiStatus: 'captured',
    cpiCurrency: 'USD',
    cpiCount: 2,
    cpiAmountMinor: 5000,
    cpiAuthorizedMinor: 5000,
    cpiCapturedMinor: 5000,
    cpiRefundedMinor: 500,
  }],
  cpoAmountComponents: [{
    cacComponentType: 'tax',
    cacSource: 'tax_document',
    cacCurrency: 'USD',
    cacCount: 2,
    cacAmountMinor: 750,
  }],
  cpoCommissions: [{
    ccmProvider: 'paypal',
    ccmEnvironment: 'sandbox',
    ccmCurrency: 'USD',
    ccmCount: 1,
    ccmBasisAmountMinor: 5000,
    ccmCommissionMinor: 500,
    ccmProviderFeeMinor: 250,
    ccmTaxMinor: 75,
    ccmSellerNetMinor: 4175,
  }],
  cpoRefunds: [{
    crfProvider: 'paypal',
    crfEnvironment: 'sandbox',
    crfStatus: 'processing',
    crfCurrency: 'USD',
    crfCount: 1,
    crfAmountMinor: 500,
  }],
  cpoDisputes: [{
    cdsProvider: 'paypal',
    cdsEnvironment: 'sandbox',
    cdsKind: 'inquiry',
    cdsStatus: 'needs_response',
    cdsCurrency: 'USD',
    cdsCount: 1,
    cdsAmountMinor: 2500,
  }],
  cpoReconciliationExceptions: [{
    crsProvider: 'paypal',
    crsEnvironment: 'sandbox',
    crsStatus: 'open',
    crsCurrency: 'USD',
    crsCount: 1,
    crsExpectedMinor: 2500,
    crsActualMinor: 0,
  }],
  cpoSettlements: [{
    cssProvider: 'paypal',
    cssEnvironment: 'sandbox',
    cssStatus: 'reported',
    cssCurrency: 'USD',
    cssCount: 1,
    cssGrossMinor: 5000,
    cssFeeMinor: 250,
    cssWithholdingMinor: 100,
    cssRefundMinor: 500,
    cssChargebackMinor: 0,
    cssNetMinor: 4150,
  }],
  cpoSellerBalances: [{
    csbProvider: 'paypal',
    csbEnvironment: 'sandbox',
    csbAvailability: 'pending',
    csbCurrency: 'USD',
    csbEntryCount: 1,
    csbNetAmountMinor: 4175,
  }],
  cpoPayouts: [{
    cpsProvider: 'paypal',
    cpsEnvironment: 'sandbox',
    cpsStatus: 'pending_review',
    cpsCurrency: 'USD',
    cpsCount: 1,
    cpsAmountMinor: 4175,
  }],
});

describe('CommerceProviderEventsPage', () => {
  let container: HTMLDivElement;
  let root: Root;
  let queryClient: QueryClient;

  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
    if (!window.matchMedia) {
      Object.defineProperty(window, 'matchMedia', {
        writable: true,
        value: () => ({
          matches: false,
          media: '',
          onchange: null,
          addListener: () => undefined,
          removeListener: () => undefined,
          addEventListener: () => undefined,
          removeEventListener: () => undefined,
          dispatchEvent: () => false,
        }),
      });
    }
  });

  beforeEach(async () => {
    getPaymentOverviewMock.mockReset();
    listProviderEventsMock.mockReset();
    replayProviderEventMock.mockReset();
    listProviderEventsMock.mockResolvedValue([
      buildEvent(),
      buildEvent({
        cpeId: '00000000-0000-4000-8000-000000000020',
        cpeProviderEventId: 'WH-RETRY-1',
        cpeStatus: 'retry',
        cpeAttemptCount: 2,
        cpeErrorSummary: 'Provider temporarily unavailable',
      }),
    ]);
    getPaymentOverviewMock.mockResolvedValue(buildOverview());
    replayProviderEventMock.mockResolvedValue(buildEvent({ cpeStatus: 'retry' }));
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);
    queryClient = new QueryClient({
      defaultOptions: { queries: { retry: false, gcTime: 0 } },
    });
    await act(async () => {
      root.render(
        <MemoryRouter initialEntries={['/admin/commerce/provider-events']}>
          <QueryClientProvider client={queryClient}>
            <CommerceProviderEventsPage />
          </QueryClientProvider>
        </MemoryRouter>,
      );
      await flushPromises();
      await flushPromises();
    });
    await waitFor(() => {
      expect(container.querySelectorAll('[data-testid="commerce-provider-event-card"]')).toHaveLength(2);
    });
  });

  afterEach(async () => {
    await act(async () => {
      root.unmount();
      await flushPromises();
    });
    queryClient.clear();
    container.remove();
  });

  it('shows redacted evidence and offers replay only for dead-letter records', () => {
    expect(container.textContent).toContain('Preparación de proveedores');
    expect(container.textContent).toContain('datafast');
    expect(container.textContent).toContain('card/one_time');
    expect(container.textContent).toContain('Bloqueado');
    expect(container.textContent).toContain('Desglose financiero');
    expect(container.textContent).toContain('tax');
    expect(container.textContent).toContain('Comisiones');
    expect(container.textContent).toContain('needs_response');
    expect(container.textContent).toContain('PAYMENT.CAPTURE.COMPLETED');
    expect(container.textContent).toContain('Provider binding mismatch');
    expect(container.textContent).toContain('Provider temporarily unavailable');
    expect(container.textContent).not.toContain('payload_ciphertext');
    expect(container.textContent).not.toContain('merchant_account_ref');
    const replayButtons = Array.from(container.querySelectorAll('button'))
      .filter((button) => button.textContent?.includes('Reintentar evento'));
    expect(replayButtons).toHaveLength(1);
  });

  it('requires a remediation reason before the replay action is enabled', async () => {
    const replayButton = Array.from(container.querySelectorAll('button'))
      .find((button) => button.textContent?.includes('Reintentar evento'));
    expect(replayButton).toBeDefined();
    await act(async () => {
      replayButton?.click();
      await flushPromises();
    });
    const confirmButton = Array.from(document.body.querySelectorAll('button'))
      .find((button) => button.textContent?.includes('Registrar y reintentar'));
    expect(confirmButton).toBeDefined();
    expect(confirmButton?.disabled).toBe(true);
    expect(document.body.textContent).toContain('La evidencia queda inmutable');

    const reasonInput = document.body.querySelector<HTMLInputElement>('input[required]');
    expect(reasonInput).not.toBeNull();
    if (!reasonInput || !confirmButton) return;

    await changeInputValue(reasonInput, 'reparar');
    expect(confirmButton.disabled).toBe(true);

    await changeInputValue(reasonInput, '  Credenciales reparadas por operador  ');
    expect(confirmButton.disabled).toBe(false);
    await act(async () => {
      confirmButton.click();
      await flushPromises();
    });
    expect(replayProviderEventMock).toHaveBeenCalledWith(
      '00000000-0000-4000-8000-000000000010',
      'Credenciales reparadas por operador',
    );
  });
});
