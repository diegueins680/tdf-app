import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import type { CommerceProviderEvent } from '../api/commerceOperations';

const listProviderEventsMock = jest.fn<() => Promise<CommerceProviderEvent[]>>();
const replayProviderEventMock = jest.fn<(eventId: string, reason: string) => Promise<CommerceProviderEvent>>();

jest.unstable_mockModule('react-i18next', () => ({
  useTranslation: () => ({ i18n: { language: 'es', resolvedLanguage: 'es' } }),
}));

jest.unstable_mockModule('../api/commerceOperations', () => ({
  CommerceOperations: {
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
