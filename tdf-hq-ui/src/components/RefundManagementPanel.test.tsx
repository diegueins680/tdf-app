import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import type { RefundDTO, RejectionReasonDTO } from '../api/socialEvents';

const listRefundsMock = jest.fn<(eventId: string) => Promise<RefundDTO[]>>();
const approveRefundMock = jest.fn<(eventId: string, refundId: string) => Promise<RefundDTO>>();
const rejectRefundMock = jest.fn<(
  eventId: string,
  refundId: string,
  data: RejectionReasonDTO,
) => Promise<RefundDTO>>();

jest.unstable_mockModule('../api/socialEvents', () => ({
  SocialEventsAPI: {
    listRefunds: (eventId: string) => listRefundsMock(eventId),
    approveRefund: (eventId: string, refundId: string) => approveRefundMock(eventId, refundId),
    rejectRefund: (eventId: string, refundId: string, data: RejectionReasonDTO) =>
      rejectRefundMock(eventId, refundId, data),
  },
}));

const { RefundManagementPanel } = await import('./RefundManagementPanel');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const waitForExpectation = async (assertion: () => void, attempts = 12) => {
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

const buttonText = (element: Element) => (element.textContent ?? '').replace(/\s+/g, ' ').trim();

const getButtonByText = (root: ParentNode, labelText: string) => {
  const button = Array.from(root.querySelectorAll<HTMLButtonElement>('button')).find(
    (candidate) => buttonText(candidate) === labelText,
  );
  if (!(button instanceof HTMLButtonElement)) {
    throw new Error(`Button not found: ${labelText}`);
  }
  return button;
};

const getTextControlByLabel = (root: ParentNode, labelText: string) => {
  const label = Array.from(root.querySelectorAll<HTMLLabelElement>('label')).find(
    (element) => buttonText(element).replace(/\s+\*$/, '') === labelText,
  );
  if (!(label instanceof HTMLLabelElement) || !label.htmlFor) {
    throw new Error(`Label not found: ${labelText}`);
  }

  const control = label.ownerDocument.getElementById(label.htmlFor);
  if (!(control instanceof HTMLInputElement) && !(control instanceof HTMLTextAreaElement)) {
    throw new Error(`Text control not found: ${labelText}`);
  }
  return control;
};

const clickButton = async (button: HTMLElement) => {
  await act(async () => {
    button.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    await flushPromises();
    await flushPromises();
  });
};

const changeTextControlValue = async (control: HTMLInputElement | HTMLTextAreaElement, value: string) => {
  const prototype = control instanceof HTMLTextAreaElement
    ? HTMLTextAreaElement.prototype
    : HTMLInputElement.prototype;
  const valueDescriptor = Object.getOwnPropertyDescriptor(prototype, 'value');
  if (!valueDescriptor?.set) throw new Error('Text control value setter not found');

  await act(async () => {
    valueDescriptor.set.call(control, value);
    control.dispatchEvent(new Event('input', { bubbles: true }));
    control.dispatchEvent(new Event('change', { bubbles: true }));
    await flushPromises();
    await flushPromises();
  });
};

const buildRefund = (overrides: Partial<RefundDTO> = {}): RefundDTO => ({
  refundId: ' refund-selected-42 ',
  refundOrderId: 'order-12345678',
  refundReason: 'Customer request',
  refundAmountCents: 2500,
  refundStatus: 'pending',
  refundCreatedAt: '2030-01-02T03:04:05.000Z',
  ...overrides,
});

const renderPanel = async (container: HTMLElement, eventId = 'event-7') => {
  const qc = new QueryClient({
    defaultOptions: { queries: { retry: false, gcTime: 0 } },
  });
  let root: Root | null = createRoot(container);

  await act(async () => {
    root?.render(
      <QueryClientProvider client={qc}>
        <RefundManagementPanel eventId={eventId} />
      </QueryClientProvider>,
    );
    await flushPromises();
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
      qc.clear();
      document.body.removeChild(container);
    },
  };
};

describe('RefundManagementPanel', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(() => {
    listRefundsMock.mockReset();
    approveRefundMock.mockReset();
    rejectRefundMock.mockReset();
    listRefundsMock.mockResolvedValue([buildRefund()]);
    approveRefundMock.mockResolvedValue(buildRefund({ refundStatus: 'approved' }));
    rejectRefundMock.mockResolvedValue(buildRefund({ refundStatus: 'rejected' }));
  });

  it('rejects the selected refund with trimmed identifiers and reason text', async () => {
    const container = document.createElement('div');
    document.body.appendChild(container);
    const { cleanup } = await renderPanel(container);

    try {
      await waitForExpectation(() => {
        expect(listRefundsMock).toHaveBeenCalledWith('event-7');
        expect(container.textContent).toContain('Customer request');
      });

      await clickButton(getButtonByText(container, 'Reject'));

      await waitForExpectation(() => {
        expect(document.body.textContent).toContain('Reject Refund Request');
      });

      await changeTextControlValue(getTextControlByLabel(document.body, 'Rejection Reason'), '  Missing receipt  ');
      await clickButton(getButtonByText(document.body, 'Reject Refund'));

      await waitForExpectation(() => {
        expect(rejectRefundMock).toHaveBeenCalledWith('event-7', 'refund-selected-42', {
          rrReason: 'Missing receipt',
        });
        expect(approveRefundMock).not.toHaveBeenCalled();
      });
    } finally {
      await cleanup();
    }
  });
});
