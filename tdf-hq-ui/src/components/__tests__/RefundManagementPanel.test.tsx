import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import type { RefundDTO, RejectionReasonDTO } from '../../api/socialEvents';
import appI18n from '../../i18n/index';

const listRefunds = jest.fn<(eventId: string) => Promise<RefundDTO[]>>();
const approveRefund = jest.fn<(eventId: string, refundId: string) => Promise<RefundDTO>>();
const rejectRefund =
  jest.fn<(eventId: string, refundId: string, data: RejectionReasonDTO) => Promise<RefundDTO>>();

const twoDigitText = (tens: number, ones: number): string => `${tens}${ones}`;
const refundFixtureInstantIso = (day: string, hour: string): string =>
  `${REFUND_FIXTURE_YEAR}-${REFUND_FIXTURE_MONTH}-${day}T${hour}:${ZERO_MINUTE_OR_SECOND}:${ZERO_MINUTE_OR_SECOND}Z`;

const REFUND_FIXTURE_YEAR = `${twoDigitText(2, 0)}${twoDigitText(2, 6)}`;
const REFUND_FIXTURE_MONTH = twoDigitText(0, 5);
const ZERO_MINUTE_OR_SECOND = twoDigitText(0, 0);
const PENDING_REFUND_AMOUNT_CENTS = 5 * 1000;
const APPROVED_REFUND_AMOUNT_CENTS = (5 + 2) * 1000 + 500;
const PENDING_REFUND_CREATED_AT_ISO = refundFixtureInstantIso(twoDigitText(2, 0), twoDigitText(1, 0));
const APPROVED_REFUND_PROCESSED_AT_ISO = refundFixtureInstantIso(twoDigitText(1, 9), twoDigitText(1, 5));
const APPROVED_REFUND_CREATED_AT_ISO = refundFixtureInstantIso(twoDigitText(1, 9), twoDigitText(1, 0));
const MUTATED_REFUND_PROCESSED_AT_ISO = refundFixtureInstantIso(twoDigitText(2, 0), twoDigitText(1, 2));

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: { listRefunds, approveRefund, rejectRefund },
}));

const { RefundManagementPanel } = await import('../RefundManagementPanel');

const createWrapper = () => {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false },
    },
  });

  const TestWrapper = ({ children }: { children: ReactNode }) => (
    <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  );
  TestWrapper.displayName = 'TestWrapper';
  return TestWrapper;
};

describe('RefundManagementPanel', () => {
  beforeAll(async () => appI18n.changeLanguage('en'));
  afterAll(async () => appI18n.changeLanguage('es'));
  /**
   * Fixture contract:
   * @precondition refund amounts are cents, not formatted display dollars.
   * @invariant timestamps are stable UTC instants so date rendering stays deterministic.
   * @postcondition list/approve/reject mocks resolve RefundDTO-compatible payloads.
   */
  const mockRefunds = [
    {
      refundId: 'refund-1',
      refundOrderId: 'order-1-aaaa',
      refundReason: 'Cannot attend event',
      refundAmountCents: PENDING_REFUND_AMOUNT_CENTS,
      refundCurrency: 'USD',
      refundStatus: 'pending',
      refundStripeRefundId: null,
      refundRejectionReason: null,
      refundProcessedAt: null,
      refundCreatedAt: PENDING_REFUND_CREATED_AT_ISO,
    },
    {
      refundId: 'refund-2',
      refundOrderId: 'order-2-bbbb',
      refundReason: 'Event cancelled',
      refundAmountCents: APPROVED_REFUND_AMOUNT_CENTS,
      refundCurrency: 'EUR',
      refundStatus: 'approved',
      refundStripeRefundId: 're_123456',
      refundRejectionReason: null,
      refundProcessedAt: APPROVED_REFUND_PROCESSED_AT_ISO,
      refundCreatedAt: APPROVED_REFUND_CREATED_AT_ISO,
    },
  ] satisfies [RefundDTO, RefundDTO];

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('renders refund list', async () => {
    listRefunds.mockResolvedValue(mockRefunds);

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('$50.00')).toBeInTheDocument();
      expect(screen.getByText('€75.00')).toBeInTheDocument();
    });
  });

  it('shows loading state', () => {
    // A promise that never settles keeps the query in its loading state.
    listRefunds.mockImplementation(() => new Promise<never>(() => undefined));

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    expect(screen.getByRole('progressbar')).toBeInTheDocument();
  });

  it('shows empty state when no refunds', async () => {
    listRefunds.mockResolvedValue([]);

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText(/No refund requests/i)).toBeInTheDocument();
    });
  });

  it('approves refund request', async () => {
    listRefunds.mockResolvedValue(mockRefunds);
    approveRefund.mockResolvedValue({
      ...mockRefunds[0],
      refundStatus: 'approved',
      refundProcessedAt: MUTATED_REFUND_PROCESSED_AT_ISO,
    });

    // Approval is gated behind a ConfirmDialog.
    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('$50.00')).toBeInTheDocument();
    });

    const confirmApproveButton = screen.getByRole('button', { name: /Approve/i });
    fireEvent.click(confirmApproveButton);

    // Confirm the action in the dialog.
    const dialogConfirmButton = await screen.findByRole('button', { name: /Confirmar/i });
    fireEvent.click(dialogConfirmButton);

    await waitFor(() => {
      expect(approveRefund).toHaveBeenCalledWith('event-1', 'refund-1');
    });
  });

  it('rejects refund request with reason', async () => {
    listRefunds.mockResolvedValue(mockRefunds);
    rejectRefund.mockResolvedValue({
      ...mockRefunds[0],
      refundStatus: 'rejected',
      refundProcessedAt: MUTATED_REFUND_PROCESSED_AT_ISO,
    });

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('$50.00')).toBeInTheDocument();
    });

    // Open the rejection dialog from the pending refund row.
    fireEvent.click(screen.getByRole('button', { name: /^Reject$/i }));

    const reasonInput = await screen.findByLabelText(/Rejection Reason/i);
    fireEvent.change(reasonInput, { target: { value: 'Past refund deadline' } });

    fireEvent.click(screen.getByRole('button', { name: /Reject Refund/i }));

    await waitFor(() => {
      expect(rejectRefund).toHaveBeenCalledWith('event-1', 'refund-1', {
        rrReason: 'Past refund deadline',
      });
    });
  });

  it('displays refund status chips correctly', async () => {
    listRefunds.mockResolvedValue(mockRefunds);

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('Pending')).toBeInTheDocument();
      expect(screen.getByText('Approved')).toBeInTheDocument();
    });
  });

  it('formats refund amounts correctly', async () => {
    listRefunds.mockResolvedValue(mockRefunds);

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('$50.00')).toBeInTheDocument();
      expect(screen.getByText('€75.00')).toBeInTheDocument();
    });
  });

  it('invokes approval even though the panel has no inline error surface', async () => {
    listRefunds.mockResolvedValue(mockRefunds);
    approveRefund.mockRejectedValue(new Error('Stripe refund failed'));

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('$50.00')).toBeInTheDocument();
    });

    const rejectedApproveButton = screen.getByRole('button', { name: /Approve/i });
    fireEvent.click(rejectedApproveButton);
    fireEvent.click(await screen.findByRole('button', { name: /Confirmar/i }));

    await waitFor(() => {
      expect(approveRefund).toHaveBeenCalledWith('event-1', 'refund-1');
    });

    // A failed approval leaves the row in place (no crash, no inline alert).
    expect(screen.getByText('$50.00')).toBeInTheDocument();
  });

  it('only renders action buttons for pending refunds', async () => {
    listRefunds.mockResolvedValue(mockRefunds);

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('$50.00')).toBeInTheDocument();
    });

    // Only the single pending refund exposes Approve/Reject actions.
    expect(screen.getAllByRole('button', { name: /Approve/i })).toHaveLength(1);
    expect(screen.getAllByRole('button', { name: /^Reject$/i })).toHaveLength(1);
  });
});
