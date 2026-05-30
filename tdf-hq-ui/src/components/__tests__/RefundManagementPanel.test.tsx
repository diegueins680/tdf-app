import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ReactNode } from 'react';

const listRefunds = jest.fn<() => Promise<unknown>>();
const approveRefund = jest.fn<() => Promise<unknown>>();
const rejectRefund = jest.fn<() => Promise<unknown>>();

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

  return ({ children }: { children: ReactNode }) => (
    <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  );
};

describe('RefundManagementPanel', () => {
  const mockRefunds = [
    {
      refundId: 'refund-1',
      refundOrderId: 'order-1-aaaa',
      refundReason: 'Cannot attend event',
      refundAmountCents: 5000,
      refundStatus: 'pending',
      refundStripeRefundId: null,
      refundRejectionReason: null,
      refundProcessedAt: null,
      refundCreatedAt: '2026-05-20T10:00:00Z',
    },
    {
      refundId: 'refund-2',
      refundOrderId: 'order-2-bbbb',
      refundReason: 'Event cancelled',
      refundAmountCents: 7500,
      refundStatus: 'approved',
      refundStripeRefundId: 're_123456',
      refundRejectionReason: null,
      refundProcessedAt: '2026-05-19T15:00:00Z',
      refundCreatedAt: '2026-05-19T10:00:00Z',
    },
  ];

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('renders refund list', async () => {
    listRefunds.mockResolvedValue(mockRefunds);

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('USD 50.00')).toBeInTheDocument();
      expect(screen.getByText('USD 75.00')).toBeInTheDocument();
    });
  });

  it('shows loading state', () => {
    listRefunds.mockImplementation(() => new Promise(() => {}));

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
      refundProcessedAt: '2026-05-20T12:00:00Z',
    });

    // Approval is gated behind a window.confirm.
    global.confirm = jest.fn(() => true);

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('USD 50.00')).toBeInTheDocument();
    });

    const approveButton = screen.getByRole('button', { name: /Approve/i });
    fireEvent.click(approveButton);

    await waitFor(() => {
      expect(approveRefund).toHaveBeenCalledWith('event-1', 'refund-1');
    });
  });

  it('rejects refund request with reason', async () => {
    listRefunds.mockResolvedValue(mockRefunds);
    rejectRefund.mockResolvedValue({
      ...mockRefunds[0],
      refundStatus: 'rejected',
      refundProcessedAt: '2026-05-20T12:00:00Z',
    });

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('USD 50.00')).toBeInTheDocument();
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
      expect(screen.getByText('PENDING')).toBeInTheDocument();
      expect(screen.getByText('APPROVED')).toBeInTheDocument();
    });
  });

  it('formats refund amounts correctly', async () => {
    listRefunds.mockResolvedValue(mockRefunds);

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('USD 50.00')).toBeInTheDocument();
      expect(screen.getByText('USD 75.00')).toBeInTheDocument();
    });
  });

  it('invokes approval even though the panel has no inline error surface', async () => {
    listRefunds.mockResolvedValue(mockRefunds);
    approveRefund.mockRejectedValue(new Error('Stripe refund failed'));
    global.confirm = jest.fn(() => true);

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('USD 50.00')).toBeInTheDocument();
    });

    const approveButton = screen.getByRole('button', { name: /Approve/i });
    fireEvent.click(approveButton);

    await waitFor(() => {
      expect(approveRefund).toHaveBeenCalledWith('event-1', 'refund-1');
    });

    // A failed approval leaves the row in place (no crash, no inline alert).
    expect(screen.getByText('USD 50.00')).toBeInTheDocument();
  });

  it('only renders action buttons for pending refunds', async () => {
    listRefunds.mockResolvedValue(mockRefunds);

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('USD 50.00')).toBeInTheDocument();
    });

    // Only the single pending refund exposes Approve/Reject actions.
    expect(screen.getAllByRole('button', { name: /Approve/i })).toHaveLength(1);
    expect(screen.getAllByRole('button', { name: /^Reject$/i })).toHaveLength(1);
  });
});
