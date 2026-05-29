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
      trrId: 'refund-1',
      trrOrderId: 'order-1',
      trrStatus: 'pending',
      trrReason: 'Cannot attend event',
      trrRequestedAt: '2026-05-20T10:00:00Z',
      trrProcessedAt: null,
      trrStripeRefundId: null,
      trrRefundAmount: 5000,
      order: {
        etoId: 'order-1',
        etoEventId: 'event-1',
        etoTierId: 'tier-1',
        etoBuyerEmail: 'john@example.com',
        etoBuyerName: 'John Doe',
        etoQuantity: 2,
        etoTotalAmountCents: 10000,
        etoStatus: 'completed',
      },
    },
    {
      trrId: 'refund-2',
      trrOrderId: 'order-2',
      trrStatus: 'approved',
      trrReason: 'Event cancelled',
      trrRequestedAt: '2026-05-19T10:00:00Z',
      trrProcessedAt: '2026-05-19T15:00:00Z',
      trrStripeRefundId: 're_123456',
      trrRefundAmount: 7500,
      order: {
        etoId: 'order-2',
        etoEventId: 'event-1',
        etoTierId: 'tier-1',
        etoBuyerEmail: 'jane@example.com',
        etoBuyerName: 'Jane Smith',
        etoQuantity: 1,
        etoTotalAmountCents: 7500,
        etoStatus: 'refunded',
      },
    },
  ];

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('renders refund list', async () => {
    listRefunds.mockResolvedValue(
      mockRefunds
    );

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('John Doe')).toBeInTheDocument();
      expect(screen.getByText('Jane Smith')).toBeInTheDocument();
    });
  });

  it('shows loading state', () => {
    listRefunds.mockImplementation(
      () => new Promise(() => {})
    );

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
    listRefunds.mockResolvedValue(
      mockRefunds
    );
    approveRefund.mockResolvedValue({
      ...mockRefunds[0],
      trrStatus: 'approved',
      trrProcessedAt: '2026-05-20T12:00:00Z',
    });

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('John Doe')).toBeInTheDocument();
    });

    const approveButton = screen.getAllByRole('button', { name: /Approve/i })[0];
    fireEvent.click(approveButton);

    await waitFor(() => {
      expect(approveRefund).toHaveBeenCalledWith(
        'event-1',
        'refund-1'
      );
    });
  });

  it('rejects refund request with reason', async () => {
    listRefunds.mockResolvedValue(
      mockRefunds
    );
    rejectRefund.mockResolvedValue({
      ...mockRefunds[0],
      trrStatus: 'rejected',
      trrProcessedAt: '2026-05-20T12:00:00Z',
    });

    // Mock window.prompt
    global.prompt = jest.fn(() => 'Past refund deadline');

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('John Doe')).toBeInTheDocument();
    });

    const rejectButton = screen.getAllByRole('button', { name: /Reject/i })[0];
    fireEvent.click(rejectButton);

    await waitFor(() => {
      expect(rejectRefund).toHaveBeenCalledWith(
        'event-1',
        'refund-1',
        'Past refund deadline'
      );
    });
  });

  it('displays refund status chips correctly', async () => {
    listRefunds.mockResolvedValue(
      mockRefunds
    );

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('Pending')).toBeInTheDocument();
      expect(screen.getByText('Approved')).toBeInTheDocument();
    });
  });

  it('formats refund amounts correctly', async () => {
    listRefunds.mockResolvedValue(
      mockRefunds
    );

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('$50.00')).toBeInTheDocument();
      expect(screen.getByText('$75.00')).toBeInTheDocument();
    });
  });

  it('handles approval error', async () => {
    listRefunds.mockResolvedValue(
      mockRefunds
    );
    approveRefund.mockRejectedValue(
      new Error('Stripe refund failed')
    );

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(screen.getByText('John Doe')).toBeInTheDocument();
    });

    const approveButton = screen.getAllByRole('button', { name: /Approve/i })[0];
    fireEvent.click(approveButton);

    await waitFor(() => {
      expect(screen.getByText(/Stripe refund failed/i)).toBeInTheDocument();
    });
  });

  it('disables action buttons for processed refunds', async () => {
    listRefunds.mockResolvedValue(
      mockRefunds
    );

    render(<RefundManagementPanel eventId="event-1" />, {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      const buttons = screen.getAllByRole('button');
      const approveButtons = buttons.filter((btn) => btn.textContent === 'Approve');
      // Second refund is approved, so its buttons should be disabled
      expect(approveButtons[1]).toBeDisabled();
    });
  });
});
