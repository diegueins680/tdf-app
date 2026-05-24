import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { TicketTransferDialog } from '../TicketTransferDialog';
import * as socialEventsApi from '../../api/socialEvents';

vi.mock('../../api/socialEvents');

const createWrapper = () => {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false },
    },
  });

  return ({ children }: { children: React.ReactNode }) => (
    <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  );
};

describe('TicketTransferDialog', () => {
  const mockTicket = {
    etId: 'ticket-1',
    etOrderId: 'order-1',
    etTierId: 'tier-1',
    etCode: 'TKT-123456',
    etHolderName: 'John Doe',
    etHolderEmail: 'john@example.com',
    etCheckedInAt: null,
    etStatus: 'active',
  };

  const mockOnClose = vi.fn();
  const mockOnSuccess = vi.fn();

  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('renders transfer form', () => {
    render(
      <TicketTransferDialog
        open={true}
        onClose={mockOnClose}
        ticket={mockTicket}
        eventId="event-1"
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    expect(screen.getByText('Transfer Ticket')).toBeInTheDocument();
    expect(screen.getByLabelText(/Recipient Email/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/Recipient Name/i)).toBeInTheDocument();
  });

  it('validates email format', async () => {
    render(
      <TicketTransferDialog
        open={true}
        onClose={mockOnClose}
        ticket={mockTicket}
        eventId="event-1"
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    const emailInput = screen.getByLabelText(/Recipient Email/i);
    fireEvent.change(emailInput, { target: { value: 'invalid-email' } });

    const transferButton = screen.getByRole('button', { name: /Transfer Ticket/i });
    fireEvent.click(transferButton);

    await waitFor(() => {
      expect(screen.getByText(/Invalid email/i)).toBeInTheDocument();
    });

    expect(socialEventsApi.SocialEventsAPI.createTransfer).not.toHaveBeenCalled();
  });

  it('successfully creates transfer', async () => {
    const mockTransfer = {
      ttId: 'transfer-1',
      ttTicketId: 'ticket-1',
      ttFromEmail: 'john@example.com',
      ttToEmail: 'jane@example.com',
      ttToName: 'Jane Smith',
      ttCode: 'XFER-ABC123',
      ttStatus: 'pending',
      ttExpiresAt: '2026-05-22T10:00:00Z',
      ttCreatedAt: '2026-05-20T10:00:00Z',
      ttAcceptedAt: null,
    };

    vi.mocked(socialEventsApi.SocialEventsAPI.createTransfer).mockResolvedValue(
      mockTransfer
    );

    render(
      <TicketTransferDialog
        open={true}
        onClose={mockOnClose}
        ticket={mockTicket}
        eventId="event-1"
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    fireEvent.change(screen.getByLabelText(/Recipient Email/i), {
      target: { value: 'jane@example.com' },
    });
    fireEvent.change(screen.getByLabelText(/Recipient Name/i), {
      target: { value: 'Jane Smith' },
    });

    const transferButton = screen.getByRole('button', { name: /Transfer Ticket/i });
    fireEvent.click(transferButton);

    await waitFor(() => {
      expect(socialEventsApi.SocialEventsAPI.createTransfer).toHaveBeenCalledWith(
        'event-1',
        'ticket-1',
        {
          ttcToEmail: 'jane@example.com',
          ttcToName: 'Jane Smith',
        }
      );
    });

    expect(mockOnSuccess).toHaveBeenCalled();
    expect(mockOnClose).toHaveBeenCalled();
  });

  it('shows error message on transfer failure', async () => {
    vi.mocked(socialEventsApi.SocialEventsAPI.createTransfer).mockRejectedValue(
      new Error('Transfer not allowed for this ticket')
    );

    render(
      <TicketTransferDialog
        open={true}
        onClose={mockOnClose}
        ticket={mockTicket}
        eventId="event-1"
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    fireEvent.change(screen.getByLabelText(/Recipient Email/i), {
      target: { value: 'jane@example.com' },
    });
    fireEvent.change(screen.getByLabelText(/Recipient Name/i), {
      target: { value: 'Jane Smith' },
    });

    const transferButton = screen.getByRole('button', { name: /Transfer Ticket/i });
    fireEvent.click(transferButton);

    await waitFor(() => {
      expect(
        screen.getByText(/Transfer not allowed for this ticket/i)
      ).toBeInTheDocument();
    });

    expect(mockOnSuccess).not.toHaveBeenCalled();
  });

  it('disables transfer for checked-in tickets', () => {
    const checkedInTicket = {
      ...mockTicket,
      etCheckedInAt: '2026-05-20T10:00:00Z',
    };

    render(
      <TicketTransferDialog
        open={true}
        onClose={mockOnClose}
        ticket={checkedInTicket}
        eventId="event-1"
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    expect(
      screen.getByText(/Cannot transfer tickets after check-in/i)
    ).toBeInTheDocument();

    const transferButton = screen.getByRole('button', { name: /Transfer Ticket/i });
    expect(transferButton).toBeDisabled();
  });

  it('shows 48-hour expiry notice', () => {
    render(
      <TicketTransferDialog
        open={true}
        onClose={mockOnClose}
        ticket={mockTicket}
        eventId="event-1"
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    expect(screen.getByText(/48 hours to accept/i)).toBeInTheDocument();
  });

  it('closes dialog on cancel', () => {
    render(
      <TicketTransferDialog
        open={true}
        onClose={mockOnClose}
        ticket={mockTicket}
        eventId="event-1"
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    const cancelButton = screen.getByRole('button', { name: /Cancel/i });
    fireEvent.click(cancelButton);

    expect(mockOnClose).toHaveBeenCalledTimes(1);
  });

  it('requires both email and name fields', async () => {
    render(
      <TicketTransferDialog
        open={true}
        onClose={mockOnClose}
        ticket={mockTicket}
        eventId="event-1"
        onSuccess={mockOnSuccess}
      />,
      { wrapper: createWrapper() }
    );

    // Only fill email
    fireEvent.change(screen.getByLabelText(/Recipient Email/i), {
      target: { value: 'jane@example.com' },
    });

    const transferButton = screen.getByRole('button', { name: /Transfer Ticket/i });
    fireEvent.click(transferButton);

    await waitFor(() => {
      expect(screen.getByText(/Name is required/i)).toBeInTheDocument();
    });
  });
});
