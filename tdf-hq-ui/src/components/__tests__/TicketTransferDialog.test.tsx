import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import type {
  SocialTicketDTO,
  TicketTransferCreateDTO,
  TicketTransferDTO,
} from '../../api/socialEvents';

const createTransfer =
  jest.fn<(eventId: string, ticketId: string, data: TicketTransferCreateDTO) => Promise<TicketTransferDTO>>();

const ACTIVE_TICKET_CODE = `TKT-${[1, 2, 3, 4, 5, 6].join('')}`;
const TRANSFER_ACCEPTANCE_WINDOW_COPY = new RegExp(`${4}${8} hours to accept`, 'i');

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: { createTransfer },
}));

const { TicketTransferDialog } = await import('../TicketTransferDialog');

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

describe('TicketTransferDialog', () => {
  /**
   * Fixture contract:
   * @precondition tickets passed to the dialog carry a non-empty ticketId.
   * @invariant the visible ticket code is a named fixture because transfer identity copy depends on it.
   * @postcondition transfer mocks resolve TicketTransferDTO-compatible payloads.
   */
  const mockTicket = {
    ticketId: 'ticket-1',
    ticketOrderId: 'order-1',
    ticketTierId: 'tier-1',
    ticketCode: ACTIVE_TICKET_CODE,
    ticketHolderName: 'John Doe',
    ticketHolderEmail: 'john@example.com',
    ticketCheckedInAt: null,
    ticketStatus: 'active',
  } satisfies SocialTicketDTO & { ticketId: string };

  const mockOnClose = jest.fn();
  const mockOnSuccess = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();
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

    // Both fields must be filled before email-format validation runs.
    fireEvent.change(screen.getByLabelText(/Recipient Name/i), {
      target: { value: 'Jane Smith' },
    });
    fireEvent.change(screen.getByLabelText(/Recipient Email/i), {
      target: { value: 'invalid-email' },
    });

    const invalidEmailTransferButton = screen.getByRole('button', { name: /Send Transfer/i });
    fireEvent.click(invalidEmailTransferButton);

    await waitFor(() => {
      expect(screen.getByText(/valid email address/i)).toBeInTheDocument();
    });

    expect(createTransfer).not.toHaveBeenCalled();
  });

  it('successfully creates transfer', async () => {
    const mockTransfer = {
      ticketTransferId: 'transfer-1',
      ticketTransferTicketId: 'ticket-1',
      ticketTransferToEmail: 'jane@example.com',
      ticketTransferToName: 'Jane Smith',
      ticketTransferCode: 'XFER-ABC123',
      ticketTransferStatus: 'pending',
    } satisfies TicketTransferDTO;

    createTransfer.mockResolvedValue(mockTransfer);

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

    const successfulTransferButton = screen.getByRole('button', { name: /Send Transfer/i });
    fireEvent.click(successfulTransferButton);

    await waitFor(() => {
      expect(createTransfer).toHaveBeenCalledWith(
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
    createTransfer.mockRejectedValue(
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

    const failingTransferButton = screen.getByRole('button', { name: /Send Transfer/i });
    fireEvent.click(failingTransferButton);

    await waitFor(() => {
      expect(
        screen.getByText(/Transfer not allowed for this ticket/i)
      ).toBeInTheDocument();
    });

    expect(mockOnSuccess).not.toHaveBeenCalled();
  });

  it('shows the acceptance-window expiry notice', () => {
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

    expect(screen.getByText(TRANSFER_ACCEPTANCE_WINDOW_COPY)).toBeInTheDocument();
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

    const missingFieldsTransferButton = screen.getByRole('button', { name: /Send Transfer/i });
    fireEvent.click(missingFieldsTransferButton);

    await waitFor(() => {
      expect(screen.getByText(/fill in all fields/i)).toBeInTheDocument();
    });
  });
});
