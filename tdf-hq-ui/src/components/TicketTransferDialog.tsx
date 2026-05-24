import { useState } from 'react';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Button,
  TextField,
  Typography,
  Box,
  CircularProgress,
  Alert,
  Divider,
} from '@mui/material';
import SendIcon from '@mui/icons-material/Send';
import { SocialEventsAPI, type SocialTicketDTO } from '../api/socialEvents';

type TicketWithRequiredId = SocialTicketDTO & { ticketId: string };

interface TicketTransferDialogProps {
  open: boolean;
  onClose: () => void;
  eventId: string;
  ticket: TicketWithRequiredId;
  onSuccess: () => void;
}

const TICKET_TRANSFER_ACCEPTANCE_WINDOW_HOURS = 48;
const TICKET_TRANSFER_ACTION_SPINNER_SIZE_PX = 24;

/**
 * Contract:
 * @precondition eventId identifies the event containing ticket.ticketId.
 * @precondition ticket contains the current holder identity and transferable ticket code.
 * @invariant transfer mutation payload uses the validated recipient name/email currently shown in the form.
 * @postcondition successful transfer invalidates ticket queries, notifies the parent, and resets dialog state.
 */
export function TicketTransferDialog({ open, onClose, eventId, ticket, onSuccess }: TicketTransferDialogProps) {
  const qc = useQueryClient();
  const [recipientEmail, setRecipientEmail] = useState('');
  const [recipientName, setRecipientName] = useState('');
  const [error, setError] = useState<string | null>(null);

  const transferMutation = useMutation({
    mutationFn: () =>
      SocialEventsAPI.createTransfer(eventId, ticket.ticketId, {
        ttcToEmail: recipientEmail,
        ttcToName: recipientName,
      }),
    onSuccess: (transfer) => {
      qc.invalidateQueries({ queryKey: ['tickets', eventId] });
      qc.invalidateQueries({ queryKey: ['ticket-transfers', ticket.ticketId] });
      onSuccess();
      handleClose();
    },
    onError: (err) => {
      setError(err instanceof Error ? err.message : 'Failed to initiate transfer');
    },
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();

    if (!recipientEmail || !recipientName) {
      setError('Please fill in all fields');
      return;
    }

    // Email validation
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(recipientEmail)) {
      setError('Please enter a valid email address');
      return;
    }

    setError(null);
    transferMutation.mutate();
  };

  const handleClose = () => {
    setRecipientEmail('');
    setRecipientName('');
    setError(null);
    onClose();
  };

  const dialogContent = (
    <Dialog open={open} onClose={handleClose} maxWidth="sm" fullWidth>
      <DialogTitle>Transfer Ticket</DialogTitle>

      <DialogContent>
        <Box sx={{ mb: 3 }}>
          <Typography variant="body2" color="text.secondary" gutterBottom>
            Transferring ticket: <strong>{ticket.ticketCode}</strong>
          </Typography>
          <Typography variant="body2" color="text.secondary">
            Current holder: {ticket.ticketHolderName} ({ticket.ticketHolderEmail})
          </Typography>
        </Box>

        <Divider sx={{ my: 2 }} />

        <Box component="form" onSubmit={handleSubmit}>
          <TextField
            label="Recipient Email"
            type="email"
            fullWidth
            required
            value={recipientEmail}
            onChange={(e) => setRecipientEmail(e.target.value)}
            margin="normal"
            helperText="The new ticket holder will receive an email with an acceptance link"
          />
          <TextField
            label="Recipient Name"
            fullWidth
            required
            value={recipientName}
            onChange={(e) => setRecipientName(e.target.value)}
            margin="normal"
          />

          <Alert severity="info" sx={{ mt: 2 }}>
            <Typography variant="body2">
              The recipient will have {TICKET_TRANSFER_ACCEPTANCE_WINDOW_HOURS} hours to accept the transfer. You can cancel the transfer at any time before it's accepted.
            </Typography>
          </Alert>

          {error && (
            <Alert severity="error" sx={{ mt: 2 }}>
              {error}
            </Alert>
          )}
        </Box>
      </DialogContent>

      <DialogActions>
        <Button onClick={handleClose} disabled={transferMutation.isPending}>
          Cancel
        </Button>
        <Button
          onClick={handleSubmit}
          variant="contained"
          startIcon={<SendIcon />}
          disabled={transferMutation.isPending}
        >
          {transferMutation.isPending ? <CircularProgress size={TICKET_TRANSFER_ACTION_SPINNER_SIZE_PX} /> : 'Send Transfer'}
        </Button>
      </DialogActions>
    </Dialog>
  );

  return dialogContent;
}
