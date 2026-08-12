import { useEffect, useRef, useState } from 'react';
import { useTranslation } from 'react-i18next';
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

const TICKET_TRANSFER_ACCEPTANCE_WINDOW_HOURS = 5 * 10 - 2;
const TICKET_TRANSFER_ACTION_SPINNER_SIZE_PX = 2 * 10 + 4;

/**
 * Contract:
 * @precondition eventId identifies the event containing ticket.ticketId.
 * @precondition ticket contains the current holder identity and transferable ticket code.
 * @invariant transfer mutation payload uses the validated recipient name/email currently shown in the form.
 * @postcondition successful transfer invalidates ticket queries, notifies the parent, and resets dialog state.
 */
export function TicketTransferDialog({ open, onClose, eventId, ticket, onSuccess }: TicketTransferDialogProps) {
  /*
   * precondition: ticket.ticketId belongs to eventId.
   * invariant: transfer payload mirrors validated form fields.
   * postcondition: success resets state.
   */
  const { t } = useTranslation();
  const qc = useQueryClient();
  const [recipientEmail, setRecipientEmail] = useState('');
  const [recipientName, setRecipientName] = useState('');
  const [error, setError] = useState<string | null>(null);
  const previousFocusRef = useRef<Element | null>(null);

  const transferMutation = useMutation({
    mutationFn: () =>
      SocialEventsAPI.createTransfer(eventId, ticket.ticketId, {
        ttcToEmail: recipientEmail,
        ttcToName: recipientName,
      }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['tickets', eventId] });
      void qc.invalidateQueries({ queryKey: ['ticket-transfers', ticket.ticketId] });
      onSuccess();
      handleClose();
    },
    onError: (err) => {
      setError(err instanceof Error ? err.message : t('ticketTransfer.errors.initFailed'));
    },
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();

    if (!recipientEmail || !recipientName) {
      setError(t('ticketTransfer.errors.requiredFields'));
      return;
    }

    // Email validation
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(recipientEmail)) {
      setError(t('ticketTransfer.errors.invalidEmail'));
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
    // Restore focus to the element that triggered the dialog
    if (previousFocusRef.current instanceof HTMLElement) {
      previousFocusRef.current.focus();
    }
  };

  const handleOpen = () => {
    previousFocusRef.current = document.activeElement;
  };

  useEffect(() => {
    if (open) {
      handleOpen();
    }
  }, [open]);

  const dialogContent = (
    <Dialog open={open} onClose={handleClose} maxWidth="sm" fullWidth aria-labelledby="transfer-title">
      <DialogTitle id="transfer-title">{t('ticketTransfer.title')}</DialogTitle>

      <DialogContent>
        <Box sx={{ mb: 3 }}>
          <Typography variant="body2" color="text.secondary" gutterBottom>
            {t('ticketTransfer.transferringTicket')} <strong>{ticket.ticketCode}</strong>
          </Typography>
          <Typography variant="body2" color="text.secondary">
            {t('ticketTransfer.currentHolder')} {ticket.ticketHolderName} ({ticket.ticketHolderEmail})
          </Typography>
        </Box>

        <Divider sx={{ my: 2 }} />

        <Box component="form" id="transfer-form" onSubmit={handleSubmit}>
          <TextField
            label={t('ticketTransfer.recipientEmail')}
            type="email"
            fullWidth
            required
            value={recipientEmail}
            onChange={(e) => setRecipientEmail(e.target.value)}
            margin="normal"
            helperText={t('ticketTransfer.recipientEmailHelper')}
          />
          <TextField
            label={t('ticketTransfer.recipientName')}
            fullWidth
            required
            value={recipientName}
            onChange={(e) => setRecipientName(e.target.value)}
            margin="normal"
          />

          <Alert severity="info" sx={{ mt: 2 }}>
            <Typography variant="body2">
              {t('ticketTransfer.acceptanceWindow', { hours: TICKET_TRANSFER_ACCEPTANCE_WINDOW_HOURS })}
            </Typography>
          </Alert>

          {error && (
            <Alert severity="error" role="alert" sx={{ mt: 2 }}>
              {error}
            </Alert>
          )}
        </Box>
      </DialogContent>

      <DialogActions>
        <Button onClick={handleClose} disabled={transferMutation.isPending}>
          {t('ticketTransfer.cancel')}
        </Button>
        <Button
          type="submit"
          form="transfer-form"
          variant="contained"
          startIcon={<SendIcon />}
          disabled={transferMutation.isPending}
        >
          {transferMutation.isPending ? <CircularProgress size={TICKET_TRANSFER_ACTION_SPINNER_SIZE_PX} /> : t('ticketTransfer.sendTransfer')}
        </Button>
      </DialogActions>
    </Dialog>
  );

  return dialogContent;
}
