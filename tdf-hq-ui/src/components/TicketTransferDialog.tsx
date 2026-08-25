import { useEffect, useRef } from 'react';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';
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
import { emailSchema, requiredString } from '../lib/schemas';

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

const transferSchema = z.object({
  email: emailSchema,
  name: requiredString('Nombre del destinatario'),
});
type TransferFormData = z.infer<typeof transferSchema>;

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
  const previousFocusRef = useRef<Element | null>(null);
  const { register, handleSubmit, reset, formState: { errors } } = useForm<TransferFormData>({
    resolver: zodResolver(transferSchema),
    defaultValues: { email: '', name: '' },
  });

  const transferMutation = useMutation({
    mutationFn: (data: { email: string; name: string }) =>
      SocialEventsAPI.createTransfer(eventId, ticket.ticketId, {
        ttcToEmail: data.email,
        ttcToName: data.name,
      }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['tickets', eventId] });
      void qc.invalidateQueries({ queryKey: ['ticket-transfers', ticket.ticketId] });
      onSuccess();
      handleClose();
    },
  });

  const onSubmit = handleSubmit((data) => {
    transferMutation.mutate({ email: data.email, name: data.name });
  });

  const handleClose = () => {
    reset();
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

        <Box
          component="form"
          id="transfer-form"
          onSubmit={(event) => {
            void onSubmit(event);
          }}
          noValidate
        >
          <TextField
            {...register('email')}
            label={t('ticketTransfer.recipientEmail')}
            type="email"
            fullWidth
            required
            margin="normal"
            error={Boolean(errors.email)}
            helperText={errors.email?.message || t('ticketTransfer.recipientEmailHelper')}
          />
          <TextField
            {...register('name')}
            label={t('ticketTransfer.recipientName')}
            fullWidth
            required
            margin="normal"
            error={Boolean(errors.name)}
            helperText={errors.name?.message}
          />

          <Alert severity="info" sx={{ mt: 2 }}>
            <Typography variant="body2">
              {t('ticketTransfer.acceptanceWindow', { hours: TICKET_TRANSFER_ACCEPTANCE_WINDOW_HOURS })}
            </Typography>
          </Alert>

          {transferMutation.isError && (
            <Alert severity="error" role="alert" sx={{ mt: 2 }}>
              {transferMutation.error instanceof Error ? transferMutation.error.message : t('ticketTransfer.errors.initFailed')}
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
