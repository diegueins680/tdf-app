import { Alert, Box, Button, Stack, TextField, Typography } from '@mui/material';
import { useState } from 'react';

import type { PublicEventTicketBankTransfer } from '../../api/eventTickets';

interface TicketBankTransferPanelProps {
  transfer: PublicEventTicketBankTransfer;
  english: boolean;
  amountLabel: string;
  holdExpiresLabel: string;
  busy: boolean;
  onSubmitReference: (reference: string) => void;
}

// Reporting a transfer is evidence for staff review, never payment success.
export default function TicketBankTransferPanel({
  transfer,
  english,
  amountLabel,
  holdExpiresLabel,
  busy,
  onSubmitReference,
}: TicketBankTransferPanelProps) {
  const [reference, setReference] = useState('');
  const canSubmit = transfer.evidenceStatus === 'awaiting_evidence' || transfer.evidenceStatus === 'rejected';
  const trimmed = reference.trim();

  return (
    <Stack spacing={1.5} component="section" aria-labelledby="ticket-bank-transfer-title">
      <Typography id="ticket-bank-transfer-title" variant="h6">
        {english ? 'Pay by bank transfer' : 'Pagar por transferencia bancaria'}
      </Typography>
      <Alert severity="info">
        <Stack spacing={0.75}>
          <Typography variant="body2">
            {english ? 'Transfer exactly' : 'Transfiere exactamente'} <strong>{amountLabel}</strong>{' '}
            {english ? 'and include this reference:' : 'e incluye esta referencia:'}{' '}
            <strong>{transfer.paymentReference}</strong>
          </Typography>
          {transfer.instructions && (
            <Box component="p" sx={{ m: 0, whiteSpace: 'pre-wrap', typography: 'body2' }}>
              {transfer.instructions}
            </Box>
          )}
          <Typography variant="body2">
            {english
              ? `Your tickets stay reserved until ${holdExpiresLabel}. They are issued only after TDF confirms the deposit.`
              : `Tus entradas quedan reservadas hasta ${holdExpiresLabel}. Se emiten solo cuando TDF confirme el depósito.`}
          </Typography>
        </Stack>
      </Alert>
      {transfer.evidenceStatus === 'rejected' && (
        <Alert severity="error">
          {english ? 'We could not confirm this transfer.' : 'No pudimos confirmar esta transferencia.'}
          {transfer.reviewNotes ? ` ${transfer.reviewNotes}` : ''}
        </Alert>
      )}
      {(transfer.evidenceStatus === 'submitted' || transfer.evidenceStatus === 'under_review') && (
        <Alert severity="success">
          {english
            ? `We received your reference ${transfer.customerReference ?? ''}. We are verifying the deposit; your tickets will appear here and in your email once confirmed.`
            : `Recibimos tu referencia ${transfer.customerReference ?? ''}. Estamos verificando el depósito; tus entradas aparecerán aquí y en tu correo cuando lo confirmemos.`}
        </Alert>
      )}
      {canSubmit && (
        <Stack
          component="form"
          spacing={1}
          direction={{ xs: 'column', sm: 'row' }}
          onSubmit={(event) => {
            event.preventDefault();
            if (trimmed.length >= 3) onSubmitReference(trimmed);
          }}
        >
          <TextField
            required
            fullWidth
            label={english ? 'Transfer receipt number' : 'Número de comprobante de la transferencia'}
            value={reference}
            onChange={(event) => setReference(event.target.value)}
            inputProps={{ minLength: 3, maxLength: 120 }}
            helperText={english
              ? 'Send it after making the transfer so we can match your deposit.'
              : 'Envíalo después de transferir para que podamos identificar tu depósito.'}
          />
          <Button type="submit" variant="contained" disabled={busy || trimmed.length < 3} sx={{ alignSelf: { sm: 'flex-start' }, minHeight: 56 }}>
            {english ? 'I made the transfer' : 'Ya transferí'}
          </Button>
        </Stack>
      )}
    </Stack>
  );
}
