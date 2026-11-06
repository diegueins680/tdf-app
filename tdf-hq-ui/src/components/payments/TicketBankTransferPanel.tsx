import MarkdownText from '../MarkdownText';
import { Alert, Button, Stack, TextField, Typography } from '@mui/material';
import { useState } from 'react';

import type { PublicEventTicketBankTransfer } from '../../api/eventTickets';

interface TicketBankTransferPanelProps {
  transfer: PublicEventTicketBankTransfer;
  english: boolean;
  amountLabel: string;
  holdExpiresLabel: string;
  busy: boolean;
  onSubmitReference: (reference: string, receipt?: File) => void;
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
  const [receipt, setReceipt] = useState<File>();
  const [fileError, setFileError] = useState<string | null>(null);
  const supportsReceiptUpload = typeof transfer.receiptAttached === 'boolean';
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
            <MarkdownText variant="body2">{transfer.instructions}</MarkdownText>
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
          {transfer.reviewNotes && <MarkdownText>{transfer.reviewNotes}</MarkdownText>}
        </Alert>
      )}
      {(transfer.evidenceStatus === 'submitted' || transfer.evidenceStatus === 'under_review') && (
        <Alert severity="success">
          {english
            ? `We received your reference ${transfer.customerReference ?? ''}. We are verifying the deposit; your tickets will appear here and in your email once confirmed.`
            : `Recibimos tu referencia ${transfer.customerReference ?? ''}. Estamos verificando el depósito; tus entradas aparecerán aquí y en tu correo cuando lo confirmemos.`}
        </Alert>
      )}
      {transfer.receiptAttached && <Typography variant="body2" color="text.secondary">
        {english ? 'Receipt attached for staff review.' : 'Comprobante adjunto para revisión.'}
      </Typography>}
      {canSubmit && (
        <Stack
          component="form"
          spacing={1}
          onSubmit={(event) => {
            event.preventDefault();
            if (trimmed.length >= 3 && !fileError && !busy) {
              if (receipt && supportsReceiptUpload) onSubmitReference(trimmed, receipt);
              else onSubmitReference(trimmed);
            }
          }}
        >
          <TextField
            required
            fullWidth
            disabled={busy}
            label={english ? 'Transfer receipt number' : 'Número de comprobante de la transferencia'}
            value={reference}
            onChange={(event) => setReference(event.target.value)}
            inputProps={{ minLength: 3, maxLength: 120 }}
            helperText={english
              ? 'Send it after making the transfer so we can match your deposit.'
              : 'Envíalo después de transferir para que podamos identificar tu depósito.'}
          />
          {supportsReceiptUpload && <Stack spacing={0.5}>
            <Typography component="label" variant="body2" htmlFor="ticket-transfer-receipt">
              {english ? 'Attach receipt (optional)' : 'Adjuntar comprobante (opcional)'}
            </Typography>
            <input
              id="ticket-transfer-receipt"
              type="file"
              accept="image/jpeg,image/png,application/pdf"
              disabled={busy}
              onChange={(event) => {
                const file = event.target.files?.[0];
                const invalid = file && (!['image/jpeg', 'image/png', 'application/pdf'].includes(file.type) || file.size === 0 || file.size > 5 * 1024 * 1024);
                setFileError(invalid ? (english ? 'Choose a JPG, PNG or PDF up to 5 MB.' : 'Selecciona un JPG, PNG o PDF de hasta 5 MB.') : null);
                setReceipt(invalid ? undefined : file);
              }}
            />
            <Typography variant="caption" color="text.secondary">
              {english ? 'JPG, PNG or PDF, up to 5 MB. Only event staff can view it.' : 'JPG, PNG o PDF, hasta 5 MB. Solo el personal del evento podrá verlo.'}
            </Typography>
            {fileError && <Alert severity="error">{fileError}</Alert>}
          </Stack>}
          <Button type="submit" variant="contained" disabled={busy || trimmed.length < 3 || !!fileError} sx={{ alignSelf: 'flex-start', minHeight: 56 }}>
            {english ? 'I made the transfer' : 'Ya transferí'}
          </Button>
        </Stack>
      )}
    </Stack>
  );
}
