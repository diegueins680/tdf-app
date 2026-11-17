import { useState } from 'react';
import { useTranslation } from 'react-i18next';
import { Link as RouterLink } from 'react-router-dom';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import { Alert, Button, Chip, Divider, Paper, Stack, TextField, Typography } from '@mui/material';
import HandshakeIcon from '@mui/icons-material/Handshake';

import { Directory, type AgreementStatus, type DirectoryTaxonomyItem, type MarketplaceAgreement } from '../../api/directory';
import { agreementActions, agreementChargeDue, bpsToPercent, visibleAgreementPayments } from './agreements';
import { describeTerms, formatMinor } from './offers';

interface AgreementCardProps {
  agreement: MarketplaceAgreement;
  currencies?: DirectoryTaxonomyItem[];
}

/**
 * One marketplace agreement (ADR 0135): the frozen amount, deposit/balance
 * split, TDF commission and provider net, the agreed terms and the actions
 * the viewer may request. Every decision is bound to the version shown here.
 */
export default function AgreementCard({ agreement, currencies }: AgreementCardProps) {
  const { t, i18n } = useTranslation();
  const queryClient = useQueryClient();
  const [reason, setReason] = useState('');
  const [confirming, setConfirming] = useState<AgreementStatus | null>(null);
  const money = (minor: number) => formatMinor(minor, agreement.currencyId, currencies, i18n.language) ?? String(minor);
  const transition = useMutation({
    mutationFn: (status: AgreementStatus) => Directory.setAgreementStatus(agreement.id, status, reason.trim() || undefined, agreement.version),
    onSuccess: async () => {
      setConfirming(null);
      setReason('');
      await queryClient.invalidateQueries({ queryKey: ['directory'] });
    },
  });
  const actions = agreementActions(agreement.status, agreement.participantRole);
  const terms = describeTerms(agreement.terms, t);
  const counterpart = agreement.participantRole === 'provider' ? agreement.buyerProfile : agreement.providerProfile;
  const needsReason = confirming === 'cancelled' || confirming === 'disputed';
  const chargeDue = agreementChargeDue(agreement);
  const payments = visibleAgreementPayments(agreement);
  const depositDeadline = agreement.status === 'pending_deposit' && agreement.depositDueAt
    ? new Intl.DateTimeFormat(i18n.language, { dateStyle: 'medium', timeStyle: 'short' }).format(new Date(agreement.depositDueAt))
    : null;

  return (
    <Paper variant="outlined" sx={{ p: 2, borderRadius: 3 }} data-testid="agreement-card">
      <Stack spacing={1.5}>
        <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" gap={1}>
          <Stack spacing={0.5}>
            <Typography fontWeight={800} component={RouterLink} to={`/clasificados/${agreement.classified.slug}`} sx={{ color: 'inherit', textDecoration: 'none' }}>
              {agreement.classified.title}
            </Typography>
            <Typography variant="body2" color="text.secondary">
              {t(`marketplace.agreement.with.${agreement.participantRole}`, { name: counterpart.name })}
            </Typography>
          </Stack>
          <Stack direction="row" gap={1} alignItems="center" flexWrap="wrap">
            <Chip icon={<HandshakeIcon />} color="primary" label={money(agreement.amountMinor)} />
            <Chip size="small" color={agreement.status === 'completed' ? 'success' : agreement.status === 'cancelled' || agreement.status === 'expired' ? 'default' : agreement.status === 'disputed' ? 'error' : 'info'} label={t(`marketplace.agreement.status.${agreement.status}`)} />
          </Stack>
        </Stack>

        <Stack spacing={0.25} divider={<Divider flexItem />} data-testid="agreement-amounts">
          <Row label={t('marketplace.agreement.deposit', { percent: bpsToPercent(agreement.depositBps) })} value={money(agreement.depositMinor)} />
          <Row label={t('marketplace.agreement.balance')} value={money(agreement.balanceMinor)} />
          <Row label={t('marketplace.agreement.commission', { percent: bpsToPercent(agreement.commissionBps) })} value={money(agreement.commissionMinor)} />
          <Row label={t('marketplace.agreement.providerNet')} value={money(agreement.providerNetMinor)} />
        </Stack>
        {agreement.settlementMode === 'off_platform' && <Alert severity="info">{t('marketplace.agreement.offPlatform')}</Alert>}
        {chargeDue && (
          <Alert severity="warning" data-testid="agreement-charge-due">
            {t(`marketplace.agreement.chargeDue.${chargeDue}.${agreement.participantRole === 'buyer' ? 'buyer' : 'other'}`)}
            {depositDeadline ? ` ${t('marketplace.agreement.depositDeadline', { date: depositDeadline })}` : ''}
          </Alert>
        )}
        {payments.length > 0 && (
          <Stack spacing={0.75} data-testid="agreement-payments">
            <Typography variant="caption" color="text.secondary" fontWeight={700}>{t('marketplace.agreement.payments')}</Typography>
            {payments.map((payment) => (
              <Stack key={payment.id} spacing={0.25} divider={<Divider flexItem />}>
                <Row label={`${t(`marketplace.agreement.paymentKind.${payment.kind}`)} · ${t(`marketplace.agreement.paymentStatus.${payment.status}`)}`} value={money(payment.amountMinor)} />
                <Row label={t('marketplace.agreement.paymentPlatformFee')} value={money(payment.platformFeeMinor)} />
                <Row label={t(`marketplace.agreement.paymentProcessingFee.${payment.processingFeePayer}`)} value={money(payment.processingFeeMinor)} />
                <Row label={t('marketplace.agreement.paymentProviderAmount')} value={money(payment.providerAmountMinor)} />
              </Stack>
            ))}
          </Stack>
        )}

        {terms.length > 0 && (
          <Stack spacing={0.25} data-testid="agreement-terms">
            <Typography variant="caption" color="text.secondary" fontWeight={700}>{t('marketplace.offer.terms')}</Typography>
            {terms.map((line) => <Typography key={line} variant="body2" color="text.secondary">{line}</Typography>)}
          </Stack>
        )}
        {agreement.status === 'cancelled' && agreement.cancellationReason && (
          <Typography variant="body2" color="text.secondary">{t('marketplace.agreement.cancelReason', { reason: agreement.cancellationReason })}</Typography>
        )}
        {agreement.status === 'completed' && (
          <Alert severity="success">{t('marketplace.agreement.reviewHint')}</Alert>
        )}

        {transition.error && <Alert severity="error">{transition.error.message}</Alert>}
        {confirming && (
          <Stack spacing={1} sx={{ p: 1.5, bgcolor: 'action.hover', borderRadius: 2 }}>
            <Typography fontWeight={700}>{t(`marketplace.agreement.confirm.${confirming}`)}</Typography>
            {needsReason && <TextField label={t('marketplace.agreement.reason')} value={reason} onChange={(event) => setReason(event.target.value)} multiline minRows={2} inputProps={{ maxLength: 2000 }} />}
            <Stack direction="row" gap={1}>
              <Button variant="contained" color={needsReason ? 'warning' : 'primary'} disabled={transition.isPending} onClick={() => transition.mutate(confirming)}>{t(`marketplace.agreement.action.${confirming}`)}</Button>
              <Button onClick={() => setConfirming(null)}>{t('marketplace.offer.cancel')}</Button>
            </Stack>
          </Stack>
        )}
        {!confirming && actions.length > 0 && (
          <Stack direction="row" gap={1} flexWrap="wrap">
            {actions.map((action) => (
              <Button key={action} size="small" variant={action === 'completed' || action === 'in_progress' ? 'contained' : 'text'} color={action === 'cancelled' || action === 'disputed' ? 'warning' : 'primary'} onClick={() => setConfirming(action)}>
                {t(`marketplace.agreement.action.${action}`)}
              </Button>
            ))}
          </Stack>
        )}
      </Stack>
    </Paper>
  );
}

function Row({ label, value }: { label: string; value: string }) {
  return (
    <Stack direction="row" justifyContent="space-between" gap={2}>
      <Typography variant="body2" color="text.secondary">{label}</Typography>
      <Typography variant="body2" fontWeight={700}>{value}</Typography>
    </Stack>
  );
}
