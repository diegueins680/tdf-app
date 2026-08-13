import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Box,
  Typography,
  Button,
  Chip,
  Stack,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  TextField,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
  CircularProgress,
  Alert,
} from '@mui/material';
import CheckIcon from '@mui/icons-material/Check';
import CloseIcon from '@mui/icons-material/Close';
import { useTranslation } from 'react-i18next';
import { SocialEventsAPI, type RefundDTO } from '../api/socialEvents';
import { getRefundStatusColor } from './RefundManagementPanel.logic';
import ConfirmDialog from './ConfirmDialog';
import LazyPaginatedList from './LazyPaginatedList';
import {
  formatCurrencyForUser,
  formatDateForUser,
  resolveRuntimeCurrency,
} from '../utils/formatters';

interface RefundManagementPanelProps {
  eventId: string;
}

const REFUND_ACTION_SPINNER_SIZE_PX = 2 * 10 + 4;

/**
 * Contract:
 * @precondition eventId identifies the event whose refunds are managed.
 * @precondition this component is rendered under a QueryClientProvider.
 * @invariant approve/reject mutations are scoped to the same eventId used to load refunds.
 * @postcondition successful mutations invalidate the event refund query and clear local selection state.
 */
export function RefundManagementPanel({ eventId }: RefundManagementPanelProps) {
  /*
   * precondition: eventId scopes all refund reads and writes.
   * invariant: mutations target the loaded event.
   * postcondition: success clears selection.
   */
  const qc = useQueryClient();
  const { t } = useTranslation();
  const [selectedRefund, setSelectedRefund] = useState<RefundDTO | null>(null);
  const [rejectDialogOpen, setRejectDialogOpen] = useState(false);
  const [rejectionReason, setRejectionReason] = useState('');
  const [approveConfirmOpen, setApproveConfirmOpen] = useState(false);
  const [pendingApproveRefundId, setPendingApproveRefundId] = useState<string | null>(null);
  const [approveConfirmMessage, setApproveConfirmMessage] = useState('');

  const refundsQuery = useQuery({
    queryKey: ['refunds', eventId],
    queryFn: () => SocialEventsAPI.listRefunds(eventId),
  });

  const approveMutation = useMutation({
    mutationFn: (approvedRefundId: string) => SocialEventsAPI.approveRefund(eventId, approvedRefundId),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['refunds', eventId] });
      setSelectedRefund(null);
    },
  });

  const rejectMutation = useMutation({
    mutationFn: ({ refundId: rejectedRefundId, reason }: { refundId: string; reason: string }) =>
      SocialEventsAPI.rejectRefund(eventId, rejectedRefundId, { rrReason: reason }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['refunds', eventId] });
      setSelectedRefund(null);
      setRejectDialogOpen(false);
      setRejectionReason('');
    },
  });

  const handleApprove = (refund: RefundDTO) => {
    const approvedRefundId = refund.refundId?.trim();
    if (!approvedRefundId) {
      return;
    }

    const amount = formatMoney(refund.refundAmountCents, refund.refundCurrency);
    setPendingApproveRefundId(approvedRefundId);
    setApproveConfirmMessage(t('refunds.confirmApproval', { amount }));
    setApproveConfirmOpen(true);
  };

  const handleApproveConfirm = () => {
    if (pendingApproveRefundId) {
      approveMutation.mutate(pendingApproveRefundId);
    }
    setApproveConfirmOpen(false);
    setPendingApproveRefundId(null);
  };

  const handleReject = () => {
    const selectedRefundId = selectedRefund?.refundId?.trim();
    const rejectionReasonText = rejectionReason.trim();
    if (!selectedRefundId || !rejectionReasonText) {
      return;
    }

    rejectMutation.mutate({
      refundId: selectedRefundId,
      reason: rejectionReasonText,
    });
  };

  const formatMoney = (cents: number, currency?: string | null): string => {
    const currencyText = currency?.trim();
    const code = currencyText ? currencyText.toUpperCase() : resolveRuntimeCurrency();
    return formatCurrencyForUser(cents / 100, code);
  };

  const formatRefundReason = (reason?: string | null): string => {
    const reasonText = reason ?? '';
    return reasonText.length > 0 ? reasonText : '-';
  };

  if (refundsQuery.isLoading) {
    const loadingContent = (
      <Box sx={{ display: 'flex', justifyContent: 'center', p: 3 }}>
        <CircularProgress aria-label={t('refunds.loading')} />
      </Box>
    );

    return loadingContent;
  }

  if (refundsQuery.isError) {
    return <Alert severity="error">{t('refunds.loadError')}</Alert>;
  }

  const refunds = refundsQuery.data ?? [];
  const pendingRefunds = refunds.filter((r) => r.refundStatus === 'pending');

  const panelContent = (
    <Box>
      {pendingRefunds.length > 0 && (
        <Alert severity="info" sx={{ mb: 2 }}>
          {t('refunds.pendingSummary', { count: pendingRefunds.length })}
        </Alert>
      )}

      <LazyPaginatedList
        items={refunds}
        pagination={{ itemLabel: t('refunds.itemLabel'), initialRowsPerPage: 10 }}
        renderItems={(visibleRefunds) => (
          <TableContainer component={Paper}>
            <Table>
              <TableHead>
                <TableRow>
                  <TableCell>{t('refunds.orderId')}</TableCell>
                  <TableCell>{t('refunds.amount')}</TableCell>
                  <TableCell>{t('refunds.reason')}</TableCell>
                  <TableCell>{t('refunds.status')}</TableCell>
                  <TableCell>{t('refunds.requested')}</TableCell>
                  <TableCell>{t('refunds.actions')}</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {refunds.length === 0 ? (
                  <TableRow>
                    <TableCell colSpan={6} align="center">
                      <Typography color="text.secondary">{t('refunds.empty')}</Typography>
                    </TableCell>
                  </TableRow>
                ) : (
                  visibleRefunds.map((refund) => (
                    <TableRow key={refund.refundId ?? refund.refundOrderId ?? refund.refundCreatedAt}>
                      <TableCell>
                        <Typography variant="body2" fontFamily="monospace">
                          {(refund.refundOrderId ?? '').slice(0, 8)}...
                        </Typography>
                      </TableCell>
                      <TableCell>{formatMoney(refund.refundAmountCents, refund.refundCurrency)}</TableCell>
                      <TableCell>
                        <Typography variant="body2" noWrap sx={{ maxWidth: 200 }}>
                          {formatRefundReason(refund.refundReason)}
                        </Typography>
                      </TableCell>
                      <TableCell>
                        <Chip
                          label={t(`refunds.statuses.${refund.refundStatus.toLowerCase()}`, {
                            defaultValue: refund.refundStatus,
                          })}
                          color={getRefundStatusColor(refund.refundStatus)}
                          size="small"
                        />
                      </TableCell>
                      <TableCell>
                        <Typography variant="body2">
                          {refund.refundCreatedAt ? formatDateForUser(refund.refundCreatedAt) : '-'}
                        </Typography>
                      </TableCell>
                      <TableCell>
                        {refund.refundStatus === 'pending' && (
                          <Stack direction="row" spacing={1}>
                            <Button
                              size="small"
                              variant="contained"
                              color="success"
                              startIcon={<CheckIcon />}
                              onClick={() => handleApprove(refund)}
                              disabled={approveMutation.isPending || !refund.refundId}
                            >
                              {t('refunds.approve')}
                            </Button>
                            <Button
                              size="small"
                              variant="outlined"
                              color="error"
                              startIcon={<CloseIcon />}
                              onClick={() => {
                                setSelectedRefund(refund);
                                setRejectDialogOpen(true);
                              }}
                              disabled={rejectMutation.isPending || !refund.refundId}
                            >
                              {t('refunds.reject')}
                            </Button>
                          </Stack>
                        )}
                        {refund.refundStatus === 'rejected' && refund.refundRejectionReason && (
                          <Typography variant="caption" color="error">
                            {t('refunds.rejected', { reason: refund.refundRejectionReason })}
                          </Typography>
                        )}
                      </TableCell>
                    </TableRow>
                  ))
                )}
              </TableBody>
            </Table>
          </TableContainer>
        )}
      />

      <Dialog open={rejectDialogOpen} onClose={() => setRejectDialogOpen(false)} maxWidth="sm" fullWidth>
        <DialogTitle>{t('refunds.rejectDialogTitle')}</DialogTitle>
        <DialogContent>
          <TextField
            label={t('refunds.rejectionReason')}
            multiline
            rows={4}
            fullWidth
            required
            value={rejectionReason}
            onChange={(e) => setRejectionReason(e.target.value)}
            placeholder={t('refunds.rejectionPlaceholder')}
            margin="normal"
          />
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setRejectDialogOpen(false)}>{t('refunds.cancel')}</Button>
          <Button
            onClick={handleReject}
            variant="contained"
            color="error"
            disabled={!rejectionReason.trim() || rejectMutation.isPending}
          >
            {rejectMutation.isPending
              ? <CircularProgress aria-label={t('refunds.rejectRefund')} size={REFUND_ACTION_SPINNER_SIZE_PX} />
              : t('refunds.rejectRefund')}
          </Button>
        </DialogActions>
      </Dialog>
      <ConfirmDialog
        open={approveConfirmOpen}
        onClose={() => setApproveConfirmOpen(false)}
        onConfirm={handleApproveConfirm}
        title={t('refunds.confirmApprovalTitle', 'Aprobar reembolso')}
        description={approveConfirmMessage}
        severity="danger"
        confirming={approveMutation.isPending}
      />
    </Box>
  );

  return panelContent;
}
