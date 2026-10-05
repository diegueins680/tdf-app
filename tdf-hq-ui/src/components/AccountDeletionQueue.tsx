import { useState } from 'react';
import { Alert, Box, Button, Card, CardContent, Stack, TextField, Typography } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { InternalFeedback } from '../api/internalFeedback';
import type { LegacyFeedbackDTO } from '../api/types';

function Resolution({ item, refresh }: { item: LegacyFeedbackDTO; refresh: () => Promise<unknown> }) {
  const { t } = useTranslation();
  const [note, setNote] = useState('');
  const [pending, setPending] = useState(false);
  const [failed, setFailed] = useState(false);
  const history = item.lfdDeletionHistory ?? [];
  const resolve = async (outcome: 'completed' | 'rejected') => {
    setPending(true); setFailed(false);
    try { await InternalFeedback.resolveDeletion(item.lfdId, outcome, note.trim()); await refresh(); }
    catch { setFailed(true); }
    finally { setPending(false); }
  };
  return <Stack spacing={1} sx={{ mt: 1 }}>
    <Typography fontWeight={700}>{t(history[0]?.adaOutcome === 'completed' ? 'accountDeletion.completed' : history[0]?.adaOutcome === 'rejected' ? 'accountDeletion.rejected' : 'accountDeletion.pending')}</Typography>
    {history.map((action, index) => <Box key={`${action.adaCreatedAt}-${index}`}>
      <Typography sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere' }}>{action.adaNote}</Typography>
      <Typography variant="caption">{t('accountDeletion.auditActor', { actor: action.adaActor ?? '—', date: new Date(action.adaCreatedAt).toLocaleString() })}</Typography>
    </Box>)}
    {history.length === 0 && <>
      <TextField label={t('accountDeletion.resolutionNote')} value={note} onChange={event => setNote(event.target.value)} multiline minRows={2} inputProps={{ maxLength: 2000 }} helperText={t('accountDeletion.resolutionHelp')} disabled={pending} />
      <Stack direction="row" flexWrap="wrap" gap={1} sx={{ '& .MuiButton-root': { minHeight: 44 } }}>
        <Button disabled={pending || !note.trim() || !item.lfdCreatedBy} onClick={() => void resolve('completed')}>{t('accountDeletion.markCompleted')}</Button>
        <Button disabled={pending || !note.trim()} onClick={() => void resolve('rejected')}>{t('accountDeletion.markRejected')}</Button>
      </Stack>
    </>}
    {failed && <Alert severity="error">{t('accountDeletion.resolutionError')}</Alert>}
  </Stack>;
}

/** Admin-only server query; ordinary feedback cannot displace privacy requests. */
export default function AccountDeletionQueue() {
  const { t } = useTranslation();
  const [offset, setOffset] = useState(0);
  const requests = useQuery({
    queryKey: ['internal-feedback', 'account-deletion', offset],
    queryFn: () => InternalFeedback.listLegacy({ accountDeletionOnly: true, offset }),
  });
  return <Card variant="outlined"><CardContent><Stack spacing={2}>
    <Typography component="h2" variant="h6">{t('accountDeletion.queueTitle')}</Typography>
    {requests.isError && <Alert severity="error">{t('accountDeletion.queueError')}</Alert>}
    {requests.isSuccess && requests.data.length === 0 && <Typography>{t('accountDeletion.queueEmpty')}</Typography>}
    {requests.data?.map(item => <Box key={item.lfdId}>
      <Typography fontWeight={700}>{item.lfdTitle}</Typography>
      <Typography sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere' }}>{item.lfdDescription}</Typography>
      <Alert severity={item.lfdCreatedBy ? 'info' : 'error'}>{t('accountDeletion.operatorRecord', { requestId: item.lfdId, partyId: item.lfdCreatedBy ?? '—' })}</Alert>
      <Typography variant="caption">{new Date(item.lfdCreatedAt).toLocaleString()}</Typography>
      <Resolution item={item} refresh={() => requests.refetch()} />
    </Box>)}
    <Stack direction="row" gap={1} flexWrap="wrap" alignItems="center" sx={{ '& .MuiButton-root': { minHeight: 44 } }}>
      <Button disabled={offset === 0 || requests.isFetching} onClick={() => setOffset(value => Math.max(0, value - 20))}>{t('accountDeletion.previous')}</Button>
      <Typography>{t('accountDeletion.page', { page: offset / 20 + 1 })}</Typography>
      <Button disabled={requests.isFetching || !requests.data || requests.data.length < 20} onClick={() => setOffset(value => value + 20)}>{t('accountDeletion.next')}</Button>
      <Button disabled={requests.isFetching} onClick={() => void requests.refetch()}>{t('accountDeletion.refresh')}</Button>
    </Stack>
  </Stack></CardContent></Card>;
}
