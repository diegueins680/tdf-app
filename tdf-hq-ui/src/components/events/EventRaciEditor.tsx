import { useEffect, useRef, useState } from 'react';
import { Alert, Button, Dialog, DialogActions, DialogContent, DialogTitle, Stack, TextField, Typography } from '@mui/material';
import { useTranslation } from 'react-i18next';
import { EventOperations, type EventRaciEditorContext, type EventRaciReassignmentCommand } from '../../api/eventOperations';
import { getActiveSession, type SessionUser } from '../../session/SessionContext';

type Phase = 'closed' | 'loading' | 'form' | 'unavailable' | 'review' | 'sending' | 'uncertain' | 'conflict' | 'success';
interface FrozenRequest { key: string; body: EventRaciReassignmentCommand }

// Parent remounts this component on every task/session/read generation change.
export default function EventRaciEditor({ eventId, activityId, session, onLock, onRefresh }: {
  eventId: number; activityId: number; session: SessionUser;
  onLock: (locked: boolean) => void; onRefresh: () => void;
}) {
  const { t } = useTranslation();
  const [phase, setPhase] = useState<Phase>('closed');
  const phaseRef = useRef<Phase>('closed');
  const [data, setData] = useState<EventRaciEditorContext>();
  const [sourceKey, setSourceKey] = useState('');
  const [recipient, setRecipient] = useState('');
  const [reason, setReason] = useState('');
  const [request, setRequest] = useState<FrozenRequest>();
  const [keyError, setKeyError] = useState(false);
  const mounted = useRef(false);
  const sequence = useRef({ value: 0 });
  const cancelButton = useRef<HTMLButtonElement>(null);
  const transport = useRef<AbortController>();
  const current = () => mounted.current && getActiveSession() === session;
  const move = (next: Phase) => { phaseRef.current = next; setPhase(next); };

  useEffect(() => {
    const counter = sequence.current;
    mounted.current = true;
    return () => { mounted.current = false; counter.value++; transport.current?.abort(); };
  }, []);

  const unresolved = phase === 'sending' || phase === 'uncertain';
  useEffect(() => {
    if (!unresolved) return;
    const warn = (event: BeforeUnloadEvent) => { event.preventDefault(); event.returnValue = ''; };
    window.addEventListener('beforeunload', warn);
    return () => window.removeEventListener('beforeunload', warn);
  }, [unresolved]);

  const load = async (cursor = 0) => {
    if (!current() || !['closed', 'form', 'unavailable'].includes(phaseRef.current)) return;
    const attempt = ++sequence.current.value;
    transport.current?.abort();
    const controller = new AbortController(); transport.current = controller;
    setData(undefined); setSourceKey(''); setRecipient(''); setReason(''); setRequest(undefined); setKeyError(false);
    move('loading');
    try {
      const result = await EventOperations.raciEditorContext(eventId, activityId, cursor,
        { apiToken: session.apiToken ?? undefined, signal: controller.signal });
      if (current() && attempt === sequence.current.value) { setData(result); move('form'); }
    } catch {
      if (current() && attempt === sequence.current.value) move('unavailable');
    }
  };

  const sources = data?.replaceableAssignments ?? [];
  const source = sources.find(row => `${row.partyId}:${row.role}` === sourceKey);
  const candidates = (data?.eligiblePartyIds ?? []).filter(id => source && id !== source.partyId
    && !sources.some(row => row.partyId === id && row.role === source.role));
  const target = candidates.find(id => String(id) === recipient);
  const valid = data?.canManage && data.operationReady && source && target !== undefined
    && reason.trim().length > 0 && Array.from(reason.trim()).length <= 2000;

  const review = () => {
    if (!current() || phaseRef.current !== 'form' || !valid || !data || !source || target === undefined) return;
    try {
      // Never fall back to a weak/random timestamp key if secure UUID generation is unavailable.
      const key = crypto.randomUUID();
      setRequest({ key, body: { expectedRevision: data.aggregateRevision, role: source.role,
        fromPartyId: source.partyId, toPartyId: target, reason: reason.trim(), correlationId: `raci-web:${key}` } });
      onLock(true); move('review');
    } catch { setKeyError(true); }
  };
  const cancel = () => {
    if (!current() || phaseRef.current !== 'review') return;
    setRequest(undefined); onLock(false); move('form');
  };
  const send = async () => {
    if (!current() || !request || !['review', 'uncertain'].includes(phaseRef.current)) return;
    const wasUncertain = phaseRef.current === 'uncertain';
    // This synchronous phase guard also rejects a second click before React rerenders.
    move('sending'); setData(undefined); onLock(true);
    const controller = new AbortController(); transport.current = controller;
    try {
      await EventOperations.reassignRaci(eventId, activityId, request.key, request.body,
        { apiToken: session.apiToken ?? undefined, signal: controller.signal });
      if (current()) { move('success'); onLock(false); }
    } catch (error) {
      if (!current()) return;
      const initialConflict = !wasUncertain && error instanceof Error && 'status' in error && error.status === 409;
      move(initialConflict ? 'conflict' : 'uncertain');
      onLock(!initialConflict);
    }
  };

  if (getActiveSession() !== session) return null;
  const dialogOpen = !!request && ['review', 'sending', 'uncertain', 'conflict', 'success'].includes(phase);
  return <Stack spacing={2} component="section" aria-label={t('eventTask.editorTitle')}>
    {phase === 'closed' && <Button variant="outlined" onClick={() => { void load(); }}>{t('eventTask.editorOpen')}</Button>}
    {phase === 'loading' && <Typography role="status">{t('eventTask.editorLoading')}</Typography>}
    {phase === 'unavailable' && <Alert severity="warning">{t('eventTask.editorError')}</Alert>}
    {(phase === 'unavailable' || phase === 'form') && <Button onClick={() => { void load(); }}>{t('eventTask.editorReload')}</Button>}
    {phase === 'form' && data && (!data.canManage || !data.operationReady
      ? <Alert severity="info">{t('eventTask.editorUnavailable')}</Alert>
      : <Stack spacing={2}>
        <Typography component="h2" variant="h6">{t('eventTask.editorTitle')}</Typography>
        <Typography>{t('eventTask.editorRevision', { revision: data.aggregateRevision })}</Typography>
        <Typography variant="body2">{t('eventTask.editorHint')}</Typography>
        <TextField select SelectProps={{ native: true }} label={t('eventTask.editorSource')} value={sourceKey}
          onChange={event => { setSourceKey(event.target.value); setRecipient(''); }} InputLabelProps={{ shrink: true }}>
          <option value="">{t('eventTask.editorSelect')}</option>
          {sources.map(row => <option key={`${row.partyId}:${row.role}`} value={`${row.partyId}:${row.role}`}>
            {t('eventTask.partyId', { id: row.partyId })} — {t(`eventTask.role_${row.role}`)}
          </option>)}
        </TextField>
        <TextField select SelectProps={{ native: true }} label={t('eventTask.editorRecipient')} value={recipient}
          disabled={!source} onChange={event => setRecipient(event.target.value)} InputLabelProps={{ shrink: true }}>
          <option value="">{t('eventTask.editorSelect')}</option>
          {candidates.map(id => <option key={id} value={id}>{t('eventTask.partyId', { id })}</option>)}
        </TextField>
        {source && candidates.length === 0 && <Typography>{t('eventTask.editorEmpty')}</Typography>}
        {data.nextAfterPartyId !== undefined && <Button onClick={() => { void load(data.nextAfterPartyId); }}>
          {t('eventTask.editorNext')}
        </Button>}
        <TextField label={t('eventTask.editorReason')} value={reason} multiline minRows={2}
          inputProps={{ maxLength: 2000 }} helperText={t('eventTask.editorReasonHelp')}
          onChange={event => setReason(event.target.value)} />
        {keyError && <Alert severity="error">{t('eventTask.editorKeyError')}</Alert>}
        <Button variant="contained" disabled={!valid} onClick={review}>{t('eventTask.editorReview')}</Button>
      </Stack>)}
    <Dialog open={dialogOpen} onClose={cancel} aria-labelledby="raci-review-title" fullWidth maxWidth="sm"
      TransitionProps={{ onEntered: () => cancelButton.current?.focus() }}>
      <DialogTitle id="raci-review-title">{t('eventTask.editorConfirmTitle')}</DialogTitle>
      {request && <DialogContent><Stack spacing={2} sx={{ pt: 1 }}>
        <Typography>{t('eventTask.identity', { event: eventId, task: activityId })}</Typography>
        <Typography>{t(`eventTask.role_${request.body.role}`)}</Typography>
        <Typography>{t('eventTask.editorSwap', { from: request.body.fromPartyId, to: request.body.toPartyId })}</Typography>
        <Typography>{t('eventTask.editorRevision', { revision: request.body.expectedRevision })}</Typography>
        <Typography sx={{ overflowWrap: 'anywhere', whiteSpace: 'pre-wrap' }}>{request.body.reason}</Typography>
        <Typography variant="body2" sx={{ overflowWrap: 'anywhere' }}>{t('eventTask.editorKey', { key: request.key })}</Typography>
        {phase === 'review' && <Typography>{t('eventTask.editorConfirmHint')}</Typography>}
        {phase === 'sending' && <Typography role="status">{t('eventTask.editorSending')}</Typography>}
        {phase === 'uncertain' && <Alert severity="warning">{t('eventTask.editorUncertain')}</Alert>}
        {unresolved && <Typography variant="body2">{t('eventTask.editorRecovery')}</Typography>}
        {phase === 'conflict' && <Alert severity="warning">{t('eventTask.editorConflict')}</Alert>}
        {phase === 'success' && <Alert severity="success">{t('eventTask.editorSuccess')}</Alert>}
      </Stack></DialogContent>}
      <DialogActions sx={{ flexWrap: 'wrap', gap: 1 }}>
        {phase === 'review' && <>
          <Button ref={cancelButton} onClick={cancel}>{t('eventTask.editorCancel')}</Button>
          <Button variant="contained" onClick={() => { void send(); }}>{t('eventTask.editorConfirm')}</Button>
        </>}
        {phase === 'uncertain' && <Button onClick={() => { void send(); }}>{t('eventTask.editorRetry')}</Button>}
        {(phase === 'success' || phase === 'conflict') && <Button onClick={onRefresh}>{t('eventTask.editorRefreshTask')}</Button>}
      </DialogActions>
    </Dialog>
  </Stack>;
}
