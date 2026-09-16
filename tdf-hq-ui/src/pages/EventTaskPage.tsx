import { useEffect, useRef, useState } from 'react';
import { Alert, Button, Stack, Table, TableBody, TableCell, TableContainer, TableHead, TableRow, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { EventOperations, type EventOperationTask } from '../api/eventOperations';
import { getActiveSession, useSession } from '../session/SessionContext';
import { parseEventTaskId } from '../utils/eventTaskRoutes';
import EventRaciEditor from '../components/events/EventRaciEditor';

export default function EventTaskPage({ eventId, activityId }: { eventId: string; activityId: string | null }) {
  const { t } = useTranslation();
  const { session, loading } = useSession();
  const event = parseEventTaskId(eventId);
  const activity = parseEventTaskId(activityId);
  const [reload, setReload] = useState(0);
  const [commandLock, setCommandLock] = useState({ generation: -1, locked: false });
  const context = useRef({ session, loading, eventId, activityId, reload, generation: 0 });
  const previous = context.current;
  const generation = previous.generation + Number(previous.session !== session || previous.loading !== loading
    || previous.eventId !== eventId || previous.activityId !== activityId || previous.reload !== reload);
  context.current = { session, loading, eventId, activityId, reload, generation };
  const [receipt, setReceipt] = useState<{ generation: number; task?: EventOperationTask; failed: boolean }>({ generation: -1, failed: false });

  useEffect(() => {
    // Rendering is fenced synchronously below; also release old receipt data from state.
    setReceipt({ generation, failed: false });
    if (loading || !session || event === null || activity === null || getActiveSession() !== session) return;
    const controller = new AbortController();
    let disposed = false;
    const current = () => !disposed && context.current.generation === generation && getActiveSession() === session;
    void EventOperations.task(event, activity, { apiToken: session.apiToken ?? undefined, signal: controller.signal })
      .then(task => { if (current()) setReceipt({ generation, task, failed: false }); })
      .catch(() => { if (current()) setReceipt({ generation, failed: true }); });
    return () => { disposed = true; controller.abort(); };
  }, [activity, event, generation, loading, session]);

  const current = receipt.generation === generation && getActiveSession() === session;
  const task = current && !loading && session ? receipt.task : undefined;
  const failed = current && receipt.failed;
  const valid = event !== null && activity !== null;
  const pending = loading || (valid && !!session && !task && !failed);
  const locked = commandLock.generation === generation && commandLock.locked;

  return <Stack spacing={3} component="section" aria-labelledby="event-task-title">
    <Typography id="event-task-title" component="h1" variant="h4">{t('eventTask.title')}</Typography>
    <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
      <Button component={RouterLink} to="/social/eventos" disabled={locked}>{t('eventTask.back')}</Button>
      {valid && session && <Button variant="outlined" disabled={pending || locked} onClick={() => setReload(value => value + 1)}>
        {t(failed ? 'eventTask.retry' : 'eventTask.refresh')}
      </Button>}
    </Stack>
    {!valid ? <Alert severity="error">{t('eventTask.invalid')}</Alert>
      : loading ? <Typography role="status">{t('eventTask.loading')}</Typography>
        : !session ? <Alert severity="info">{t('eventTask.signIn')}</Alert>
          : failed ? <Alert severity="warning">{t('eventTask.error')}</Alert>
            : !task ? <Typography role="status">{t('eventTask.loading')}</Typography>
              : <Stack spacing={2}>
                <Typography>{t('eventTask.identity', { event: task.eventId, task: task.activityId })}</Typography>
                <Typography>{t(`eventTask.status_${task.status}`)}</Typography>
                <Typography>{t('eventTask.version', { version: task.version })}</Typography>
                {task.accountabilityNeedsAttention && <Alert severity="warning">{t('eventTask.attention')}</Alert>}
                {task.policy ? <Stack spacing={1}>
                  <Typography>{t('eventTask.policyVersion', { version: task.policy.version })}</Typography>
                  <Typography>{t('eventTask.accountability')}: {t(task.policy.requiresAccountability ? 'eventTask.yes' : 'eventTask.no')}</Typography>
                  <Typography>{t('eventTask.dependencies')}: {t(task.policy.dependenciesGateCompletion ? 'eventTask.yes' : 'eventTask.no')}</Typography>
                </Stack> : <Typography>{t('eventTask.noPolicy')}</Typography>}
                {task.raci.length === 0 ? <Typography>{t('eventTask.noAssignments')}</Typography>
                  : <TableContainer><Table size="small" aria-label={t('eventTask.raci')}>
                    <TableHead><TableRow><TableCell scope="col">{t('eventTask.party')}</TableCell><TableCell scope="col">{t('eventTask.role')}</TableCell></TableRow></TableHead>
                    <TableBody>{task.raci.map(assignment => <TableRow key={`${assignment.partyId}:${assignment.role}`}>
                      <TableCell component="th" scope="row">{t('eventTask.partyId', { id: assignment.partyId })}</TableCell>
                      <TableCell>{t(`eventTask.role_${assignment.role}`)}</TableCell>
                    </TableRow>)}</TableBody>
                  </Table></TableContainer>}
                <Typography variant="body2" color="text.secondary">{t('eventTask.readOnly')}</Typography>
                <EventRaciEditor key={generation} eventId={task.eventId} activityId={task.activityId} session={session}
                  onLock={value => setCommandLock({ generation, locked: value })}
                  onRefresh={() => setReload(value => value + 1)} />
              </Stack>}
  </Stack>;
}
