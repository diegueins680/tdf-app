import { useEffect, useId, useRef, useState } from 'react';
import { Alert, Box, Button, Card, CardContent, Chip, Dialog, DialogActions, DialogContent, DialogTitle, Stack, TextField, Typography } from '@mui/material';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { ApiError } from '../../api/client';
import { safeMemoryFileUrl, sourceCommand, sourceReports, validSourceControls } from '../../api/eventMemoryModeration';
import type { MemorySourceCommand, MemorySourceControls, MemorySourceReport } from '../../api/eventMemoryModeration';
import { useSession } from '../../session/SessionContext';
import { useSessionOwner } from '../../session/useSessionOwner';

type Action = 'report' | 'hide' | 'remove' | 'restore' | 'resolve';
interface Selection { action: Action; targetId: string; version: number; controls: MemorySourceControls }
const actionLabel = (action: Action) => ({ report: 'Denunciar recuerdo', hide: 'Ocultar recuerdo', remove: 'Retirar recuerdo', restore: 'Restaurar recuerdo', resolve: 'Cerrar denuncia sin ocultar' })[action];
const stateLabel = (state: MemorySourceControls['state']) => ({ visible: 'Visible', hidden: 'Oculto', removed: 'Retirado' })[state];
const actionError = (error: unknown) => error instanceof ApiError && error.status === 409
  ? 'El recuerdo cambió. Cancela y actualiza la revisión antes de volver a decidir.'
  : error instanceof ApiError && [401, 403, 404].includes(error.status)
    ? 'El recuerdo o tus permisos cambiaron. Cancela y actualiza el evento.'
    : 'No se pudo confirmar la acción. Puedes reintentar con el mismo motivo.';

function SourceActionDialog({ selection, onClose, onDone }: { selection: Selection; onClose: () => void; onDone: () => Promise<void> }) {
  const { session } = useSession();
  const ownership = useSessionOwner(session?.partyId ?? null, selection.targetId);
  // A dialog belongs to the session that opened it, including across retries.
  const openedOwner = useRef(ownership.isCurrentOwner);
  const heading = useId(); const [reason, setReason] = useState(''); const [pending, setPending] = useState(false); const [error, setError] = useState('');
  const attempt = useRef<{ key: string; command: MemorySourceCommand } | null>(null);
  const mounted = useRef(true);
  useEffect(() => { mounted.current = true; return () => { mounted.current = false; }; }, []);
  const confirm = async () => {
    const { action, version, controls, targetId } = selection;
    if (!reason.trim() || pending) return;
    if (!openedOwner.current()) { setError('Tu sesión cambió. Cancela y vuelve a abrir esta acción.'); return; }
    if (!attempt.current) {
      const shared = { reason: reason.trim(), expectedSourceRevision: controls.sourceRevision };
      const command: MemorySourceCommand = action === 'report' ? { operation: 'source.report', ...shared }
        : action === 'resolve' ? { operation: 'source.report.resolve', ...shared, expectedVersion: version, decision: 'dismissed' }
          : { operation: action === 'hide' ? 'source.hide' : action === 'remove' ? 'source.remove' : 'source.restore', ...shared, expectedVersion: version };
      attempt.current = { key: crypto.randomUUID(), command };
    }
    setPending(true); setError('');
    try {
      await sourceCommand(targetId, attempt.current.command, attempt.current.key);
      if (mounted.current && openedOwner.current()) { await onDone(); if (mounted.current && openedOwner.current()) onClose(); }
    } catch (failure) { if (mounted.current) setError(actionError(failure)); }
    finally { if (mounted.current) setPending(false); }
  };
  return <Dialog open onClose={() => { if (!pending) onClose(); }} aria-labelledby={heading} fullWidth maxWidth="sm">
    <DialogTitle id={heading}>{actionLabel(selection.action)}</DialogTitle>
    <DialogContent><Stack spacing={2} sx={{ pt: 1 }}>
      <Typography>{selection.action === 'report' ? 'El equipo autorizado podrá revisar tu motivo. Tu identidad no aparecerá en la revisión del organizador.'
        : selection.action === 'restore' ? 'El recuerdo volverá a estar disponible según los permisos del evento.'
          : selection.action === 'resolve' ? 'La publicación seguirá visible. El motivo quedará en el registro de revisión.'
            : 'El recuerdo y su conversación dejarán de mostrarse. Los archivos públicos y las copias externas pueden seguir existiendo.'}</Typography>
      <TextField label="Motivo" value={reason} onChange={(event) => setReason(event.target.value)} disabled={pending || Boolean(attempt.current)}
        inputProps={{ maxLength: 1000 }} helperText={attempt.current ? 'Para cambiar el motivo, cancela y vuelve a abrir esta acción.' : 'Describe el motivo, hasta 1.000 caracteres.'} fullWidth required />
      {error && <Alert severity="error">{error}</Alert>}
    </Stack></DialogContent>
    <DialogActions><Button disabled={pending} onClick={onClose}>Cancelar</Button>
      <Button disabled={pending || !reason.trim() || Array.from(reason).some((character) => character.charCodeAt(0) < 32 || character.charCodeAt(0) === 127)} onClick={() => void confirm()}>{pending ? 'Guardando…' : 'Confirmar'}</Button></DialogActions>
  </Dialog>;
}

async function refreshSource(client: ReturnType<typeof useQueryClient>, eventId: string) {
  await Promise.all([
    client.invalidateQueries({ queryKey: ['event-memory-source-reports'] }),
    client.invalidateQueries({ queryKey: ['social-event-moments', eventId] }),
    client.invalidateQueries({ queryKey: ['interactions'] }),
    client.invalidateQueries({ queryKey: ['notifications'] }),
  ]);
}

/** Render only server-advertised source permissions; older backends omit them. */
export function EventMemorySourceActions({ targetId, version, controls }: { targetId: string; version: number; controls: MemorySourceControls }) {
  const { session } = useSession(); const client = useQueryClient(); const [selection, setSelection] = useState<Selection | null>(null);
  const scope = session ? String(session.partyId ?? session.username) : 'anonymous';
  const selectionScope = useRef(scope);
  useEffect(() => { setSelection(null); }, [scope, targetId]);
  if (!session || !validSourceControls(controls)) return null;
  const choose = (action: Action) => { selectionScope.current = scope; setSelection({ action, targetId, version, controls }); };
  return <>
    {controls.canReport && <Button onClick={() => choose('report')}>Denunciar recuerdo</Button>}
    {controls.canHide && <Button onClick={() => choose('hide')}>Ocultar recuerdo</Button>}
    {controls.canRemove && <Button color="error" onClick={() => choose('remove')}>Retirar recuerdo</Button>}
    {selection && selectionScope.current === scope && selection.targetId === targetId && <SourceActionDialog key={scope} selection={selection} onClose={() => setSelection(null)} onDone={() => refreshSource(client, controls.eventId)} />}
  </>;
}

function ReviewItem({ item, choose }: { item: MemorySourceReport; choose: (selection: Selection) => void }) {
  const url = safeMemoryFileUrl(item.mediaUrl);
  const select = (action: Action) => choose({ action, targetId: item.id, version: item.version, controls: item.source });
  return <Card variant="outlined"><CardContent><Stack spacing={1}>
    <Chip label={stateLabel(item.source.state)} size="small" sx={{ alignSelf: 'flex-start' }} />
    <Typography sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere' }}>{item.caption?.trim() ? item.caption : 'Recuerdo sin descripción'}</Typography>
    {item.author?.displayName && <Typography variant="caption">Publicado por {item.author.displayName}</Typography>}
    {url && <Button component="a" href={url} target="_blank" rel="noopener noreferrer" referrerPolicy="no-referrer" sx={{ alignSelf: 'flex-start' }}>Ver archivo</Button>}
    <Typography variant="body2">Denuncias abiertas: {item.openReports}</Typography>
    {item.reportsReferToEarlierContent && <Alert severity="info">Algunas denuncias corresponden a una versión anterior. Revisa el contenido actual antes de decidir.</Alert>}
    {item.reasons.map((reason, index) => <Typography key={index} variant="body2" sx={{ overflowWrap: 'anywhere' }}>{reason}</Typography>)}
    {item.audit.map((audit, index) => <Typography key={index} variant="caption" sx={{ overflowWrap: 'anywhere' }}>Revisión anterior: {audit.reason}</Typography>)}
    <Stack direction="row" flexWrap="wrap" gap={1}>
      {item.source.canHide && <Button onClick={() => select('hide')}>Ocultar recuerdo</Button>}
      {item.source.canRestore && <Button onClick={() => select('restore')}>Restaurar recuerdo</Button>}
      {item.source.canRemove && <Button color="error" onClick={() => select('remove')}>Retirar recuerdo</Button>}
      {item.openReports > 0 && <Button onClick={() => select('resolve')}>Cerrar denuncia sin ocultar</Button>}
    </Stack>
  </Stack></CardContent></Card>;
}

export function EventMemoryModerationPanel({ eventId, organizer }: { eventId: string; organizer: boolean }) {
  const { session } = useSession(); const client = useQueryClient(); const [open, setOpen] = useState(false);
  const [cursors, setCursors] = useState<string[]>([]); const [selection, setSelection] = useState<Selection | null>(null);
  const scope = session ? String(session.partyId ?? session.username) : 'anonymous'; const cursor = cursors.at(-1);
  const selectionScope = useRef(scope);
  useEffect(() => { setOpen(false); setSelection(null); setCursors([]); }, [scope, eventId]);
  const reviews = useQuery({ queryKey: ['event-memory-source-reports', scope, eventId, cursor ?? ''],
    queryFn: ({ signal }) => sourceReports(eventId, cursor, signal), enabled: Boolean(session && eventId), retry: false });
  if (!session || reviews.isPending) return null;
  if (reviews.isError) return organizer ? <Alert severity="warning" action={<Button onClick={() => void reviews.refetch()}>Reintentar</Button>}>No se pudo cargar la revisión de recuerdos.</Alert> : null;
  if (!reviews.data?.supported || !reviews.data.canReview) return null;
  return <Box component="section" aria-label="Revisión de recuerdos">
    <Button aria-expanded={open} onClick={() => setOpen(!open)}>{open ? 'Cerrar revisión de recuerdos' : 'Revisar recuerdos'}</Button>
    {open && <Stack spacing={2} sx={{ mt: 1 }}>
      <Typography variant="h6">Revisión de recuerdos</Typography>
      <Typography variant="body2">Revisa denuncias y publicaciones ocultas de este evento. La identidad de quienes denunciaron se mantiene fuera de esta vista.</Typography>
      <Button disabled={reviews.isFetching} onClick={() => { setCursors([]); void reviews.refetch(); }}>Actualizar revisión</Button>
      {reviews.data.items.length === 0 && <Typography>No hay recuerdos pendientes de revisión en esta página.</Typography>}
      {reviews.data.items.map((item) => <ReviewItem key={item.id} item={item} choose={(value) => { selectionScope.current = scope; setSelection(value); }} />)}
      <Stack direction="row" gap={1}>
        {cursors.length > 0 && <Button onClick={() => setCursors(cursors.slice(0, -1))}>Página anterior</Button>}
        {reviews.data.nextCursor && <Button onClick={() => setCursors([...cursors, reviews.data.nextCursor!])}>Ver más recuerdos</Button>}
      </Stack>
    </Stack>}
    {selection && selectionScope.current === scope && selection.controls.eventId === eventId && <SourceActionDialog key={scope} selection={selection} onClose={() => setSelection(null)} onDone={() => refreshSource(client, eventId)} />}
  </Box>;
}
