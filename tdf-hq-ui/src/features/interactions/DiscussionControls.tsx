import { useState } from 'react';
import { commentPolicyAvailable } from './model';
import { useInfiniteQuery } from '@tanstack/react-query';
import { Alert, Button, Dialog, DialogActions, DialogContent, DialogTitle, MenuItem, Select, Stack, TextField, Typography } from '@mui/material';
import { PartyMultiSelector } from '../../components/party-selector/PartySelector';
import type { PartySelectorOption } from '../../api/partySelector';
import { Interactions } from '../../api/interactions';
import type { InteractionCommand, InteractionSummary } from '../../api/interactions';

export function DiscussionControls({ summary, scope, run }: { summary: InteractionSummary; scope: string; run: (command: InteractionCommand) => Promise<void> }) {
  const [open, setOpen] = useState(false); const [moderationOpen, setModerationOpen] = useState(false);
  const [policy, setPolicy] = useState(commentPolicyAvailable(summary.commentPolicy, summary.ownerId) ? summary.commentPolicy : 'off');
  const [people, setPeople] = useState<PartySelectorOption[]>([]);
  const [pending, setPending] = useState(false); const [error, setError] = useState(''); const [reason, setReason] = useState('');
  const queue = useInfiniteQuery({ queryKey: ['interactions', scope, summary.kind, summary.key, 'moderation'],
    initialPageParam: undefined as string | undefined, queryFn: ({ pageParam, signal }) => Interactions.moderation(summary.id, pageParam, signal),
    getNextPageParam: (page) => page.nextCursor ?? undefined, enabled: moderationOpen, retry: false });
  return <>
    <Stack direction="row" spacing={1}>
      {summary.canManage && <Button onClick={() => { setPolicy(commentPolicyAvailable(summary.commentPolicy, summary.ownerId) ? summary.commentPolicy : 'off'); setPeople((summary.mentionedPeople ?? []).map((person) => ({ partyId: person.id, displayName: person.displayName, avatarUrl: person.avatarUrl, username: null, secondaryLabel: null, partyType: 'person', accountStatus: 'active' }))); setError(''); setOpen(true); }}>Quién puede comentar</Button>}
      {(summary.canManage || summary.canModerate) && <Button onClick={() => setModerationOpen(true)}>Moderación</Button>}
    </Stack>
    <Dialog open={open} onClose={() => { if (!pending) setOpen(false); }} aria-labelledby={`policy-${summary.id}`}>
      <DialogTitle id={`policy-${summary.id}`}>Quién puede comentar</DialogTitle><DialogContent><Stack spacing={2} sx={{ pt: 1 }}>
        <Select value={policy} inputProps={{ 'aria-label': 'Permiso para comentar' }} onChange={(event) => setPolicy(event.target.value as typeof policy)}>
          <MenuItem value="everyone">Todos con acceso</MenuItem>{commentPolicyAvailable('followers', summary.ownerId) && <MenuItem value="followers">Seguidores</MenuItem>}
          <MenuItem value="mentioned">Personas mencionadas</MenuItem><MenuItem value="off">Comentarios desactivados</MenuItem>
        </Select>
        {policy === 'mentioned' && <><Typography>Selecciona las personas que podrán comentar.</Typography><PartyMultiSelector value={people} onChange={setPeople}
          field={{ label: 'Personas mencionadas' }} search={{ context: 'interaction_mention', scopeId: summary.id }} /></>}
        {error && <Alert severity="error">{error}</Alert>}
      </Stack></DialogContent><DialogActions><Button disabled={pending} onClick={() => setOpen(false)}>Cancelar</Button>
        <Button disabled={pending || (policy === 'mentioned' && people.length === 0)} onClick={() => {
          setPending(true); setError(''); void run({ operation: 'settings.update', commentPolicy: policy, expectedVersion: summary.version, mentionedPartyIds: people.map((person) => person.partyId) })
            .then(() => setOpen(false)).catch(() => setError('No se pudieron guardar los permisos. Actualiza la conversación e inténtalo de nuevo.')).finally(() => setPending(false));
        }}>Guardar</Button></DialogActions>
    </Dialog>
    <Dialog open={moderationOpen} onClose={() => setModerationOpen(false)} aria-labelledby={`moderation-${summary.id}`} fullWidth maxWidth="sm">
      <DialogTitle id={`moderation-${summary.id}`}>Moderación de la conversación</DialogTitle><DialogContent><Stack spacing={2}>
        {queue.isPending && <Typography role="status">Cargando…</Typography>}
        {queue.isError && <Alert severity="error" action={<Button onClick={() => void queue.refetch()}>Reintentar</Button>}>No se pudo cargar la moderación.</Alert>}
        {queue.data?.pages.every((page) => page.items.length === 0) && <Typography>No hay comentarios pendientes.</Typography>}
        <TextField label="Motivo de la decisión" multiline value={reason} onChange={(event) => setReason(event.target.value)} inputProps={{ maxLength: 1000 }} />
        {queue.data?.pages.flatMap((page) => page.items).map((comment) => <Stack key={comment.id} spacing={1}>
          <Typography sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere' }}>{comment.moderationBody}</Typography>
            {!!comment.reportReasons?.length && <Stack component="section" aria-label="Motivos de los reportes" spacing={1}>
              <Typography variant="caption">Motivos recientes ({comment.reportReasons.length} de {comment.openReports ?? 0})</Typography>
              {comment.reportReasons.map((text, index) => <Typography key={index} sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere' }}>{text}</Typography>)}
            </Stack>}
          <Typography variant="caption">{comment.state === 'hidden' ? 'Oculto' : `${comment.openReports ?? 0} reportes`}</Typography>
          <Stack direction="row">
            {summary.canManage && comment.state === 'visible' && <Button disabled={pending || !reason.trim()} onClick={() => {
              setPending(true); void run({ operation: 'comment.hide', commentId: comment.id, expectedVersion: comment.version, reason })
                .catch(() => setError('No se pudo ocultar el comentario.')).finally(() => setPending(false));
            }}>Ocultar en mi contenido</Button>}
            {comment.state === 'hidden' && <Button disabled={pending || !reason.trim()} onClick={() => {
              setPending(true); void run({ operation: 'comment.restore', commentId: comment.id, expectedVersion: comment.version, reason })
                .catch(() => setError('No se pudo restaurar el comentario.')).finally(() => setPending(false));
            }}>Restaurar</Button>}
            {summary.canModerate && ['visible', 'hidden'].includes(comment.state) && <Button disabled={pending || !reason.trim()} onClick={() => {
              setPending(true); void run({ operation: 'comment.remove', commentId: comment.id, expectedVersion: comment.version, reason })
                .catch(() => setError('No se pudo retirar el comentario.')).finally(() => setPending(false));
            }}>Retirar como administrador</Button>}
            {summary.canModerate && (comment.openReports ?? 0) > 0 && <Button disabled={pending || !reason.trim()} onClick={() => {
              setPending(true); void run({ operation: 'comment.report.resolve', commentId: comment.id, expectedVersion: comment.version, reason, decision: 'dismissed' })
                .catch(() => setError('No se pudo resolver el reporte.')).finally(() => setPending(false));
            }}>Desestimar reportes</Button>}
          </Stack>
        </Stack>)}
        {error && <Alert severity="error">{error}</Alert>}
        {queue.hasNextPage && <Button disabled={queue.isFetchingNextPage} onClick={() => void queue.fetchNextPage()}>Ver más</Button>}
      </Stack></DialogContent><DialogActions><Button onClick={() => setModerationOpen(false)}>Cerrar</Button></DialogActions>
    </Dialog>
  </>;
}
