import { useState } from 'react';
import { useInfiniteQuery, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Button, Checkbox, Dialog, DialogActions, DialogContent, DialogTitle, FormControlLabel, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { Interactions } from '../../api/interactions';
import type { InteractionPreferences } from '../../api/interactions';
import { discussionLink } from './model';

const labels: Record<keyof InteractionPreferences, string> = { reactions: 'Reacciones', comments: 'Comentarios', replies: 'Respuestas', mentions: 'Menciones' };
export function AccountControls({ scope, canModerate }: { scope: string; canModerate: boolean }) {
  const [open, setOpen] = useState(false); const [pending, setPending] = useState(false); const [error, setError] = useState('');
  const [section, setSection] = useState<'preferences' | 'blocks' | 'reports'>('preferences'); const client = useQueryClient();
  const preferences = useQuery({ queryKey: ['interactions', scope, 'preferences'], queryFn: () => Interactions.preferences(), enabled: open, retry: false });
  const blocks = useInfiniteQuery({ queryKey: ['interactions', scope, 'blocked-accounts'], initialPageParam: undefined as number | undefined,
    queryFn: ({ pageParam, signal }) => Interactions.blockedAccounts(pageParam, signal), getNextPageParam: (page) => page.nextCursor ?? undefined, enabled: open && section === 'blocks', retry: false });
  const reports = useInfiniteQuery({ queryKey: ['interactions', scope, 'reports'], initialPageParam: undefined as string | undefined,
    queryFn: ({ pageParam, signal }) => Interactions.reports(pageParam, signal), getNextPageParam: (page) => page.nextCursor ?? undefined, enabled: open && section === 'reports' && canModerate, retry: false });
  const perform = async (work: () => Promise<unknown>) => {
    setPending(true); setError('');
    try { await work(); await client.invalidateQueries({ queryKey: ['interactions'] }); }
    catch { setError('No se pudo guardar el cambio. Actualiza e inténtalo de nuevo.'); }
    finally { setPending(false); }
  };
  return <>
    <Button onClick={() => setOpen(true)}>Preferencias de interacción</Button>
    <Dialog open={open} onClose={() => { if (!pending) setOpen(false); }} fullWidth maxWidth="sm" aria-labelledby="interaction-account-title">
      <DialogTitle id="interaction-account-title">Preferencias de interacción</DialogTitle>
      <DialogContent><Stack spacing={2}>
        <Stack direction="row" flexWrap="wrap">
          <Button aria-pressed={section === 'preferences'} onClick={() => setSection('preferences')}>Notificaciones</Button>
          <Button aria-pressed={section === 'blocks'} onClick={() => setSection('blocks')}>Cuentas bloqueadas</Button>
          {canModerate && <Button aria-pressed={section === 'reports'} onClick={() => setSection('reports')}>Reportes</Button>}
        </Stack>
        {section === 'preferences' && <>
          <Typography>Estas preferencias se aplican a todas tus conversaciones. También puedes silenciar una conversación concreta.</Typography>
          {preferences.isPending && <Typography role="status">Cargando…</Typography>}
          {preferences.isError && <Alert severity="error" action={<Button onClick={() => void preferences.refetch()}>Reintentar</Button>}>No se pudieron cargar las preferencias.</Alert>}
          {preferences.data && (Object.keys(labels) as (keyof InteractionPreferences)[]).map((key) => <FormControlLabel key={key} label={labels[key]}
            control={<Checkbox checked={preferences.data[key]} disabled={pending} onChange={(_event, checked) => { void perform(() => Interactions.setPreferences({ ...preferences.data, [key]: checked })); }} />} />)}
        </>}
        {section === 'blocks' && <>
          <Typography>Desbloquear permite nuevas interacciones; no restaura conexiones anteriores.</Typography>
          {blocks.isPending && <Typography role="status">Cargando…</Typography>}
          {blocks.isError && <Alert severity="error" action={<Button onClick={() => void blocks.refetch()}>Reintentar</Button>}>No se pudo cargar la lista.</Alert>}
          {blocks.data?.pages.every((page) => page.items.length === 0) && <Typography>No has bloqueado cuentas.</Typography>}
          {blocks.data?.pages.flatMap((page) => page.items).map((person) => <Stack direction="row" key={person.partyId} alignItems="center" justifyContent="space-between">
            <Typography>{person.displayName}</Typography><Button disabled={pending} onClick={() => { void perform(() => Interactions.block(person.partyId, false, person.version, crypto.randomUUID())); }}>Desbloquear</Button>
          </Stack>)}
          {blocks.hasNextPage && <Button disabled={blocks.isFetchingNextPage} onClick={() => void blocks.fetchNextPage()}>Ver más cuentas</Button>}
        </>}
        {section === 'reports' && <>
          {reports.isPending && <Typography role="status">Cargando reportes…</Typography>}
          {reports.isError && <Alert severity="error" action={<Button onClick={() => void reports.refetch()}>Reintentar</Button>}>No se pudieron cargar los reportes.</Alert>}
          {reports.data?.pages.every((page) => page.items.length === 0) && <Typography>No hay reportes pendientes.</Typography>}
          {reports.data?.pages.flatMap((page) => page.items).map((comment) => <Stack key={comment.id}>
            <Typography sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere' }}>{comment.moderationBody}</Typography>
            {!!comment.reportReasons?.length && <Stack component="section" aria-label="Motivos de los reportes" spacing={1}>
              <Typography variant="caption">Motivos recientes ({comment.reportReasons.length} de {comment.openReports ?? 0})</Typography>
              {comment.reportReasons.map((text, index) => <Typography key={index} sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere' }}>{text}</Typography>)}
            </Stack>}
            <Button component={RouterLink} to={discussionLink('comment', comment.id)} onClick={() => setOpen(false)}>Revisar {comment.openReports} reportes en la conversación</Button>
          </Stack>)}
          {reports.hasNextPage && <Button disabled={reports.isFetchingNextPage} onClick={() => void reports.fetchNextPage()}>Ver más reportes</Button>}
        </>}
        {error && <Alert severity="error">{error}</Alert>}
      </Stack></DialogContent><DialogActions><Button disabled={pending} onClick={() => setOpen(false)}>Cerrar</Button></DialogActions>
    </Dialog>
  </>;
}
