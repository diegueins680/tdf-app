import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Button, Card, CardContent, FormControlLabel, Stack, Switch, TextField, Typography } from '@mui/material';
import { get, post, put } from '../api/client';

interface Source { id: number; partyId?: number; artistProfileId?: number; channelId: string; lastSuccessAt?: string; configuration: { enabled: boolean; collectionId: string; channelTitle?: string; approvalReference?: string } }
interface Run { id: string; key: string; status: string; dryRun: boolean; startedAt: string; report: { sourceAccountId?: string; full?: boolean; phase?: string; checkpoint?: string; pages?: number; error?: string; counts?: Record<string, number> } }
interface Overview { enabled: boolean; intervalSeconds: number; nextScheduledAt: string | null; sources: Source[]; runs: Run[] }
const base = '/admin/records-ingestion';

export default function RecordsIngestionPage() {
  const client = useQueryClient();
  const query = useQuery({ queryKey: ['records-ingestion'], queryFn: () => get<Overview>(base), refetchInterval: 30000 });
  const [channelId, setChannelId] = useState('');
  const [collectionId, setCollectionId] = useState('');
  const [approvalReference, setApprovalReference] = useState('');
  const [interval, setInterval] = useState('');
  const [partyId, setPartyId] = useState('');
  const [artistProfileId, setArtistProfileId] = useState('');
  const [result, setResult] = useState('');
  const action = useMutation({
    mutationFn: async ({ path, body, method = 'put' }: { path: string; body: unknown; method?: 'put' | 'post' }) =>
      method === 'post' ? post<Record<string, unknown>>(`${base}${path}`, body) : put<Record<string, unknown>>(`${base}${path}`, body),
    onSuccess: async value => { setResult(value['status'] === 'resume_required' ? 'Hay una ejecución pendiente. Continúala desde la lista de ejecuciones.'
      : value['status'] === 'failed' ? 'La ejecución falló. Consulta el error en su registro.'
      : value['status'] === 'partial' ? 'Progreso guardado. La ejecución puede continuar.'
      : value['status'] === 'busy' ? 'La fuente ya tiene una ejecución activa.'
      : value['status'] === 'rate_limited' ? 'Espera un minuto antes de volver a intentarlo.'
      : value['status'] === 'disabled_or_unapproved' ? 'La importación está detenida o la fuente no está aprobada.'
      : 'Operación completada.'); await client.invalidateQueries({ queryKey: ['records-ingestion'] }); },
  });
  const seconds = Number(interval !== '' ? interval : (query.data?.intervalSeconds ?? 3600));
  const validInterval = Number.isInteger(seconds) && seconds >= 300 && seconds <= 86400;
  const validIdentity = (value: string) => !value || (/^[1-9][0-9]*$/.test(value) && Number.isSafeInteger(Number(value)));
  const run = (source: Source, reconciliation: boolean, dryRun: boolean) => action.mutate({ path: '/runs', method: 'post', body: {
    sourceAccountId: source.id, executionKey: crypto.randomUUID(), reconciliation, dryRun,
  } });
  return <Stack spacing={3} sx={{ p: 3 }}>
    <Typography variant="h4">Importación de videos</Typography>
    <Typography>Canales oficiales aprobados. Las sesiones TDF conservan su colección editorial. No se envían avisos durante la reconciliación.</Typography>
    {(query.isError || action.isError) && <Alert severity="error">{String(query.error ?? action.error)}</Alert>}
    {query.isLoading && <Typography role="status">Cargando fuentes y ejecuciones…</Typography>}
    {result && <Alert severity="info" sx={{ overflowWrap: 'anywhere' }}>{result}</Alert>}
    {query.data && <>
      <Card><CardContent><Stack spacing={2}>
        <FormControlLabel label="Importación habilitada" control={<Switch checked={query.data.enabled} disabled={action.isPending}
          onChange={(_, running) => action.mutate({ path: '/control', body: { running, intervalSeconds: query.data?.intervalSeconds ?? 3600 } })} />} />
        <Typography>Próxima ejecución: {query.data.nextScheduledAt ? new Date(query.data.nextScheduledAt).toLocaleString() : 'Programación detenida'} (horas guardadas en UTC). Reconciliación semanal: domingo.</Typography>
        <TextField label="Frecuencia en segundos" type="number" value={interval || String(query.data.intervalSeconds)} onChange={e => setInterval(e.target.value)} inputProps={{ min: 300, max: 86400 }} />
        <Button disabled={action.isPending || !validInterval} onClick={() => action.mutate({ path: '/control', body: { running: query.data?.enabled, intervalSeconds: seconds } })}>Guardar frecuencia</Button>
      </Stack></CardContent></Card>
      <Card><CardContent><Stack spacing={2} component="form" onSubmit={event => { event.preventDefault(); action.mutate({ path: '/sources', body: { channelId, collectionId, approvalReference, enabled: true, partyId: partyId ? Number(partyId) : null, artistProfileId: artistProfileId ? Number(artistProfileId) : null } }); }}>
        <Typography variant="h6">Aprobar un canal oficial</Typography>
        <TextField required label="ID estable del canal de YouTube" value={channelId} onChange={e => setChannelId(e.target.value)} />
        <TextField required label="ID de la colección de grabaciones" value={collectionId} onChange={e => setCollectionId(e.target.value)} />
        <TextField required label="Referencia de autorización para importar el canal" value={approvalReference} onChange={e => setApprovalReference(e.target.value)} />
        <TextField label="ID de persona o proyecto vinculado (opcional)" value={partyId} onChange={e => setPartyId(e.target.value)} error={!validIdentity(partyId)} />
        <TextField label="ID de perfil de artista (opcional)" value={artistProfileId} onChange={e => setArtistProfileId(e.target.value)} error={!validIdentity(artistProfileId)} />
        <Button type="submit" disabled={action.isPending || !validIdentity(partyId) || !validIdentity(artistProfileId)}>Verificar y aprobar fuente</Button>
      </Stack></CardContent></Card>
      {query.data.sources.map(source => <Card key={source.id}><CardContent><Stack spacing={1}>
        <Typography variant="h6">{source.configuration?.channelTitle ?? source.channelId}</Typography>
        <Typography sx={{ overflowWrap: 'anywhere' }}>{source.channelId} · Última ejecución completa: {source.lastSuccessAt ?? 'Todavía ninguna'}</Typography>
        <Typography>{source.configuration?.enabled ? 'Habilitada' : 'Deshabilitada'}</Typography>
        <Stack direction="row" spacing={1} useFlexGap sx={{ flexWrap: 'wrap' }}>
          <Button disabled={action.isPending} onClick={() => run(source, false, true)}>Simular</Button>
          <Button disabled={action.isPending} onClick={() => run(source, false, false)}>Importar</Button>
          <Button disabled={action.isPending} onClick={() => run(source, true, false)}>Reconciliar</Button>
          <Button disabled={action.isPending} onClick={() => action.mutate({ path: '/sources', body: { channelId: source.channelId, collectionId: source.configuration?.collectionId ?? '', approvalReference: null, enabled: false, partyId: null, artistProfileId: null } })}>Deshabilitar</Button>
        </Stack>
      </Stack></CardContent></Card>)}
      <Typography variant="h5">Ejecuciones recientes</Typography>
      {query.data.runs.map(item => <Card key={item.id}><CardContent><Stack spacing={1}>
        <Typography sx={{ overflowWrap: 'anywhere' }}>{item.id} · {item.status}{item.dryRun ? ' · Simulación' : ''}</Typography>
        <Typography>{item.startedAt} · Páginas: {item.report.pages ?? 0} · {item.status === 'partial' ? 'Continuación pendiente' : item.status === 'completed' ? 'Completa' : 'Consulta el estado'}</Typography>
        <Typography sx={{ overflowWrap: 'anywhere' }}>{JSON.stringify(item.report.counts ?? {})}</Typography>
        {['partial', 'failed', 'deferred', 'running'].includes(item.status) && item.report.sourceAccountId &&
          <Button disabled={action.isPending} onClick={() => action.mutate({ path: '/runs', method: 'post', body: {
            sourceAccountId: Number(item.report.sourceAccountId), executionKey: item.key.replace(/^records-youtube:(full|incremental):[0-9]+:/, ''),
            reconciliation: item.report.full ?? false, dryRun: item.dryRun,
          } })}>Continuar ejecución</Button>}
        {item.report.error && <Alert severity="error">{item.report.error}</Alert>}
      </Stack></CardContent></Card>)}
    </>}
  </Stack>;
}
