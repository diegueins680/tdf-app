import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Button, Card, CardContent, FormControlLabel, Stack, Switch, TextField, Typography } from '@mui/material';
import { get, post, put } from '../api/client';

interface Source { id: number; channelId: string; lastSuccessAt?: string; configuration: { enabled: boolean; collectionId: string; channelTitle?: string; approvalReference?: string } };
interface Run { id: string; status: string; dryRun: boolean; startedAt: string; report: { checkpoint?: string; pages?: number; error?: string; counts?: Record<string, number> } };
interface Overview { enabled: boolean; intervalSeconds: number; nextScheduledAt: string; sources: Source[]; runs: Run[] };
const base = '/admin/records-ingestion';

export default function RecordsIngestionPage() {
  const client = useQueryClient();
  const query = useQuery({ queryKey: ['records-ingestion'], queryFn: () => get<Overview>(base), refetchInterval: 30000 });
  const [channelId, setChannelId] = useState('');
  const [collectionId, setCollectionId] = useState('');
  const [approvalReference, setApprovalReference] = useState('');
  const [interval, setInterval] = useState('3600');
  const [result, setResult] = useState('');
  const action = useMutation({
    mutationFn: async ({ path, body, method = 'put' }: { path: string; body: unknown; method?: 'put' | 'post' }) =>
      method === 'post' ? post<Record<string, unknown>>(`${base}${path}`, body) : put<Record<string, unknown>>(`${base}${path}`, body),
    onSuccess: async value => { setResult(JSON.stringify(value)); await client.invalidateQueries({ queryKey: ['records-ingestion'] }); },
  });
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
        <Typography>Próxima ejecución: {new Date(query.data.nextScheduledAt).toLocaleString()} (horas guardadas en UTC). Reconciliación semanal: domingo.</Typography>
        <TextField label="Frecuencia en segundos" type="number" value={interval} onChange={e => setInterval(e.target.value)} inputProps={{ min: 300, max: 86400 }} />
        <Button disabled={action.isPending || Number(interval) < 300 || Number(interval) > 86400} onClick={() => action.mutate({ path: '/control', body: { running: query.data?.enabled, intervalSeconds: Number(interval) } })}>Guardar frecuencia</Button>
      </Stack></CardContent></Card>
      <Card><CardContent><Stack spacing={2} component="form" onSubmit={event => { event.preventDefault(); action.mutate({ path: '/sources', body: { channelId, collectionId, approvalReference, enabled: true, partyId: null, artistProfileId: null } }); }}>
        <Typography variant="h6">Aprobar un canal oficial</Typography>
        <TextField required label="ID estable del canal de YouTube" value={channelId} onChange={e => setChannelId(e.target.value)} />
        <TextField required label="ID de la colección de grabaciones" value={collectionId} onChange={e => setCollectionId(e.target.value)} />
        <TextField required label="Referencia de autorización para importar el canal" value={approvalReference} onChange={e => setApprovalReference(e.target.value)} />
        <Button type="submit" disabled={action.isPending}>Verificar y aprobar fuente</Button>
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
        <Typography>{item.startedAt} · Páginas: {item.report.pages ?? 0} · {item.report.checkpoint ? 'Continuación pendiente' : 'Sin continuación'}</Typography>
        <Typography sx={{ overflowWrap: 'anywhere' }}>{JSON.stringify(item.report.counts ?? {})}</Typography>
        {item.report.error && <Alert severity="error">{item.report.error}</Alert>}
      </Stack></CardContent></Card>)}
    </>}
  </Stack>;
}
