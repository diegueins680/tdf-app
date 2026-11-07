import { useState } from 'react';
import { Alert, Button, Stack, Typography } from '@mui/material';
import { useInfiniteQuery } from '@tanstack/react-query';
import { get, post } from '../api/client';
import { useSession } from '../session/SessionContext';

interface MediaHealth {
  sourceKind: 'asset' | 'legacy'; sourceId: string; sourceVersion: string;
  status: 'pending' | 'valid' | 'temporary' | 'invalid'; reason: string | null;
  checkedAt: string | null; nextCheckAt: string;
}
const reason: Record<string, string> = {
  decodable: 'Audio comprobado', missing: 'Archivo no encontrado', unsupported: 'Formato no reproducible',
  offline: 'Sin conexión', timeout: 'Tiempo de espera agotado', server: 'Servidor temporalmente no disponible',
  authentication: 'El proveedor requiere autorización', expired: 'El enlace necesita renovación',
  unsafe_url: 'La dirección no admite una comprobación segura', too_large: 'Archivo fuera del límite de comprobación',
  inconclusive: 'La muestra de audio no permite confirmar el formato', transport: 'No se pudo conectar con el proveedor',
};
export default function MediaHealthPanel() {
  const { session } = useSession();
  if (!session || !Number.isSafeInteger(session.partyId) || (session.partyId ?? 0) <= 0) return null;
  return <OwnerMediaHealth key={session.partyId} ownerId={session.partyId!} />;
}
function OwnerMediaHealth({ ownerId }: { ownerId: number }) {
  const [message, setMessage] = useState<string | null>(null);
  const [pending, setPending] = useState<string | null>(null);
  const health = useInfiniteQuery({
    queryKey: ['music-media-health', ownerId],
    initialPageParam: null as Pick<MediaHealth, 'sourceKind' | 'sourceId'> | null,
    queryFn: ({ pageParam, signal }) => {
      const query = new URLSearchParams({ limit: '100' });
      if (pageParam) { query.set('afterKind', pageParam.sourceKind); query.set('afterId', pageParam.sourceId); }
      return get<MediaHealth[]>(`/music/media-health?${query}`, { signal });
    },
    getNextPageParam: (last) => last.length === 100 ? last.at(-1) : undefined,
    retry: false,
  });
  const sources = health.data?.pages.flat() ?? [];
  return <Stack spacing={1} sx={{ mt: 2 }}>
    <Typography component="h2" variant="h6">Estado de los archivos de música</Typography>
    <Typography variant="body2">Los archivos confirmados como no reproducibles se conservan aquí y se ocultan del audio público. Una comprobación correcta los vuelve a habilitar.</Typography>
    <Button onClick={() => { void health.refetch(); }} disabled={health.isFetching}>Actualizar comprobaciones</Button>
    {message && <Alert severity="info">{message}</Alert>}
    {health.isError && !health.isFetchNextPageError && <Alert severity="warning">No pudimos consultar los archivos. Intenta actualizar.</Alert>}
    {sources.filter((item) => item.status !== 'valid').map((item) => {
      const id = `${item.sourceKind}:${item.sourceId}`;
      return <Alert key={id} severity={item.status === 'invalid' ? 'error' : 'warning'}
        action={<Button disabled={pending !== null} onClick={() => {
          setPending(id); setMessage(null);
          void post(`/music/media-health/${encodeURIComponent(item.sourceKind)}/${encodeURIComponent(item.sourceId)}/revalidate`, {})
            .then(() => { setMessage('Comprobación programada. Actualiza para consultar el resultado.'); })
            .catch(() => { setMessage('No pudimos programar la comprobación. Revisa tus permisos e inténtalo otra vez.'); })
            .finally(() => setPending(null));
        }}>Comprobar de nuevo</Button>}>
        <Typography variant="body2">{id}: {item.reason ? reason[item.reason] ?? 'Comprobación pendiente' : 'Comprobación pendiente'}</Typography>
        {item.checkedAt && <Typography variant="caption">Última comprobación: {new Date(item.checkedAt).toLocaleString()}</Typography>}
      </Alert>;
    })}
    {health.isFetchNextPageError && <Alert severity="warning">No pudimos cargar los siguientes archivos. Las comprobaciones anteriores siguen disponibles.</Alert>}
    {health.hasNextPage && <Button disabled={health.isFetchingNextPage} onClick={() => { void health.fetchNextPage(); }}>
      {health.isFetchingNextPage ? 'Cargando archivos…' : health.isFetchNextPageError ? 'Reintentar archivos' : 'Cargar más archivos'}
    </Button>}
    {health.data && <Typography variant="caption">{sources.length} archivos consultados. Cada página comprueba tus permisos actuales.</Typography>}
  </Stack>;
}
