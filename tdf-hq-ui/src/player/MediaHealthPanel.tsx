import { useState } from 'react';
import { Alert, Button, Stack, Typography } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
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
  const [message, setMessage] = useState<string | null>(null);
  const [pending, setPending] = useState<string | null>(null);
  const health = useQuery({ queryKey: ['music-media-health', session?.partyId],
    queryFn: ({ signal }) => get<MediaHealth[]>('/music/media-health', { signal }), enabled: Boolean(session), retry: false });
  if (!session) return null;
  return <Stack spacing={1} sx={{ mt: 2 }}>
    <Typography component="h2" variant="h6">Estado de los archivos de música</Typography>
    <Typography variant="body2">Los archivos confirmados como no reproducibles se conservan aquí y se ocultan del audio público. Una comprobación correcta los vuelve a habilitar.</Typography>
    <Button onClick={() => { void health.refetch(); }} disabled={health.isFetching}>Actualizar comprobaciones</Button>
    {message && <Alert severity="info">{message}</Alert>}
    {health.isError && <Alert severity="warning">No pudimos consultar los archivos. Intenta actualizar.</Alert>}
    {health.data?.filter((item) => item.status !== 'valid').map((item) => {
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
    {health.data?.length === 500 && <Typography variant="caption">Se muestran los primeros 500 archivos. Otros archivos siguen en la cola de comprobación.</Typography>}
  </Stack>;
}
