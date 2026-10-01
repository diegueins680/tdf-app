import { useQuery } from '@tanstack/react-query';
import { Alert, Button, Card, CardContent, Skeleton, Typography } from '@mui/material';
import { ApiError } from '../../api/client';
import { Interactions } from '../../api/interactions';
import { notificationUuid } from '../../components/notificationTarget';
import { useSession } from '../../session/SessionContext';
import { InteractionPanel } from '../interactions/InteractionPanel';
import { PublicationAnchor, type PublicationSelection } from '../interactions/PublicationSelection';

type RecordKind = 'recording' | 'recording_session' | 'record_release';

/** A bounded destination for published items outside the feed window or without a preview. */
export function SelectedRecordPublication({ kind, selection, loading }: {
  kind: RecordKind; selection: PublicationSelection; loading: boolean;
}) {
  if (loading || selection.requested === null || selection.index >= 0) return null;
  if (!notificationUuid(selection.requested)) return selection.notice;
  return <SelectedRecord key={`${kind}:${selection.requested}`} kind={kind} entityKey={selection.requested} />;
}

function SelectedRecord({ kind, entityKey }: { kind: RecordKind; entityKey: string }) {
  const { session } = useSession();
  const scope = session ? `account:${session.partyId ?? session.username}` : 'anonymous';
  // This is the same permission-enforcing query used by the panel. Feed position
  // and media availability are presentation constraints, never publication grants.
  const summary = useQuery({ queryKey: ['interactions', scope, kind, entityKey, 'summary'],
    queryFn: ({ signal }) => Interactions.summary({ kind, entityKey }, Boolean(session), signal), retry: false });
  if (summary.isPending) return <Skeleton height={160} aria-label="Cargando publicación" />;
  if (summary.isError) {
    const unavailable = summary.error instanceof ApiError && [401, 403, 404].includes(summary.error.status);
    return <Alert severity={unavailable ? 'info' : 'warning'} role="status"
      action={unavailable ? undefined : <Button onClick={() => void summary.refetch()}>Reintentar</Button>}>
      {unavailable ? 'Esta publicación ya no está disponible o no tienes acceso.' : 'No se pudo cargar la publicación.'}
    </Alert>;
  }
  if (!summary.data) return null;
  return <Card component={PublicationAnchor} selected aria-label={summary.data.title} sx={{ mb: 3 }}>
    <CardContent>
      <Typography component="h3" variant="h6">{summary.data.title}</Typography>
      <InteractionPanel kind={kind} entityKey={entityKey} initiallyExpanded />
    </CardContent>
  </Card>;
}
