import { useParams, Link as RouterLink } from 'react-router-dom';
import { useQuery } from '@tanstack/react-query';
import { Alert, Button, Container, Link, Skeleton, Stack, Typography } from '@mui/material';
import { useSession } from '../session/SessionContext';
import { Interactions } from '../api/interactions';
import { InteractionPanel } from '../features/interactions/InteractionPanel';
import { notificationUuid } from '../components/notificationTarget';

export default function InteractionPage() {
  const { destinationKind, destinationId } = useParams();
  const { session } = useSession();
  const valid = (destinationKind === 'comment' || destinationKind === 'target') && !!notificationUuid(destinationId);
  const destination = useQuery({ queryKey: ['interactions', session ? `account:${session.partyId ?? session.username}` : 'anonymous', 'destination', destinationKind, destinationId],
    queryFn: ({ signal }) => Interactions.destination(destinationKind as 'comment' | 'target', destinationId!, Boolean(session), signal), enabled: valid, retry: false });
  if (!valid || destination.isError) return <Container maxWidth="md" sx={{ py: 4 }}><Stack spacing={2}>
    <Alert severity="info">Esta conversación no está disponible. Puede haberse retirado o requerir acceso.</Alert>
    {!session && <Button component={RouterLink} to={`/login?redirect=${encodeURIComponent(`/conversacion/${destinationKind}/${destinationId}`)}`}>Iniciar sesión</Button>}
    <Link component={RouterLink} to="/inicio">Volver al inicio</Link>
  </Stack></Container>;
  if (!destination.data) return <Container maxWidth="md" sx={{ py: 4 }}><Skeleton height={160} aria-label="Cargando conversación" /></Container>;
  const data = destination.data;
  return <Container maxWidth="md" sx={{ py: 4 }}><Stack spacing={2}>
    <Typography component="h1" variant="h5">{data.title}</Typography>
    <Link component={RouterLink} to={data.route}>Ver publicación</Link>
    <InteractionPanel key={`${data.kind}:${data.key}:${session?.partyId ?? 'anonymous'}`} kind={data.kind} entityKey={data.key} focusCommentId={data.commentId ?? undefined} initiallyExpanded />
  </Stack></Container>;
}
