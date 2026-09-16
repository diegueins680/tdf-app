import { useState } from 'react';
import { useInfiniteQuery, useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Button, Card, Checkbox, FormControlLabel, Stack, Tab, Tabs, Typography } from '@mui/material';
import { Link } from 'react-router-dom';
import { SocialV2, type SocialOperation, type SocialRelationship } from '../../api/socialV2';
import { useSession } from '../../session/SessionContext';

function RelationshipActions({ partyId, displayName, initial }: {
  partyId: number; displayName: string; initial: SocialRelationship;
}) {
  const { session } = useSession();
  const qc = useQueryClient();
  const root = ['social-v2', session?.partyId];
  const mutation = useMutation({
    mutationFn: (operation: SocialOperation) => {
      return SocialV2.command(partyId, operation, initial.revision, crypto.randomUUID());
    },
    // A successful block/mute can revoke cached eligibility. Drop visible cached
    // results while the authoritative queries reload.
    onSuccess: () => { void qc.resetQueries({ queryKey: root }); },
    onError: () => { void qc.invalidateQueries({ queryKey: root }); },
  });
  const data = initial;
  const action = (label: string, op: SocialOperation) => <Button size="small"
    disabled={!data || mutation.isPending} onClick={() => mutation.mutate(op)}
    aria-label={`${label}: ${displayName}`}>{label}</Button>;
  return <Stack spacing={1}>
    {mutation.isError && <Alert severity="error">
      No se pudo actualizar esta relación. Actualiza para comprobar su estado.
      <Button onClick={() => { void qc.invalidateQueries({ queryKey: root }); }}>Actualizar</Button>
    </Alert>}
    <Stack direction="row" flexWrap="wrap" gap={1}>
      {data?.blocked ? action('Desbloquear', 'unblock') : <>
        {action(data?.following ? 'Dejar de seguir' : 'Seguir', data?.following ? 'unfollow' : 'follow')}
        {data?.connected ? action('Desconectar', 'disconnect')
          : data?.incoming ? action('Aceptar conexión', 'accept')
          : data?.requested ? action('Cancelar solicitud', 'disconnect') : action('Conectar', 'request')}
        {action(data?.muted ? 'Dejar de silenciar' : 'Silenciar', data?.muted ? 'unmute' : 'mute')}
        {action('Bloquear', 'block')}
        {action(data?.dismissed ? 'Volver a recomendar' : 'No me interesa', data?.dismissed ? 'undismiss' : 'dismiss')}
      </>}
    </Stack>
  </Stack>;
}

export default function SocialWorkspace() {
  const { session } = useSession();
  // Keyed parent remounts this component on account change; no prior account page state.
  const [tab, setTab] = useState(0);
  const qc = useQueryClient();
  const root = ['social-v2', session?.partyId];
  const enabled = Boolean(session?.partyId);
  const profile = useQuery({ queryKey: [...root, 'me'], queryFn: SocialV2.me, enabled });
  const feed = useInfiniteQuery({ queryKey: [...root, 'following'],
    queryFn: ({ pageParam }) => SocialV2.following(pageParam), initialPageParam: undefined as string | undefined,
    getNextPageParam: (last) => last.nextCursor ?? undefined, enabled: enabled && tab === 0 });
  const discover = useQuery({ queryKey: [...root, 'discover'], queryFn: SocialV2.discover,
    enabled: enabled && tab === 1 });
  const preferences = useMutation({
    mutationFn: (value: { discoverable: boolean; personalized: boolean }) => {
      if (!profile.data) throw new Error('Perfil no disponible');
      return SocialV2.preferences(value, profile.data.revision);
    },
    onSettled: () => { void qc.invalidateQueries({ queryKey: root }); },
  });
  if (!enabled) return <Alert severity="info">Inicia sesión para ver tu comunidad.</Alert>;
  const posts = feed.isError ? [] : feed.data?.pages.flatMap((page) => page.items) ?? [];
  return <Stack spacing={3}>
    <Typography variant="h4" component="h1">Tu comunidad musical</Typography>
    <Typography>Encuentra personas con quienes crear, colaborar y compartir música.</Typography>
    {(profile.isError || preferences.isError) && <Alert severity="error">
      No pudimos cargar tu configuración. <Button onClick={() => { void profile.refetch(); }}>Reintentar</Button>
    </Alert>}
    {profile.data && !profile.isError && <Stack>
      <FormControlLabel label="Mostrar mi perfil en Descubrir" control={<Checkbox checked={profile.data.discoverable}
        disabled={preferences.isPending} onChange={(_, discoverable) => preferences.mutate({
          discoverable, personalized: profile.data.personalized })} />} />
      <FormControlLabel label="Personalizar con mis intereses musicales" control={<Checkbox checked={profile.data.personalized}
        disabled={preferences.isPending} onChange={(_, personalized) => preferences.mutate({
          personalized, discoverable: profile.data.discoverable })} />} />
      <Button component={Link} to="/fans">Editar mis intereses musicales</Button>
    </Stack>}
    <Tabs value={tab} onChange={(_, value: number) => setTab(value)} aria-label="Contenido social">
      <Tab id="social-following-tab" aria-controls="social-following-panel" label="Siguiendo" />
      <Tab id="social-discover-tab" aria-controls="social-discover-panel" label="Descubrir" />
      <Tab id="social-connections-tab" aria-controls="social-connections-panel" label="Conexiones" />
    </Tabs>
    {tab === 0 && <Stack role="tabpanel" id="social-following-panel" aria-labelledby="social-following-tab" spacing={2}>
      <Typography>Publicaciones de los clubes y personas que sigues, de más recientes a más antiguas.</Typography>
      <Button onClick={() => { qc.removeQueries({ queryKey: [...root, 'following'] }); void feed.refetch(); }}>Actualizar publicaciones</Button>
      {feed.isPending && <Typography role="status">Cargando publicaciones…</Typography>}
      {feed.isError && <Alert severity="error">No pudimos cargar las publicaciones. <Button onClick={() => { void feed.refetch(); }}>Reintentar</Button></Alert>}
      {!feed.isPending && !feed.isError && posts.length === 0 && <Typography>Sigue artistas o descubre personas para empezar.</Typography>}
      {posts.map((post) => <Card key={post.postId} component="article" sx={{ p: 2 }}>
        <Typography variant="h6">{post.title ?? post.authorName}</Typography>
        <Typography component="time" dateTime={post.publishedAt} variant="caption">
          {new Date(post.publishedAt).toLocaleString('es-EC')}
        </Typography>
        <Typography>{post.authorName}</Typography><Typography sx={{ whiteSpace: 'pre-wrap' }}>{post.content}</Typography>
        <Button component={Link} to={`/artista/${post.artistId}`}>Ver artista</Button>
      </Card>)}
      {feed.hasNextPage && <Button disabled={feed.isFetchingNextPage} onClick={() => { void feed.fetchNextPage(); }}>Ver más</Button>}
    </Stack>}
    {tab === 1 && <Stack role="tabpanel" id="social-discover-panel" aria-labelledby="social-discover-tab" spacing={2}>
      {discover.isPending && <Typography role="status">Buscando perfiles…</Typography>}
      {discover.isError && <Alert severity="error">No pudimos cargar las sugerencias. <Button onClick={() => { void discover.refetch(); }}>Reintentar</Button></Alert>}
      {discover.data?.items.length === 0 && <Typography>No hay sugerencias nuevas. Puedes actualizar tus intereses o volver más tarde.</Typography>}
      {!discover.isError && discover.data?.items.map((person) => <Card key={person.partyId} sx={{ p: 2 }}>
        <Typography variant="h6">{person.displayName}</Typography>
        <Typography>{person.reason === 'shared_interests' ? 'Intereses musicales compartidos' : 'Perfil público de la comunidad'}</Typography>
        <RelationshipActions partyId={person.partyId} displayName={person.displayName} initial={person.relationship} />
      </Card>)}
    </Stack>}
    {tab === 2 && <Stack role="tabpanel" id="social-connections-panel" aria-labelledby="social-connections-tab" spacing={2}>
      {profile.isPending && <Typography role="status">Cargando conexiones…</Typography>}
      {profile.data?.relationships.length === 0 && <Typography>Aún no hay conexiones. Cada persona debe aceptar para conectarse.</Typography>}
      {!profile.isError && profile.data?.relationships.map((person) => <Card key={person.partyId} sx={{ p: 2 }}>
        <Typography variant="h6">{person.displayName}</Typography>
        <Typography>{person.connected ? 'Conexión aceptada' : person.incoming ? 'Solicitud recibida' : person.requested ? 'Solicitud enviada' : ''}</Typography>
        <RelationshipActions partyId={person.partyId} displayName={person.displayName} initial={person} />
      </Card>)}
    </Stack>}
  </Stack>;
}
