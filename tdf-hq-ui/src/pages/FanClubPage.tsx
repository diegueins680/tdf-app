import { useEffect, useMemo, useState } from 'react';
import { useParams, useLocation, Link as RouterLink } from 'react-router-dom';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Box, Button, Card, CardContent, Chip, Dialog, DialogActions, DialogContent, DialogTitle,
  Divider, Grid, IconButton, Stack, Tab, Tabs, TextField, Typography,
  Avatar, Tooltip, ImageList, ImageListItem,
} from '@mui/material';
import {
  PushPin as PushPinIcon,
  PushPinOutlined as PushPinOutlinedIcon,
  VisibilityOff as VisibilityOffIcon,
  Visibility as VisibilityIcon,
  HowToVote as HowToVoteIcon,
  CalendarMonth as CalendarMonthIcon,
  Forum as ForumIcon,
  Groups as GroupsIcon,
  Add as AddIcon,
  PhotoLibrary as PhotoLibraryIcon,
  Report as ReportIcon,
  Person as PersonIcon,
  LockOutlined as LockOutlinedIcon,
} from '@mui/icons-material';
import PageShell, { EmptyState, SkeletonCards } from '../components/PageShell';
import { Fans } from '../api/fans';
import { useSession } from '../session/SessionContext';
import { buildLoginRedirectPath } from '../utils/loginRouting';
import type { FanClubPostDTO, FanClubEventDTO, FanClubElectionDTO, FanClubCandidacyDTO, FanClubFeedItemDTO, FanClubMemoryDTO } from '../api/types';

export default function FanClubPage() {
  const { artistId } = useParams<{ artistId: string }>();
  const artistIdNum = parseInt(artistId || '0', 10);
  const { session } = useSession();
  const qc = useQueryClient();
  const [tab, setTab] = useState(0);

  const clubQuery = useQuery({
    queryKey: ['fan-club', artistIdNum],
    queryFn: () => Fans.getMyClub(artistIdNum),
    enabled: artistIdNum > 0,
  });

  const feedQuery = useQuery({
    queryKey: ['fan-club-feed', artistIdNum],
    queryFn: () => Fans.listClubFeed(artistIdNum),
    enabled: artistIdNum > 0 && tab === 0,
  });

  const postsQuery = useQuery({
    queryKey: ['fan-club-posts', artistIdNum],
    queryFn: () => Fans.listClubPosts(artistIdNum),
    enabled: artistIdNum > 0 && tab === 1,
  });

  const memoriesQuery = useQuery({
    queryKey: ['fan-club-memories', artistIdNum],
    queryFn: () => Fans.listClubMemories(artistIdNum),
    enabled: artistIdNum > 0 && tab === 2,
  });

  const eventsQuery = useQuery({
    queryKey: ['fan-club-events', artistIdNum],
    queryFn: () => Fans.listClubEvents(artistIdNum),
    enabled: artistIdNum > 0 && tab === 3,
  });

  const electionsQuery = useQuery({
    queryKey: ['fan-club-elections', artistIdNum],
    queryFn: () => Fans.listClubElections(artistIdNum),
    enabled: artistIdNum > 0 && tab === 4,
  });

  const memberProfilesQuery = useQuery({
    queryKey: ['fan-club-member-profiles', artistIdNum],
    queryFn: () => Fans.listClubMemberProfiles(artistIdNum),
    enabled: artistIdNum > 0,
  });

  const club = clubQuery.data;
  const isOfficer = club?.fcOfficers.some(o => o.fcoPartyId === session?.partyId) ?? false;

  return (
    <PageShell
      title={club?.fcName || 'Club de Fans'}
      subtitle={club ? `${club.fcFollowerCount} seguidores` : undefined}
      loading={clubQuery.isLoading}
    >
      {clubQuery.isLoading ? (
        <SkeletonCards count={3} />
      ) : !club ? (
        <EmptyState
          icon={<GroupsIcon fontSize="large" />}
          title="Club no encontrado"
          description="Este artista aún no tiene un club de fans activo."
        />
      ) : (
        <Stack spacing={3}>
          <Card>
            <CardContent>
              <Stack spacing={2}>
                {club.fcArtistImageUrl && (
                  <Box
                    component="img"
                    src={club.fcArtistImageUrl}
                    alt={club.fcName}
                    sx={{
                      width: '100%',
                      height: 280,
                      objectFit: 'cover',
                      borderRadius: 2,
                      mb: 1,
                    }}
                  />
                )}
                <Typography variant="h5" fontWeight={600}>{club.fcName}</Typography>
                {club.fcDescription && (
                  <Typography variant="body1" color="text.secondary">{club.fcDescription}</Typography>
                )}
                <Divider />
                <Stack direction="row" justifyContent="space-between" alignItems="center">
                  <Typography variant="subtitle2" fontWeight={600}>Directiva</Typography>
                  <Button
                    component={RouterLink}
                    to={`/fans/clubs/${artistIdNum}/members`}
                    size="small"
                    startIcon={<PersonIcon />}
                  >
                    Ver miembros
                  </Button>
                </Stack>
                <Stack direction="row" spacing={2} flexWrap="wrap">
                  {club.fcOfficers.length === 0 && (
                    <Typography variant="body2" color="text.secondary">Aún no hay directiva electa.</Typography>
                  )}
                  {club.fcOfficers.map(o => (
                    <Chip
                      key={o.fcoPartyId}
                      avatar={<Avatar src={o.fcoAvatarUrl || undefined} />}
                      label={`${o.fcoFanName} — ${o.fcoRole}`}
                      color="primary"
                      variant="outlined"
                    />
                  ))}
                </Stack>
              </Stack>
            </CardContent>
          </Card>

          <Box sx={{ borderBottom: 1, borderColor: 'divider' }}>
            <Tabs value={tab} onChange={(_, v) => setTab(v)}>
              <Tab icon={<ForumIcon />} label="Feed" />
              <Tab icon={<ForumIcon />} label="Foro" />
              <Tab icon={<PhotoLibraryIcon />} label="Recuerdos" />
              <Tab icon={<CalendarMonthIcon />} label="Calendario" />
              <Tab icon={<HowToVoteIcon />} label="Elecciones" />
            </Tabs>
          </Box>

          {tab === 0 && <ClubFeed artistId={artistIdNum} feed={feedQuery.data ?? []} isOfficer={isOfficer} loading={feedQuery.isLoading} />}
          {tab === 1 && <ClubForum artistId={artistIdNum} posts={postsQuery.data ?? []} isOfficer={isOfficer} loading={postsQuery.isLoading} />}
          {tab === 2 && <ClubMemories artistId={artistIdNum} memories={memoriesQuery.data ?? []} isOfficer={isOfficer} loading={memoriesQuery.isLoading} />}
          {tab === 3 && <ClubCalendar artistId={artistIdNum} events={eventsQuery.data ?? []} isOfficer={isOfficer} loading={eventsQuery.isLoading} />}
          {tab === 4 && <ClubElections artistId={artistIdNum} elections={electionsQuery.data ?? []} />}
        </Stack>
      )}
    </PageShell>
  );
}

function ClubFeed({ artistId, feed, isOfficer, loading }: { artistId: number; feed: FanClubFeedItemDTO[]; isOfficer: boolean; loading: boolean }) {
  const qc = useQueryClient();

  const hideMut = useMutation({
    mutationFn: ({ itemId, kind, hide }: { itemId: number; kind: string; hide: boolean }) => {
      if (kind === 'post') {
        return hide ? Fans.hideClubPost(artistId, itemId) : Fans.unhideClubPost(artistId, itemId);
      } else {
        return hide ? Fans.hideClubMemory(artistId, itemId) : Fans.unhideClubMemory(artistId, itemId);
      }
    },
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['fan-club-feed', artistId] });
      qc.invalidateQueries({ queryKey: ['fan-club-posts', artistId] });
      qc.invalidateQueries({ queryKey: ['fan-club-memories', artistId] });
    },
  });

  if (loading) return <SkeletonCards count={3} />;

  const visibleItems = feed.filter(item => !item.fcfIsHidden);

  return (
    <Stack spacing={2}>
      {visibleItems.length === 0 && (
        <EmptyState icon={<ForumIcon fontSize="large" />} title="Feed vacío" description="Sé el primero en publicar o compartir un recuerdo." />
      )}

      {visibleItems.map(item => (
        <Card key={`${item.fcfKind}-${item.fcfId}`}>
          <CardContent>
            <Stack spacing={1}>
              <Stack direction="row" spacing={1} alignItems="center">
                <Avatar src={item.fcfAvatarUrl || undefined} sx={{ width: 32, height: 32 }} />
                <Typography variant="subtitle2">{item.fcfAuthorName}</Typography>
                {item.fcfIsOfficer && <Chip size="small" label="Directiva" color="primary" />}
                {item.fcfIsPinned && <Chip size="small" icon={<PushPinIcon />} label="Fijado" color="primary" />}
                <Chip size="small" label={item.fcfKind === 'memory' ? 'Recuerdo' : 'Post'} variant="outlined" />
                <Box flexGrow={1} />
                {isOfficer && (
                  <Tooltip title={item.fcfIsHidden ? 'Mostrar' : 'Ocultar'}>
                    <IconButton
                      size="small"
                      aria-label={item.fcfIsHidden ? 'Mostrar elemento del feed' : 'Ocultar elemento del feed'}
                      onClick={() => hideMut.mutate({ itemId: item.fcfId, kind: item.fcfKind, hide: !item.fcfIsHidden })}
                    >
                      {item.fcfIsHidden ? <VisibilityIcon fontSize="small" /> : <VisibilityOffIcon fontSize="small" />}
                    </IconButton>
                  </Tooltip>
                )}
              </Stack>
              {item.fcfTitle && <Typography variant="h6">{item.fcfTitle}</Typography>}
              <Typography variant="body1">{item.fcfContent}</Typography>
              {item.fcfMediaUrls.length > 0 && (
                <ImageList cols={3} gap={8} sx={{ maxHeight: 300 }}>
                  {item.fcfMediaUrls.map((url, idx) => (
                    <ImageListItem key={idx}>
                      <img src={url} alt={`Media ${idx}`} loading="lazy" style={{ maxHeight: 150, objectFit: 'cover' }} />
                    </ImageListItem>
                  ))}
                </ImageList>
              )}
              <Typography variant="caption" color="text.secondary">
                {new Date(item.fcfCreatedAt).toLocaleString()}
              </Typography>
            </Stack>
          </CardContent>
        </Card>
      ))}
    </Stack>
  );
}

function ClubForum({ artistId, posts, isOfficer, loading }: { artistId: number; posts: FanClubPostDTO[]; isOfficer: boolean; loading: boolean }) {
  const qc = useQueryClient();
  const [open, setOpen] = useState(false);
  const [title, setTitle] = useState('');
  const [content, setContent] = useState('');

  const createPost = useMutation({
    mutationFn: () => Fans.createClubPost(artistId, { fcpReqTitle: title || null, fcpReqContent: content, fcpReqParentId: null }),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['fan-club-posts', artistId] });
      qc.invalidateQueries({ queryKey: ['fan-club-feed', artistId] });
      setOpen(false);
      setTitle('');
      setContent('');
    },
  });

  const pinMut = useMutation({
    mutationFn: ({ postId, pin }: { postId: number; pin: boolean }) =>
      pin ? Fans.pinClubPost(artistId, postId) : Fans.unpinClubPost(artistId, postId),
    onSuccess: () => qc.invalidateQueries({ queryKey: ['fan-club-posts', artistId] }),
  });

  const hideMut = useMutation({
    mutationFn: ({ postId, hide }: { postId: number; hide: boolean }) =>
      hide ? Fans.hideClubPost(artistId, postId) : Fans.unhideClubPost(artistId, postId),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['fan-club-posts', artistId] });
      qc.invalidateQueries({ queryKey: ['fan-club-feed', artistId] });
    },
  });

  if (loading) return <SkeletonCards count={3} />;

  const visiblePosts = posts.filter(p => !p.fcpIsHidden);

  return (
    <Stack spacing={2}>
      <Stack direction="row" justifyContent="flex-end">
        <Button variant="contained" startIcon={<AddIcon />} onClick={() => setOpen(true)}>
          Nuevo post
        </Button>
      </Stack>

      {visiblePosts.length === 0 && (
        <EmptyState icon={<ForumIcon fontSize="large" />} title="Foro vacío" description="Sé el primero en publicar." />
      )}

      {visiblePosts.map(post => (
        <Card key={post.fcpId} sx={{ opacity: post.fcpIsHidden ? 0.5 : 1 }}>
          <CardContent>
            <Stack spacing={1}>
              <Stack direction="row" spacing={1} alignItems="center">
                <Avatar src={post.fcpAvatarUrl || undefined} sx={{ width: 32, height: 32 }} />
                <Typography variant="subtitle2">{post.fcpAuthorName}</Typography>
                {post.fcpIsPinned && <Chip size="small" icon={<PushPinIcon />} label="Fijado" color="primary" />}
                <Box flexGrow={1} />
                {isOfficer && (
                  <>
                    <Tooltip title={post.fcpIsPinned ? 'Desfijar' : 'Fijar'}>
                      <IconButton
                        size="small"
                        aria-label={post.fcpIsPinned ? 'Desfijar post' : 'Fijar post'}
                        onClick={() => pinMut.mutate({ postId: post.fcpId, pin: !post.fcpIsPinned })}
                      >
                        {post.fcpIsPinned ? <PushPinOutlinedIcon fontSize="small" /> : <PushPinIcon fontSize="small" />}
                      </IconButton>
                    </Tooltip>
                    <Tooltip title={post.fcpIsHidden ? 'Mostrar' : 'Ocultar'}>
                      <IconButton
                        size="small"
                        aria-label={post.fcpIsHidden ? 'Mostrar post' : 'Ocultar post'}
                        onClick={() => hideMut.mutate({ postId: post.fcpId, hide: !post.fcpIsHidden })}
                      >
                        {post.fcpIsHidden ? <VisibilityIcon fontSize="small" /> : <VisibilityOffIcon fontSize="small" />}
                      </IconButton>
                    </Tooltip>
                  </>
                )}
              </Stack>
              {post.fcpTitle && <Typography variant="h6">{post.fcpTitle}</Typography>}
              <Typography variant="body1">{post.fcpContent}</Typography>
              <Typography variant="caption" color="text.secondary">
                {new Date(post.fcpCreatedAt).toLocaleString()}
              </Typography>
            </Stack>
          </CardContent>
        </Card>
      ))}

      <Dialog open={open} onClose={() => setOpen(false)} fullWidth maxWidth="sm">
        <DialogTitle>Nuevo post</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            <TextField label="Título (opcional)" value={title} onChange={e => setTitle(e.target.value)} fullWidth />
            <TextField label="Contenido" value={content} onChange={e => setContent(e.target.value)} multiline rows={4} fullWidth />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setOpen(false)}>Cancelar</Button>
          <Button variant="contained" onClick={() => createPost.mutate()} disabled={!content.trim() || createPost.isPending}>
            Publicar
          </Button>
        </DialogActions>
      </Dialog>
    </Stack>
  );
}

function ClubMemories({ artistId, memories, isOfficer, loading }: { artistId: number; memories: FanClubMemoryDTO[]; isOfficer: boolean; loading: boolean }) {
  const qc = useQueryClient();
  const [open, setOpen] = useState(false);
  const [title, setTitle] = useState('');
  const [description, setDescription] = useState('');
  const [mediaUrls, setMediaUrls] = useState('');
  const [reportOpen, setReportOpen] = useState<number | null>(null);
  const [reportReason, setReportReason] = useState('');

  const createMemory = useMutation({
    mutationFn: () => Fans.createClubMemory(artistId, {
      fcmReqTitle: title,
      fcmReqDescription: description || null,
      fcmReqMediaUrls: mediaUrls.split('\n').filter(u => u.trim()),
    }),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['fan-club-memories', artistId] });
      qc.invalidateQueries({ queryKey: ['fan-club-feed', artistId] });
      setOpen(false);
      setTitle('');
      setDescription('');
      setMediaUrls('');
    },
  });

  const hideMut = useMutation({
    mutationFn: ({ memoryId, hide }: { memoryId: number; hide: boolean }) =>
      hide ? Fans.hideClubMemory(artistId, memoryId) : Fans.unhideClubMemory(artistId, memoryId),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['fan-club-memories', artistId] });
      qc.invalidateQueries({ queryKey: ['fan-club-feed', artistId] });
    },
  });

  const deleteMut = useMutation({
    mutationFn: (memoryId: number) => Fans.deleteClubMemory(artistId, memoryId),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['fan-club-memories', artistId] });
      qc.invalidateQueries({ queryKey: ['fan-club-feed', artistId] });
    },
  });

  const reportMut = useMutation({
    mutationFn: ({ memoryId, reason }: { memoryId: number; reason: string }) =>
      Fans.reportClubMemory(artistId, memoryId, { fcmrReqReason: reason }),
    onSuccess: () => {
      setReportOpen(null);
      setReportReason('');
    },
  });

  if (loading) return <SkeletonCards count={3} />;

  const visibleMemories = memories.filter(m => !m.fcmIsHidden && !m.fcmIsDeleted);

  return (
    <Stack spacing={2}>
      <Stack direction="row" justifyContent="flex-end">
        <Button variant="contained" startIcon={<AddIcon />} onClick={() => setOpen(true)}>
          Nuevo recuerdo
        </Button>
      </Stack>

      {visibleMemories.length === 0 && (
        <EmptyState icon={<PhotoLibraryIcon fontSize="large" />} title="Sin recuerdos" description="Comparte fotos o videos de conciertos y momentos con el artista." />
      )}

      {visibleMemories.map(memory => (
        <Card key={memory.fcmId}>
          <CardContent>
            <Stack spacing={1}>
              <Stack direction="row" spacing={1} alignItems="center">
                <Avatar src={memory.fcmMemberAvatarUrl || undefined} sx={{ width: 32, height: 32 }} />
                <Typography variant="subtitle2">{memory.fcmMemberName}</Typography>
                <Box flexGrow={1} />
                {isOfficer && (
                  <>
                    <Tooltip title={memory.fcmIsHidden ? 'Mostrar' : 'Ocultar'}>
                      <IconButton
                        size="small"
                        aria-label={memory.fcmIsHidden ? 'Mostrar recuerdo' : 'Ocultar recuerdo'}
                        onClick={() => hideMut.mutate({ memoryId: memory.fcmId, hide: !memory.fcmIsHidden })}
                      >
                        {memory.fcmIsHidden ? <VisibilityIcon fontSize="small" /> : <VisibilityOffIcon fontSize="small" />}
                      </IconButton>
                    </Tooltip>
                    <Tooltip title="Eliminar">
                      <IconButton
                        size="small"
                        aria-label="Eliminar recuerdo"
                        onClick={() => deleteMut.mutate(memory.fcmId)}
                      >
                        <VisibilityOffIcon fontSize="small" />
                      </IconButton>
                    </Tooltip>
                  </>
                )}
                <Tooltip title="Reportar">
                  <IconButton
                    size="small"
                    aria-label="Reportar recuerdo"
                    onClick={() => setReportOpen(memory.fcmId)}
                  >
                    <ReportIcon fontSize="small" />
                  </IconButton>
                </Tooltip>
              </Stack>
              <Typography variant="h6">{memory.fcmTitle}</Typography>
              {memory.fcmDescription && <Typography variant="body1">{memory.fcmDescription}</Typography>}
              {memory.fcmMediaUrls.length > 0 && (
                <ImageList cols={3} gap={8} sx={{ maxHeight: 300 }}>
                  {memory.fcmMediaUrls.map((url, idx) => (
                    <ImageListItem key={idx}>
                      <img src={url} alt={`Memory ${idx}`} loading="lazy" style={{ maxHeight: 150, objectFit: 'cover' }} />
                    </ImageListItem>
                  ))}
                </ImageList>
              )}
              <Typography variant="caption" color="text.secondary">
                {new Date(memory.fcmCreatedAt).toLocaleString()}
              </Typography>
            </Stack>
          </CardContent>
        </Card>
      ))}

      <Dialog open={open} onClose={() => setOpen(false)} fullWidth maxWidth="sm">
        <DialogTitle>Nuevo recuerdo</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            <TextField label="Título" value={title} onChange={e => setTitle(e.target.value)} fullWidth required />
            <TextField label="Descripción" value={description} onChange={e => setDescription(e.target.value)} multiline rows={3} fullWidth />
            <TextField
              label="URLs de fotos/videos (una por línea)"
              value={mediaUrls}
              onChange={e => setMediaUrls(e.target.value)}
              multiline
              rows={4}
              fullWidth
              placeholder="https://example.com/photo1.jpg&#10;https://example.com/photo2.jpg"
            />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setOpen(false)}>Cancelar</Button>
          <Button variant="contained" onClick={() => createMemory.mutate()} disabled={!title.trim() || createMemory.isPending}>
            Publicar
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog open={reportOpen !== null} onClose={() => setReportOpen(null)} fullWidth maxWidth="sm">
        <DialogTitle>Reportar recuerdo</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            <TextField
              label="Motivo del reporte"
              value={reportReason}
              onChange={e => setReportReason(e.target.value)}
              multiline
              rows={3}
              fullWidth
              required
            />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setReportOpen(null)}>Cancelar</Button>
          <Button
            variant="contained"
            onClick={() => reportOpen !== null && reportMut.mutate({ memoryId: reportOpen, reason: reportReason })}
            disabled={!reportReason.trim() || reportMut.isPending}
          >
            Reportar
          </Button>
        </DialogActions>
      </Dialog>
    </Stack>
  );
}

function ClubCalendar({ artistId, events, isOfficer, loading }: { artistId: number; events: FanClubEventDTO[]; isOfficer: boolean; loading: boolean }) {
  const qc = useQueryClient();
  const [open, setOpen] = useState(false);
  const [form, setForm] = useState({ title: '', description: '', startsAt: '', endsAt: '', location: '' });

  const createEvent = useMutation({
    mutationFn: () => Fans.createClubEvent(artistId, {
      fcevTitle: form.title,
      fcevDescription: form.description || null,
      fcevStartsAt: form.startsAt || null,
      fcevEndsAt: form.endsAt || null,
      fcevLocation: form.location || null,
    }),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['fan-club-events', artistId] });
      setOpen(false);
      setForm({ title: '', description: '', startsAt: '', endsAt: '', location: '' });
    },
  });

  if (loading) return <SkeletonCards count={3} />;

  return (
    <Stack spacing={2}>
      {isOfficer && (
        <Stack direction="row" justifyContent="flex-end">
          <Button variant="contained" startIcon={<AddIcon />} onClick={() => setOpen(true)}>
            Crear evento
          </Button>
        </Stack>
      )}

      {events.length === 0 && (
        <EmptyState icon={<CalendarMonthIcon fontSize="large" />} title="Sin eventos" description="No hay eventos programados aún." />
      )}

      <Grid container spacing={2}>
        {events.map(ev => (
          <Grid item xs={12} md={6} key={ev.fceId}>
            <Card>
              <CardContent>
                <Stack spacing={1}>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <Typography variant="h6">{ev.fceTitle}</Typography>
                    {ev.fceIsArtistConcert && <Chip size="small" label="Concierto" color="secondary" />}
                  </Stack>
                  {ev.fceDescription && <Typography variant="body2" color="text.secondary">{ev.fceDescription}</Typography>}
                  {ev.fceStartsAt && (
                    <Typography variant="body2">
                      📅 {new Date(ev.fceStartsAt).toLocaleString()}
                      {ev.fceEndsAt && ` — ${new Date(ev.fceEndsAt).toLocaleString()}`}
                    </Typography>
                  )}
                  {ev.fceLocation && <Typography variant="body2">📍 {ev.fceLocation}</Typography>}
                </Stack>
              </CardContent>
            </Card>
          </Grid>
        ))}
      </Grid>

      <Dialog open={open} onClose={() => setOpen(false)} fullWidth maxWidth="sm">
        <DialogTitle>Crear evento</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            <TextField label="Título" value={form.title} onChange={e => setForm(f => ({ ...f, title: e.target.value }))} fullWidth required />
            <TextField label="Descripción" value={form.description} onChange={e => setForm(f => ({ ...f, description: e.target.value }))} multiline rows={2} fullWidth />
            <TextField label="Inicio" type="datetime-local" value={form.startsAt} onChange={e => setForm(f => ({ ...f, startsAt: e.target.value }))} fullWidth InputLabelProps={{ shrink: true }} />
            <TextField label="Fin" type="datetime-local" value={form.endsAt} onChange={e => setForm(f => ({ ...f, endsAt: e.target.value }))} fullWidth InputLabelProps={{ shrink: true }} />
            <TextField label="Ubicación" value={form.location} onChange={e => setForm(f => ({ ...f, location: e.target.value }))} fullWidth />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setOpen(false)}>Cancelar</Button>
          <Button variant="contained" onClick={() => createEvent.mutate()} disabled={!form.title.trim() || createEvent.isPending}>
            Crear
          </Button>
        </DialogActions>
      </Dialog>
    </Stack>
  );
}

function ClubElections({ artistId, elections }: { artistId: number; elections: FanClubElectionDTO[] }) {
  const qc = useQueryClient();
  const [open, setOpen] = useState(false);
  const [form, setForm] = useState({ year: new Date().getFullYear(), candidacyStartsAt: '', candidacyEndsAt: '', votingStartsAt: '', votingEndsAt: '' });
  const [voteOpen, setVoteOpen] = useState<number | null>(null);
  const [selectedCands, setSelectedCands] = useState<number[]>([]);

  const createElection = useMutation({
    mutationFn: () => Fans.createClubElection(artistId, {
      fcelYear: form.year,
      fcelCandidacyStartsAt: form.candidacyStartsAt || null,
      fcelCandidacyEndsAt: form.candidacyEndsAt || null,
      fcelVotingStartsAt: form.votingStartsAt || null,
      fcelVotingEndsAt: form.votingEndsAt || null,
    }),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['fan-club-elections', artistId] });
      setOpen(false);
    },
  });

  const castVote = useMutation({
    mutationFn: (electionId: number) => Fans.castVote(artistId, electionId, { fcvCandidacyIds: selectedCands }),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['fan-club-elections', artistId] });
      setVoteOpen(null);
      setSelectedCands([]);
    },
  });

  return (
    <Stack spacing={3}>
      <Stack direction="row" justifyContent="flex-end">
        <Button variant="contained" startIcon={<AddIcon />} onClick={() => setOpen(true)}>
          Nueva elección
        </Button>
      </Stack>

      {elections.length === 0 && (
        <EmptyState icon={<HowToVoteIcon fontSize="large" />} title="Sin elecciones" description="Aún no hay elecciones programadas." />
      )}

      {elections.map(el => (
        <Card key={el.fceElectionId}>
          <CardContent>
            <Stack spacing={2}>
              <Stack direction="row" justifyContent="space-between" alignItems="center">
                <Typography variant="h6">Elección {el.fceYear}</Typography>
                <Chip label={el.fceStatus} color="primary" variant="outlined" />
              </Stack>
              <Divider />
              <Typography variant="subtitle2">Mis candidaturas</Typography>
              {el.fceMyCandidacies.length === 0 ? (
                <Typography variant="body2" color="text.secondary">No te has postulado.</Typography>
              ) : (
                <Stack direction="row" spacing={1} flexWrap="wrap">
                  {el.fceMyCandidacies.map(c => (
                    <Chip key={c.fccCandidacyId} label={`${c.fccRole} — ${c.fccFanName}`} />
                  ))}
                </Stack>
              )}
              <Typography variant="subtitle2">Mis votos</Typography>
              {el.fceMyVotes.length === 0 ? (
                <Button size="small" variant="outlined" onClick={() => { setVoteOpen(el.fceElectionId); setSelectedCands([]); }}>
                  Votar
                </Button>
              ) : (
                <Stack direction="row" spacing={1} flexWrap="wrap">
                  {el.fceMyVotes.map(v => (
                    <Chip key={v.fcvCandidacyId} label={v.fcvRole} color="success" variant="outlined" />
                  ))}
                </Stack>
              )}
            </Stack>
          </CardContent>
        </Card>
      ))}

      <Dialog open={open} onClose={() => setOpen(false)} fullWidth maxWidth="sm">
        <DialogTitle>Nueva elección</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            <TextField label="Año" type="number" value={form.year} onChange={e => setForm(f => ({ ...f, year: parseInt(e.target.value) }))} fullWidth />
            <TextField label="Inicio candidaturas" type="datetime-local" value={form.candidacyStartsAt} onChange={e => setForm(f => ({ ...f, candidacyStartsAt: e.target.value }))} fullWidth InputLabelProps={{ shrink: true }} />
            <TextField label="Fin candidaturas" type="datetime-local" value={form.candidacyEndsAt} onChange={e => setForm(f => ({ ...f, candidacyEndsAt: e.target.value }))} fullWidth InputLabelProps={{ shrink: true }} />
            <TextField label="Inicio votación" type="datetime-local" value={form.votingStartsAt} onChange={e => setForm(f => ({ ...f, votingStartsAt: e.target.value }))} fullWidth InputLabelProps={{ shrink: true }} />
            <TextField label="Fin votación" type="datetime-local" value={form.votingEndsAt} onChange={e => setForm(f => ({ ...f, votingEndsAt: e.target.value }))} fullWidth InputLabelProps={{ shrink: true }} />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setOpen(false)}>Cancelar</Button>
          <Button variant="contained" onClick={() => createElection.mutate()} disabled={createElection.isPending}>
            Crear
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog open={voteOpen !== null} onClose={() => setVoteOpen(null)} fullWidth maxWidth="sm">
        <DialogTitle>Votar</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            {voteOpen !== null && elections.find(e => e.fceElectionId === voteOpen)?.fceMyCandidacies.length === 0 && (
              <Typography variant="body2" color="text.secondary">No hay candidatos disponibles para votar.</Typography>
            )}
            {voteOpen !== null && elections.find(e => e.fceElectionId === voteOpen)?.fceMyCandidacies.map(c => (
              <Card key={c.fccCandidacyId} variant="outlined" sx={{ cursor: 'pointer', borderColor: selectedCands.includes(c.fccCandidacyId) ? 'primary.main' : 'divider' }} onClick={() => {
                setSelectedCands(prev => prev.includes(c.fccCandidacyId) ? prev.filter(id => id !== c.fccCandidacyId) : [...prev, c.fccCandidacyId]);
              }}>
                <CardContent>
                  <Stack direction="row" spacing={2} alignItems="center">
                    <Avatar src={c.fccAvatarUrl || undefined} />
                    <Box>
                      <Typography variant="subtitle2">{c.fccFanName}</Typography>
                      <Typography variant="caption">{c.fccRole}</Typography>
                    </Box>
                  </Stack>
                </CardContent>
              </Card>
            ))}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setVoteOpen(null)}>Cancelar</Button>
          <Button variant="contained" disabled={selectedCands.length === 0 || castVote.isPending} onClick={() => voteOpen !== null && castVote.mutate(voteOpen)}>
            Confirmar voto
          </Button>
        </DialogActions>
      </Dialog>
    </Stack>
  );
}
