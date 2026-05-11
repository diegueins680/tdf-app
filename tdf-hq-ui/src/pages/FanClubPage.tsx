import { useState } from 'react';
import { useParams } from 'react-router-dom';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Box, Button, Card, CardContent, Chip, Dialog, DialogActions, DialogContent, DialogTitle,
  Divider, Grid, IconButton, Stack, Tab, Tabs, TextField, Typography,
  Avatar, Tooltip,
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
} from '@mui/icons-material';
import PageShell, { EmptyState, SkeletonCards } from '../components/PageShell';
import DataTable from '../components/DataTable';
import { Fans } from '../api/fans';
import { useSession } from '../session/SessionContext';
import type { FanClubPostDTO, FanClubEventDTO, FanClubElectionDTO, FanClubCandidacyDTO } from '../api/types';

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

  const postsQuery = useQuery({
    queryKey: ['fan-club-posts', artistIdNum],
    queryFn: () => Fans.listClubPosts(artistIdNum),
    enabled: artistIdNum > 0 && tab === 1,
  });

  const eventsQuery = useQuery({
    queryKey: ['fan-club-events', artistIdNum],
    queryFn: () => Fans.listClubEvents(artistIdNum),
    enabled: artistIdNum > 0 && tab === 2,
  });

  const electionsQuery = useQuery({
    queryKey: ['fan-club-elections', artistIdNum],
    queryFn: () => Fans.listClubElections(artistIdNum),
    enabled: artistIdNum > 0 && tab === 3,
  });

  const club = clubQuery.data;
  const isOfficer = club?.fcOfficers.some(o => o.fcoPartyId === session?.sessionPartyId) ?? false;

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
                <Typography variant="h5" fontWeight={600}>{club.fcName}</Typography>
                {club.fcDescription && (
                  <Typography variant="body1" color="text.secondary">{club.fcDescription}</Typography>
                )}
                <Divider />
                <Typography variant="subtitle2" fontWeight={600}>Directiva</Typography>
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
              <Tab icon={<ForumIcon />} label="Foro" />
              <Tab icon={<CalendarMonthIcon />} label="Calendario" />
              <Tab icon={<HowToVoteIcon />} label="Elecciones" />
            </Tabs>
          </Box>

          {tab === 0 && <ClubForum artistId={artistIdNum} posts={postsQuery.data ?? []} isOfficer={isOfficer} loading={postsQuery.isLoading} />}
          {tab === 1 && <ClubCalendar artistId={artistIdNum} events={eventsQuery.data ?? []} isOfficer={isOfficer} loading={eventsQuery.isLoading} />}
          {tab === 2 && <ClubElections artistId={artistIdNum} elections={electionsQuery.data ?? []} />}
        </Stack>
      )}
    </PageShell>
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
    onSuccess: () => qc.invalidateQueries({ queryKey: ['fan-club-posts', artistId] }),
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
                      <IconButton size="small" onClick={() => pinMut.mutate({ postId: post.fcpId, pin: !post.fcpIsPinned })}>
                        {post.fcpIsPinned ? <PushPinOutlinedIcon fontSize="small" /> : <PushPinIcon fontSize="small" />}
                      </IconButton>
                    </Tooltip>
                    <Tooltip title={post.fcpIsHidden ? 'Mostrar' : 'Ocultar'}>
                      <IconButton size="small" onClick={() => hideMut.mutate({ postId: post.fcpId, hide: !post.fcpIsHidden })}>
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
          <Grid size={{ xs: 12, md: 6 }} key={ev.fceId}>
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
