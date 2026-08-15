import {
  Alert,
  Box,
  Button,
  Card,
  CardActions,
  CardContent,
  Chip,
  CircularProgress,
  Container,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControl,
  FormControlLabel,
  InputLabel,
  MenuItem,
  Paper,
  Select,
  Snackbar,
  Stack,
  Switch,
  Tab,
  Tabs,
  TextField,
  Typography,
} from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import PublishIcon from '@mui/icons-material/Publish';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState, type FormEvent } from 'react';
import { Link as RouterLink, useSearchParams } from 'react-router-dom';

import { Directory, type ManagedClassified, type ManagedDirectoryProfile } from '../api/directory';

const slugify = (value: string) => value.toLowerCase().normalize('NFD').replace(/[\u0300-\u036f]/g, '').replace(/[^a-z0-9]+/g, '-').replace(/(^-|-$)/g, '').slice(0, 120);

export default function DirectoryManagePage() {
  const client = useQueryClient();
  const [params, setParams] = useSearchParams();
  const [tab, setTab] = useState(params.has('apply') || params.has('contact') ? 2 : 0);
  const [profileOpen, setProfileOpen] = useState(false);
  const [classifiedOpen, setClassifiedOpen] = useState(false);
  const [notice, setNotice] = useState<string | null>(null);
  const contextParam = params.get('contextKind');
  const contextKind = contextParam === 'classified' || contextParam === 'application' || contextParam === 'invitation' ? contextParam : 'profile';
  const profiles = useQuery({ queryKey: ['directory', 'managed-profiles'], queryFn: Directory.managedProfiles });
  const classifieds = useQuery({ queryKey: ['directory', 'managed-classifieds'], queryFn: Directory.managedClassifieds });
  const taxonomies = useQuery({ queryKey: ['directory', 'taxonomies', 'es'], queryFn: () => Directory.taxonomies('es'), staleTime: 30 * 60 * 1000 });
  const refresh = () => Promise.all([
    client.invalidateQueries({ queryKey: ['directory', 'managed-profiles'] }),
    client.invalidateQueries({ queryKey: ['directory', 'managed-classifieds'] }),
  ]);
  const ageMutation = useMutation({
    mutationFn: () => Directory.setAgeAssurance(true),
    onSuccess: () => setNotice('Registramos tu declaración de mayoría de edad. No equivale a una verificación de identidad.'),
  });

  return (
    <Container component="main" id="main-content" maxWidth="xl" sx={{ py: 4 }}>
      <Stack spacing={3}>
        <Box>
          <Typography component="h1" variant="h3" fontWeight={900}>Mis perfiles y clasificados</Typography>
          <Typography color="text.secondary" mt={1}>Administra únicamente los perfiles para los que tienes una autorización explícita.</Typography>
        </Box>
        <Alert severity="info" action={<Button color="inherit" onClick={() => ageMutation.mutate()} disabled={ageMutation.isPending}>Soy mayor de edad</Button>}>
          Para publicar, responder o contactar se requiere control de edad. La declaración de adulto no se muestra como identidad verificada; las cuentas de menores requieren consentimiento aprobado de su representante.
        </Alert>
        <Tabs value={tab} onChange={(_, value: unknown) => { if (typeof value === 'number') setTab(value); }} aria-label="Administración del directorio">
          <Tab label={`Perfiles (${profiles.data?.length ?? 0})`} />
          <Tab label={`Clasificados (${classifieds.data?.length ?? 0})`} />
          <Tab label="Responder o contactar" />
        </Tabs>

        {tab === 0 && <ProfilePanel profiles={profiles.data ?? []} loading={profiles.isLoading} onCreate={() => setProfileOpen(true)} onRefresh={refresh} />}
        {tab === 1 && <ClassifiedPanel classifieds={classifieds.data ?? []} loading={classifieds.isLoading} onCreate={() => setClassifiedOpen(true)} onRefresh={refresh} />}
        {tab === 2 && <OpportunityActionPanel profiles={profiles.data ?? []} applyId={params.get('apply')} contactId={params.get('contact')} contextKind={contextKind} onDone={() => { setNotice('Acción enviada de forma segura.'); setParams({}); }} />}

        <ProfileDialog open={profileOpen} onClose={() => setProfileOpen(false)} taxonomies={taxonomies.data} onCreated={() => { setProfileOpen(false); void refresh(); setNotice('Perfil creado como borrador.'); }} />
        <ClassifiedDialog open={classifiedOpen} onClose={() => setClassifiedOpen(false)} profiles={profiles.data ?? []} taxonomies={taxonomies.data} onCreated={() => { setClassifiedOpen(false); void refresh(); setNotice('Clasificado creado como borrador. Revísalo y publícalo cuando esté listo.'); }} />
        <Snackbar open={Boolean(notice)} autoHideDuration={6000} onClose={() => setNotice(null)} message={notice} />
      </Stack>
    </Container>
  );
}

function ProfilePanel({ profiles, loading, onCreate, onRefresh }: { profiles: ManagedDirectoryProfile[]; loading: boolean; onCreate: () => void; onRefresh: () => Promise<unknown> }) {
  const status = useMutation({ mutationFn: ({ id, value }: { id: string; value: string }) => Directory.setProfileStatus(id, value), onSuccess: onRefresh });
  if (loading) return <CircularProgress />;
  return <Stack spacing={2}>
    <Button variant="contained" startIcon={<AddIcon />} onClick={onCreate} sx={{ alignSelf: 'flex-start' }}>Crear perfil</Button>
    {profiles.length === 0 && <Alert severity="info">Crea tu primer perfil profesional. Una misma cuenta puede administrar varios perfiles autorizados.</Alert>}
    <Box sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', md: 'repeat(2,minmax(0,1fr))' }, gap: 2 }}>
      {profiles.map((profile) => <Card key={profile.id} variant="outlined"><CardContent><Stack direction="row" justifyContent="space-between" gap={2}><Box><Typography variant="h5" fontWeight={800}>{profile.name}</Typography><Typography color="text.secondary">{profile.kind} · /directorio/{profile.slug}</Typography></Box><Chip label={profile.status} color={profile.status === 'published' ? 'success' : 'default'} /></Stack></CardContent><CardActions><Button component={RouterLink} to={`/directorio/${profile.slug}`}>Vista pública</Button>{profile.status !== 'published' && profile.capabilities['publish'] && <Button startIcon={<PublishIcon />} onClick={() => status.mutate({ id: profile.id, value: 'published' })}>Publicar</Button>}{profile.status === 'published' && <Button onClick={() => status.mutate({ id: profile.id, value: 'paused' })}>Pausar</Button>}</CardActions></Card>)}
    </Box>
  </Stack>;
}

function ClassifiedPanel({ classifieds, loading, onCreate, onRefresh }: { classifieds: ManagedClassified[]; loading: boolean; onCreate: () => void; onRefresh: () => Promise<unknown> }) {
  const [selected, setSelected] = useState<string | null>(null);
  const applications = useQuery({ queryKey: ['directory', 'applications', selected], queryFn: () => Directory.applications(selected!), enabled: Boolean(selected) });
  const status = useMutation({ mutationFn: ({ id, value }: { id: string; value: string }) => Directory.setClassifiedStatus(id, value), onSuccess: onRefresh });
  if (loading) return <CircularProgress />;
  return <Stack spacing={2}>
    <Button variant="contained" startIcon={<AddIcon />} onClick={onCreate} sx={{ alignSelf: 'flex-start' }}>Publicar oportunidad</Button>
    {classifieds.length === 0 && <Alert severity="info">Todavía no tienes clasificados. Los anuncios básicos son gratuitos.</Alert>}
    {classifieds.map((item) => <Paper key={item.id} variant="outlined" sx={{ p: 2.5, borderRadius: 3 }}><Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" gap={2}><Box><Typography variant="h5" fontWeight={800}>{item.title}</Typography><Typography color="text.secondary">Vence: {item.expiresAt ? new Date(item.expiresAt).toLocaleDateString() : 'al publicar +30 días'}</Typography></Box><Stack direction="row" gap={1} alignItems="center" flexWrap="wrap"><Chip label={item.status} color={item.status === 'published' ? 'success' : 'default'} />{item.status === 'draft' && <Button onClick={() => status.mutate({ id: item.id, value: 'published' })}>Publicar</Button>}{item.status === 'published' && <Button startIcon={<CheckCircleIcon />} onClick={() => status.mutate({ id: item.id, value: 'filled' })}>Marcar cubierto</Button>}<Button onClick={() => setSelected(selected === item.id ? null : item.id)}>Postulaciones</Button></Stack></Stack>{selected === item.id && <Stack mt={2} spacing={1}>{applications.isLoading && <CircularProgress size={24} />}{applications.data?.length === 0 && <Typography color="text.secondary">Sin postulaciones todavía.</Typography>}{applications.data?.map((application) => <ApplicationRow key={String(application['id'])} application={application} />)}</Stack>}</Paper>)}
  </Stack>;
}

function ApplicationRow({ application }: { application: Record<string, unknown> }) {
  const profile = typeof application['applicantProfile'] === 'object' && application['applicantProfile'] ? application['applicantProfile'] as Record<string, unknown> : {};
  const mutation = useMutation({ mutationFn: (status: string) => Directory.setApplicationStatus(String(application['id']), status) });
  const profileName = typeof profile['name'] === 'string' ? profile['name'] : 'Perfil postulante';
  const message = typeof application['message'] === 'string' ? application['message'] : '';
  const applicationStatus = typeof application['status'] === 'string' ? application['status'] : 'submitted';
  return <Paper sx={{ p: 2, bgcolor: 'action.hover' }}><Typography fontWeight={800}>{profileName}</Typography><Typography sx={{ whiteSpace: 'pre-wrap' }}>{message}</Typography><Stack direction="row" gap={1} mt={1}><Chip size="small" label={applicationStatus} /><Button size="small" onClick={() => mutation.mutate('shortlisted')}>Preseleccionar</Button><Button size="small" onClick={() => mutation.mutate('rejected')}>Rechazar</Button><Button size="small" onClick={() => mutation.mutate('conversation_open')}>Conversar</Button></Stack></Paper>;
}

function ProfileDialog({ open, onClose, taxonomies, onCreated }: { open: boolean; onClose: () => void; taxonomies?: Awaited<ReturnType<typeof Directory.taxonomies>>; onCreated: () => void }) {
  const [name, setName] = useState('');
  const [kind, setKind] = useState('person');
  const [bio, setBio] = useState('');
  const [professions, setProfessions] = useState<string[]>([]);
  const [cityId, setCityId] = useState('');
  const [remote, setRemote] = useState(false);
  const mutation = useMutation({ mutationFn: () => {
    const city = taxonomies?.cities.find((item) => item.id === cityId);
    if (!city?.countryId) throw new Error('Selecciona una ciudad.');
    return Directory.createProfile({ profileKind: kind, publicName: name.trim(), slug: slugify(name), bio: bio.trim() || undefined, professionIds: professions, instrumentIds: [], genreIds: [], serviceOfferingIds: [], countryId: city.countryId, cityId, onsite: true, remote, availableToTravel: false });
  }, onSuccess: onCreated });
  return <Dialog open={open} onClose={onClose} fullWidth maxWidth="sm"><DialogTitle>Crear perfil público</DialogTitle><DialogContent><Stack spacing={2} pt={1}><TextField label="Nombre público" value={name} onChange={(event) => setName(event.target.value)} required inputProps={{ maxLength: 160 }} /><FormControl><InputLabel>Tipo</InputLabel><Select label="Tipo" value={kind} onChange={(event) => setKind(event.target.value)}>{[['person','Persona'],['artist','Artista'],['band','Banda'],['project','Proyecto'],['organization','Organización'],['venue','Venue'],['studio','Estudio'],['agency','Agencia'],['label','Sello'],['school','Escuela']].map(([value,label]) => <MenuItem key={value} value={value}>{label}</MenuItem>)}</Select></FormControl><TextField label="Biografía" multiline minRows={4} value={bio} onChange={(event) => setBio(event.target.value)} /><FormControl><InputLabel>Profesiones</InputLabel><Select multiple label="Profesiones" value={professions} onChange={(event) => setProfessions(typeof event.target.value === 'string' ? event.target.value.split(',') : event.target.value)}>{(taxonomies?.professions ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</Select></FormControl><FormControl><InputLabel>Ciudad principal</InputLabel><Select label="Ciudad principal" value={cityId} onChange={(event) => setCityId(event.target.value)}>{(taxonomies?.cities ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</Select></FormControl><FormControlLabel control={<Switch checked={remote} onChange={(event) => setRemote(event.target.checked)} />} label="También trabajo de forma remota" />{mutation.error && <Alert severity="error">{mutation.error.message}</Alert>}</Stack></DialogContent><DialogActions><Button onClick={onClose}>Cancelar</Button><Button variant="contained" onClick={() => mutation.mutate()} disabled={!name.trim() || !cityId || mutation.isPending}>Crear borrador</Button></DialogActions></Dialog>;
}

function ClassifiedDialog({ open, onClose, profiles, taxonomies, onCreated }: { open: boolean; onClose: () => void; profiles: ManagedDirectoryProfile[]; taxonomies?: Awaited<ReturnType<typeof Directory.taxonomies>>; onCreated: () => void }) {
  const [profileId, setProfileId] = useState('');
  const [categoryId, setCategoryId] = useState('');
  const [title, setTitle] = useState('');
  const [description, setDescription] = useState('');
  const [cityId, setCityId] = useState('');
  const [remote, setRemote] = useState(false);
  const mutation = useMutation({ mutationFn: () => Directory.createClassified({ authorProfileId: profileId, categoryId, title: title.trim(), slug: slugify(`${title}-${Date.now().toString(36)}`), description: description.trim(), professionIds: [], instrumentIds: [], genreIds: [], countryIds: [], cityIds: cityId ? [cityId] : [], metropolitanAreaIds: [], onsite: true, remote, availableToTravel: false, budgetNegotiable: false }), onSuccess: onCreated });
  return <Dialog open={open} onClose={onClose} fullWidth maxWidth="md"><DialogTitle>Nueva oportunidad o clasificado</DialogTitle><DialogContent><Stack spacing={2} pt={1}><FormControl><InputLabel>Perfil que publica</InputLabel><Select label="Perfil que publica" value={profileId} onChange={(event) => setProfileId(event.target.value)}>{profiles.map((profile) => <MenuItem key={profile.id} value={profile.id}>{profile.name}</MenuItem>)}</Select></FormControl><FormControl><InputLabel>Categoría</InputLabel><Select label="Categoría" value={categoryId} onChange={(event) => setCategoryId(event.target.value)}>{(taxonomies?.classifiedCategories ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</Select></FormControl><TextField label="Título" value={title} onChange={(event) => setTitle(event.target.value)} required inputProps={{ minLength: 5, maxLength: 160 }} /><TextField label="Descripción" value={description} onChange={(event) => setDescription(event.target.value)} multiline minRows={6} required inputProps={{ minLength: 20, maxLength: 10000 }} /><FormControl><InputLabel>Ciudad</InputLabel><Select label="Ciudad" value={cityId} onChange={(event) => setCityId(event.target.value)}><MenuItem value="">Solo remoto o alcance nacional</MenuItem>{(taxonomies?.cities ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</Select></FormControl><FormControlLabel control={<Switch checked={remote} onChange={(event) => setRemote(event.target.checked)} />} label="También acepta respuestas remotas" /><Typography variant="caption" color="text.secondary">El anuncio vence 30 días después de publicarse. Las ofertas reservables deben vincularse al marketplace, no duplicarse aquí.</Typography>{mutation.error && <Alert severity="error">{mutation.error.message}</Alert>}</Stack></DialogContent><DialogActions><Button onClick={onClose}>Cancelar</Button><Button variant="contained" onClick={() => mutation.mutate()} disabled={!profileId || !categoryId || title.trim().length < 5 || description.trim().length < 20 || mutation.isPending}>Guardar borrador</Button></DialogActions></Dialog>;
}

function OpportunityActionPanel({ profiles, applyId, contactId, contextKind, onDone }: { profiles: ManagedDirectoryProfile[]; applyId: string | null; contactId: string | null; contextKind: 'profile' | 'classified' | 'application' | 'invitation'; onDone: () => void }) {
  const [profileId, setProfileId] = useState(profiles[0]?.id ?? '');
  const [message, setMessage] = useState('');
  const mutation = useMutation({ mutationFn: () => {
    if (applyId) return Directory.apply(applyId, { applicantProfileId: profileId, message: message.trim(), portfolio: [] });
    if (contactId) return Directory.contact({ senderProfileId: profileId, targetProfileId: contactId, contextKind, contextId: contactId, message: message.trim() });
    throw new Error('Abre un perfil o clasificado público para iniciar esta acción.');
  }, onSuccess: onDone });
  if (!applyId && !contactId) return <Alert severity="info">Abre un perfil o clasificado desde <Button component={RouterLink} to="/buscar">la búsqueda</Button> para postularte o contactar.</Alert>;
  return <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}><Stack component="form" spacing={2} onSubmit={(event: FormEvent) => { event.preventDefault(); mutation.mutate(); }}><Typography variant="h5" fontWeight={800}>{applyId ? 'Enviar postulación' : 'Iniciar conversación'}</Typography><FormControl><InputLabel>Actuar como perfil</InputLabel><Select label="Actuar como perfil" value={profileId} onChange={(event) => setProfileId(event.target.value)}>{profiles.map((profile) => <MenuItem key={profile.id} value={profile.id}>{profile.name}</MenuItem>)}</Select></FormControl><TextField label="Mensaje" multiline minRows={5} value={message} onChange={(event) => setMessage(event.target.value)} required inputProps={{ minLength: 10, maxLength: 5000 }} helperText="No incluyas teléfono ni correo si aún no quieres compartirlos." />{mutation.error && <Alert severity="error">{mutation.error.message}</Alert>}<Button type="submit" variant="contained" disabled={!profileId || message.trim().length < 10 || mutation.isPending}>{mutation.isPending ? 'Enviando…' : applyId ? 'Postularme' : 'Abrir conversación'}</Button></Stack></Paper>;
}
