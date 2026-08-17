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
import { useEffect, useMemo, useState, type FormEvent } from 'react';
import { Link as RouterLink, useSearchParams } from 'react-router-dom';

import {
  Directory,
  type DirectoryInvitation,
  type DirectoryTaxonomies,
  type DirectoryTaxonomyItem,
  type ManagedClassified,
  type ManagedDirectoryProfile,
} from '../api/directory';

const slugify = (value: string) => value.toLowerCase().normalize('NFD').replace(/[\u0300-\u036f]/g, '').replace(/[^a-z0-9]+/g, '-').replace(/(^-|-$)/g, '').slice(0, 120);

export default function DirectoryManagePage() {
  const client = useQueryClient();
  const [params, setParams] = useSearchParams();
  const [tab, setTab] = useState(params.has('apply') || params.has('contact') || params.has('invite') ? 2 : 0);
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
          <Tab label="Responder, contactar o invitar" />
          <Tab label="Invitaciones" />
        </Tabs>

        {tab === 0 && <ProfilePanel profiles={profiles.data ?? []} loading={profiles.isLoading} onCreate={() => setProfileOpen(true)} onRefresh={refresh} />}
        {tab === 1 && <ClassifiedPanel classifieds={classifieds.data ?? []} loading={classifieds.isLoading} onCreate={() => setClassifiedOpen(true)} onRefresh={refresh} />}
        {tab === 2 && <OpportunityActionPanel profiles={profiles.data ?? []} classifieds={classifieds.data ?? []} applyId={params.get('apply')} contactId={params.get('contact')} inviteId={params.get('invite')} contextKind={contextKind} onDone={() => { setNotice('Acción enviada de forma segura.'); setParams({}); }} />}
        {tab === 3 && <InvitationPanel />}

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
    {classifieds.map((item) => <Paper key={item.id} variant="outlined" sx={{ p: 2.5, borderRadius: 3 }}><Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" gap={2}><Box><Typography variant="h5" fontWeight={800}>{item.title}</Typography><Typography color="text.secondary">Vence: {item.expiresAt ? new Date(item.expiresAt).toLocaleDateString() : 'al publicar +30 días'}</Typography></Box><Stack direction="row" gap={1} alignItems="center" flexWrap="wrap"><Chip label={item.status} color={item.status === 'published' ? 'success' : 'default'} />{item.status === 'draft' && <Button onClick={() => status.mutate({ id: item.id, value: 'published' })}>Publicar</Button>}{item.status === 'published' && <Button startIcon={<CheckCircleIcon />} onClick={() => status.mutate({ id: item.id, value: 'filled' })}>Marcar cubierto</Button>}<Button onClick={() => setSelected(selected === item.id ? null : item.id)}>Postulaciones</Button></Stack></Stack>{selected === item.id && <Stack mt={2} spacing={1}>{applications.isLoading && <CircularProgress size={24} />}{applications.data?.length === 0 && <Typography color="text.secondary">Sin postulaciones todavía.</Typography>}{applications.data?.map((application) => <ApplicationRow key={String(application['id'])} application={application} authorProfileId={item.authorProfileId} />)}</Stack>}</Paper>)}
  </Stack>;
}

function ApplicationRow({ application, authorProfileId }: { application: Record<string, unknown>; authorProfileId: string }) {
  const client = useQueryClient();
  const [conversationMessage, setConversationMessage] = useState('Hola, aceptamos tu postulación y queremos continuar la conversación en TDF.');
  const profile = typeof application['applicantProfile'] === 'object' && application['applicantProfile'] ? application['applicantProfile'] as Record<string, unknown> : {};
  const applicationId = String(application['id']);
  const applicantProfileId = typeof profile['id'] === 'string' ? profile['id'] : '';
  const refresh = () => client.invalidateQueries({ queryKey: ['directory', 'applications'] });
  const mutation = useMutation({ mutationFn: (status: string) => Directory.setApplicationStatus(applicationId, status), onSuccess: refresh });
  const conversation = useMutation({
    mutationFn: async () => {
      await Directory.contact({ senderProfileId: authorProfileId, targetProfileId: applicantProfileId, contextKind: 'application', contextId: applicationId, message: conversationMessage.trim() }, `directory-application-contact-${applicationId}`);
      return Directory.setApplicationStatus(applicationId, 'conversation_open');
    },
    onSuccess: refresh,
  });
  const profileName = typeof profile['name'] === 'string' ? profile['name'] : 'Perfil postulante';
  const message = typeof application['message'] === 'string' ? application['message'] : '';
  const applicationStatus = typeof application['status'] === 'string' ? application['status'] : 'submitted';
  const awaitingDecision = ['submitted', 'viewed', 'shortlisted'].includes(applicationStatus);
  return <Paper sx={{ p: 2, bgcolor: 'action.hover' }}><Stack spacing={1}><Typography fontWeight={800}>{profileName}</Typography><Typography sx={{ whiteSpace: 'pre-wrap' }}>{message}</Typography>{applicationStatus === 'accepted' && <TextField label="Mensaje para abrir la conversación" value={conversationMessage} onChange={(event) => setConversationMessage(event.target.value)} multiline minRows={2} inputProps={{ minLength: 1, maxLength: 5000 }} />}{(mutation.error || conversation.error) && <Alert severity="error">{(mutation.error ?? conversation.error)?.message}</Alert>}<Stack direction="row" gap={1} flexWrap="wrap"><Chip size="small" label={applicationStatus} />{awaitingDecision && <><Button size="small" onClick={() => mutation.mutate('shortlisted')}>Preseleccionar</Button><Button size="small" onClick={() => mutation.mutate('accepted')}>Aceptar</Button><Button size="small" onClick={() => mutation.mutate('rejected')}>Rechazar</Button></>}{applicationStatus === 'accepted' && <Button size="small" variant="contained" onClick={() => conversation.mutate()} disabled={!applicantProfileId || !conversationMessage.trim() || conversation.isPending}>Abrir conversación</Button>}</Stack></Stack></Paper>;
}

function InvitationPanel() {
  const invitations = useQuery({ queryKey: ['directory', 'invitations'], queryFn: Directory.invitations });
  if (invitations.isLoading) return <CircularProgress />;
  if (invitations.isError) return <Alert severity="error">No se pudieron cargar tus invitaciones.</Alert>;
  if (!invitations.data?.length) return <Alert severity="info">Todavía no tienes invitaciones enviadas o recibidas.</Alert>;
  return <Stack spacing={2}>{invitations.data.map((invitation) => <InvitationCard key={invitation.id} invitation={invitation} />)}</Stack>;
}

function InvitationCard({ invitation }: { invitation: DirectoryInvitation }) {
  const client = useQueryClient();
  const [conversationMessage, setConversationMessage] = useState('Hola, acepté la invitación y quisiera continuar la conversación en TDF.');
  const mine = invitation.participantRole === 'sender' ? invitation.senderProfile : invitation.targetProfile;
  const other = invitation.participantRole === 'sender' ? invitation.targetProfile : invitation.senderProfile;
  const transition = useMutation({
    mutationFn: (status: string) => Directory.setInvitationStatus(invitation.id, status),
    onSuccess: () => client.invalidateQueries({ queryKey: ['directory', 'invitations'] }),
  });
  const conversation = useMutation({
    mutationFn: async () => {
      await Directory.contact({
        senderProfileId: mine.id,
        targetProfileId: other.id,
        contextKind: 'invitation',
        contextId: invitation.id,
        message: conversationMessage.trim(),
      }, `directory-invitation-contact-${invitation.id}`);
      return Directory.setInvitationStatus(invitation.id, 'conversation_open');
    },
    onSuccess: () => client.invalidateQueries({ queryKey: ['directory', 'invitations'] }),
  });
  const pendingTarget = invitation.participantRole === 'target' && invitation.status === 'pending';
  const pendingSender = invitation.participantRole === 'sender' && invitation.status === 'pending';
  const canConverse = invitation.status === 'accepted';
  return <Paper variant="outlined" sx={{ p: 2.5, borderRadius: 3 }}><Stack spacing={1.5}>
    <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" gap={1}>
      <Box><Typography variant="h6" fontWeight={800}>{invitation.participantRole === 'sender' ? `Invitaste a ${other.name}` : `${other.name} te invitó`}</Typography><Typography color="text.secondary">Actúas como {mine.name}{invitation.classified ? ` · ${invitation.classified.title}` : ' · invitación general'}</Typography></Box>
      <Chip label={invitation.status} color={invitation.status === 'accepted' || invitation.status === 'conversation_open' ? 'success' : 'default'} />
    </Stack>
    <Typography sx={{ whiteSpace: 'pre-wrap' }}>{invitation.message}</Typography>
    {canConverse && <TextField label="Mensaje para abrir la conversación" value={conversationMessage} onChange={(event) => setConversationMessage(event.target.value)} multiline minRows={2} inputProps={{ minLength: 1, maxLength: 5000 }} />}
    {(transition.error || conversation.error) && <Alert severity="error">{(transition.error ?? conversation.error)?.message}</Alert>}
    <Stack direction="row" gap={1} flexWrap="wrap">
      {pendingTarget && <><Button onClick={() => transition.mutate('accepted')}>Aceptar</Button><Button onClick={() => transition.mutate('declined')}>Rechazar</Button><Button color="error" onClick={() => transition.mutate('blocked')}>Bloquear</Button></>}
      {pendingSender && <Button onClick={() => transition.mutate('withdrawn')}>Retirar</Button>}
      {canConverse && <Button variant="contained" onClick={() => conversation.mutate()} disabled={!conversationMessage.trim() || conversation.isPending}>Abrir conversación</Button>}
    </Stack>
  </Stack></Paper>;
}

function ProfileDialog({ open, onClose, taxonomies, onCreated }: { open: boolean; onClose: () => void; taxonomies?: DirectoryTaxonomies; onCreated: () => void }) {
  const [name, setName] = useState('');
  const [kind, setKind] = useState('person');
  const [bio, setBio] = useState('');
  const [professions, setProfessions] = useState<string[]>([]);
  const [instruments, setInstruments] = useState<string[]>([]);
  const [genres, setGenres] = useState<string[]>([]);
  const [services, setServices] = useState<string[]>([]);
  const [cityId, setCityId] = useState('');
  const [onsite, setOnsite] = useState(true);
  const [remote, setRemote] = useState(false);
  const [availableToTravel, setAvailableToTravel] = useState(false);
  const [travelRadiusKm, setTravelRadiusKm] = useState('');
  const mutation = useMutation({ mutationFn: () => {
    const city = taxonomies?.cities.find((item) => item.id === cityId);
    if (!city?.countryId) throw new Error('Selecciona una ciudad.');
    return Directory.createProfile({
      profileKind: kind,
      publicName: name.trim(),
      slug: slugify(name),
      bio: bio.trim() || undefined,
      professionIds: professions,
      instrumentIds: instruments,
      genreIds: genres,
      serviceOfferingIds: services,
      countryId: city.countryId,
      cityId,
      onsite,
      remote,
      availableToTravel,
      travelRadiusKm: availableToTravel && travelRadiusKm ? Number(travelRadiusKm) : undefined,
    });
  }, onSuccess: onCreated });
  const validModality = onsite || remote || availableToTravel;
  return <Dialog open={open} onClose={onClose} fullWidth maxWidth="md"><DialogTitle>Crear perfil público</DialogTitle><DialogContent><Stack spacing={2} pt={1}><TextField label="Nombre público" value={name} onChange={(event) => setName(event.target.value)} required inputProps={{ maxLength: 160 }} /><FormControl><InputLabel>Tipo</InputLabel><Select label="Tipo" value={kind} onChange={(event) => setKind(event.target.value)}>{[['person','Persona'],['artist','Artista'],['band','Banda'],['project','Proyecto'],['organization','Organización'],['venue','Venue'],['studio','Estudio'],['agency','Agencia'],['label','Sello'],['distributor','Distribuidora'],['school','Escuela']].map(([value,label]) => <MenuItem key={value} value={value}>{label}</MenuItem>)}</Select></FormControl><TextField label="Biografía" multiline minRows={4} value={bio} onChange={(event) => setBio(event.target.value)} /><TaxonomyMultiSelect label="Profesiones" items={taxonomies?.professions ?? []} values={professions} onChange={setProfessions} /><TaxonomyMultiSelect label="Instrumentos" items={taxonomies?.instruments ?? []} values={instruments} onChange={setInstruments} /><TaxonomyMultiSelect label="Géneros" items={taxonomies?.genres ?? []} values={genres} onChange={setGenres} /><TaxonomyMultiSelect label="Servicios" items={taxonomies?.serviceOfferings ?? []} values={services} onChange={setServices} /><FormControl><InputLabel>Ciudad principal</InputLabel><Select label="Ciudad principal" value={cityId} onChange={(event) => setCityId(event.target.value)}>{(taxonomies?.cities ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</Select></FormControl><Stack direction={{ xs: 'column', sm: 'row' }} gap={1}><FormControlLabel control={<Switch checked={onsite} onChange={(event) => setOnsite(event.target.checked)} />} label="Presencial" /><FormControlLabel control={<Switch checked={remote} onChange={(event) => setRemote(event.target.checked)} />} label="Remoto" /><FormControlLabel control={<Switch checked={availableToTravel} onChange={(event) => setAvailableToTravel(event.target.checked)} />} label="Disponible para viajar" /></Stack>{availableToTravel && <TextField label="Radio de viaje (km)" type="number" value={travelRadiusKm} onChange={(event) => setTravelRadiusKm(event.target.value)} inputProps={{ min: 0, max: 20000 }} />}{!validModality && <Alert severity="warning">Selecciona al menos una modalidad de trabajo.</Alert>}{mutation.error && <Alert severity="error">{mutation.error.message}</Alert>}</Stack></DialogContent><DialogActions><Button onClick={onClose}>Cancelar</Button><Button variant="contained" onClick={() => mutation.mutate()} disabled={!name.trim() || !cityId || !validModality || mutation.isPending}>Crear borrador</Button></DialogActions></Dialog>;
}

function ClassifiedDialog({ open, onClose, profiles, taxonomies, onCreated }: { open: boolean; onClose: () => void; profiles: ManagedDirectoryProfile[]; taxonomies?: DirectoryTaxonomies; onCreated: () => void }) {
  const [profileId, setProfileId] = useState('');
  const [categoryId, setCategoryId] = useState('');
  const [title, setTitle] = useState('');
  const [description, setDescription] = useState('');
  const [professionIds, setProfessionIds] = useState<string[]>([]);
  const [instrumentIds, setInstrumentIds] = useState<string[]>([]);
  const [genreIds, setGenreIds] = useState<string[]>([]);
  const [cityIds, setCityIds] = useState<string[]>([]);
  const [remote, setRemote] = useState(false);
  const [availableToTravel, setAvailableToTravel] = useState(false);
  const [startsAt, setStartsAt] = useState('');
  const [endsAt, setEndsAt] = useState('');
  const [experienceLevel, setExperienceLevel] = useState('any');
  const [compensationTypeId, setCompensationTypeId] = useState('');
  const [budgetMin, setBudgetMin] = useState('');
  const [budgetMax, setBudgetMax] = useState('');
  const [currencyId, setCurrencyId] = useState('');
  const [budgetNegotiable, setBudgetNegotiable] = useState(false);
  const [serviceOfferingId, setServiceOfferingId] = useState('');
  const selectedCategory = taxonomies?.classifiedCategories.find((item) => item.id === categoryId);
  const required = taxonomyRequirements(selectedCategory);
  const selectedCompensation = taxonomies?.compensationTypes.find((item) => item.id === compensationTypeId);
  const budgetMode = typeof selectedCompensation?.metadata?.['budget'] === 'string' ? selectedCompensation.metadata['budget'] : undefined;
  const selectedCurrencyId = currencyId || taxonomies?.currencies.find((item) => item.code === 'USD')?.id || taxonomies?.currencies[0]?.id || '';
  const selectedCurrency = taxonomies?.currencies.find((item) => item.id === selectedCurrencyId);
  const countryIds = useMemo(() => Array.from(new Set(cityIds.flatMap((id) => {
    const countryId = taxonomies?.cities.find((city) => city.id === id)?.countryId;
    return countryId ? [countryId] : [];
  }))), [cityIds, taxonomies?.cities]);
  const validationError = classifiedFormError({ required, cityIds, remote, availableToTravel, professionIds, instrumentIds, genreIds, startsAt, endsAt, compensationTypeId, budgetMode, budgetMin, budgetMax, serviceOfferingId });
  const mutation = useMutation({ mutationFn: () => {
    if (validationError) throw new Error(validationError);
    const minMinor = moneyToMinor(budgetMin, selectedCurrency?.minorUnits);
    const maxMinor = moneyToMinor(budgetMax, selectedCurrency?.minorUnits);
    return Directory.createClassified({
      authorProfileId: profileId,
      categoryId,
      title: title.trim(),
      slug: slugify(`${title}-${Date.now().toString(36)}`),
      description: description.trim(),
      professionIds,
      instrumentIds,
      genreIds,
      countryIds,
      cityIds,
      metropolitanAreaIds: [],
      onsite: cityIds.length > 0,
      remote,
      availableToTravel,
      startsAt: toIso(startsAt),
      endsAt: toIso(endsAt),
      experienceLevel,
      compensationTypeId: compensationTypeId || undefined,
      budgetMinMinor: minMinor,
      budgetMaxMinor: budgetMode === 'exact' && minMinor !== undefined ? minMinor : maxMinor,
      currencyId: minMinor !== undefined || maxMinor !== undefined ? selectedCurrencyId : undefined,
      budgetNegotiable: budgetNegotiable || selectedCompensation?.code === 'negotiable',
      serviceOfferingId: serviceOfferingId || undefined,
    });
  }, onSuccess: onCreated });
  const basicsValid = Boolean(profileId && categoryId && title.trim().length >= 5 && description.trim().length >= 20);
  const showBudget = Boolean(compensationTypeId && budgetMode !== 'forbidden');
  return <Dialog open={open} onClose={onClose} fullWidth maxWidth="md"><DialogTitle>Nueva oportunidad o clasificado</DialogTitle><DialogContent><Stack spacing={2} pt={1}><FormControl><InputLabel>Perfil que publica</InputLabel><Select label="Perfil que publica" value={profileId} onChange={(event) => setProfileId(event.target.value)}>{profiles.map((profile) => <MenuItem key={profile.id} value={profile.id}>{profile.name}</MenuItem>)}</Select></FormControl><FormControl><InputLabel>Categoría</InputLabel><Select label="Categoría" value={categoryId} onChange={(event) => setCategoryId(event.target.value)}>{(taxonomies?.classifiedCategories ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</Select></FormControl>{required.size > 0 && <Alert severity="info">Esta categoría requiere: {Array.from(required).map(requirementLabel).join(', ')}.</Alert>}<TextField label="Título" value={title} onChange={(event) => setTitle(event.target.value)} required inputProps={{ minLength: 5, maxLength: 160 }} /><TextField label="Descripción" value={description} onChange={(event) => setDescription(event.target.value)} multiline minRows={6} required inputProps={{ minLength: 20, maxLength: 10000 }} /><TaxonomyMultiSelect label="Profesiones buscadas" items={taxonomies?.professions ?? []} values={professionIds} onChange={setProfessionIds} required={required.has('professionIds')} /><TaxonomyMultiSelect label="Instrumentos buscados" items={taxonomies?.instruments ?? []} values={instrumentIds} onChange={setInstrumentIds} required={required.has('instrumentIds')} /><TaxonomyMultiSelect label="Géneros" items={taxonomies?.genres ?? []} values={genreIds} onChange={setGenreIds} required={required.has('genreIds')} /><TaxonomyMultiSelect label="Ciudades" items={taxonomies?.cities ?? []} values={cityIds} onChange={setCityIds} required={required.has('locations')} /><Stack direction={{ xs: 'column', sm: 'row' }} gap={1}><FormControlLabel control={<Switch checked={remote} onChange={(event) => setRemote(event.target.checked)} />} label="Acepta remoto" /><FormControlLabel control={<Switch checked={availableToTravel} onChange={(event) => setAvailableToTravel(event.target.checked)} />} label="Disponible para viajar" /></Stack><Stack direction={{ xs: 'column', sm: 'row' }} gap={2}><TextField fullWidth label="Inicio" type="datetime-local" value={startsAt} onChange={(event) => setStartsAt(event.target.value)} InputLabelProps={{ shrink: true }} required={required.has('dateRange')} /><TextField fullWidth label="Fin" type="datetime-local" value={endsAt} onChange={(event) => setEndsAt(event.target.value)} InputLabelProps={{ shrink: true }} required={required.has('dateRange')} /></Stack><FormControl><InputLabel>Experiencia requerida</InputLabel><Select label="Experiencia requerida" value={experienceLevel} onChange={(event) => setExperienceLevel(event.target.value)}>{[['any','Cualquier nivel'],['beginner','Principiante'],['intermediate','Intermedio'],['advanced','Avanzado'],['professional','Profesional']].map(([value,label]) => <MenuItem key={value} value={value}>{label}</MenuItem>)}</Select></FormControl><FormControl required={required.has('compensationTypeId')}><InputLabel>Compensación</InputLabel><Select label="Compensación" value={compensationTypeId} onChange={(event) => { setCompensationTypeId(event.target.value); setBudgetMin(''); setBudgetMax(''); }}>{(taxonomies?.compensationTypes ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</Select></FormControl>{showBudget && <Stack direction={{ xs: 'column', sm: 'row' }} gap={2}><FormControl sx={{ minWidth: 150 }}><InputLabel>Moneda</InputLabel><Select label="Moneda" value={selectedCurrencyId} onChange={(event) => setCurrencyId(event.target.value)}>{(taxonomies?.currencies ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.code} · {item.symbol ?? item.name}</MenuItem>)}</Select></FormControl><TextField label={budgetMode === 'exact' ? 'Monto' : 'Presupuesto mínimo'} type="number" value={budgetMin} onChange={(event) => setBudgetMin(event.target.value)} inputProps={{ min: 0, step: '0.01' }} required={required.has('budget')} />{budgetMode !== 'exact' && <TextField label="Presupuesto máximo" type="number" value={budgetMax} onChange={(event) => setBudgetMax(event.target.value)} inputProps={{ min: 0, step: '0.01' }} required={budgetMode === 'range'} />}<FormControlLabel control={<Switch checked={budgetNegotiable} onChange={(event) => setBudgetNegotiable(event.target.checked)} />} label="Negociable" /></Stack>}<FormControl required={required.has('serviceOfferingId')}><InputLabel>Servicio vinculado</InputLabel><Select label="Servicio vinculado" value={serviceOfferingId} onChange={(event) => setServiceOfferingId(event.target.value)}><MenuItem value="">Sin servicio comercial</MenuItem>{(taxonomies?.serviceOfferings ?? []).map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</Select></FormControl><Typography variant="caption" color="text.secondary">El anuncio vence 30 días después de publicarse. Las ofertas reservables se vinculan al marketplace mediante el servicio seleccionado, sin duplicar el producto comercial.</Typography>{basicsValid && validationError && <Alert severity="warning">{validationError}</Alert>}{mutation.error && <Alert severity="error">{mutation.error.message}</Alert>}</Stack></DialogContent><DialogActions><Button onClick={onClose}>Cancelar</Button><Button variant="contained" onClick={() => mutation.mutate()} disabled={!basicsValid || Boolean(validationError) || mutation.isPending}>Guardar borrador</Button></DialogActions></Dialog>;
}

function TaxonomyMultiSelect({ label, items, values, onChange, required = false }: { label: string; items: DirectoryTaxonomyItem[]; values: string[]; onChange: (values: string[]) => void; required?: boolean }) {
  const labelId = `directory-${slugify(label)}-label`;
  return <FormControl required={required}><InputLabel id={labelId}>{label}</InputLabel><Select labelId={labelId} multiple label={label} value={values} onChange={(event) => onChange(typeof event.target.value === 'string' ? event.target.value.split(',') : event.target.value)} renderValue={(selected) => selected.map((id) => items.find((item) => item.id === id)?.name ?? id).join(', ')}>{items.map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</Select></FormControl>;
}

export function taxonomyRequirements(item?: DirectoryTaxonomyItem): Set<string> {
  const raw = item?.requirements?.['required'];
  return new Set(Array.isArray(raw) ? raw.filter((value): value is string => typeof value === 'string') : []);
}

const REQUIREMENT_LABELS: Record<string, string> = {
  instrumentIds: 'instrumento', genreIds: 'género', professionIds: 'profesión', locations: 'ubicación',
  locationsOrRemote: 'ubicación o remoto', dateRange: 'periodo', compensationTypeId: 'compensación',
  budget: 'presupuesto', serviceOfferingId: 'servicio', expiresAt: 'vencimiento de 30 días',
};

const requirementLabel = (value: string) => REQUIREMENT_LABELS[value] ?? value;
const toIso = (value: string): string | undefined => value ? new Date(value).toISOString() : undefined;
export const moneyToMinor = (value: string, minorUnits = 2): number | undefined => {
  if (!value.trim()) return undefined;
  const amount = Number(value);
  return Number.isFinite(amount) && amount >= 0 ? Math.round(amount * (10 ** minorUnits)) : undefined;
};

export function classifiedFormError(input: { required: Set<string>; cityIds: string[]; remote: boolean; availableToTravel: boolean; professionIds: string[]; instrumentIds: string[]; genreIds: string[]; startsAt: string; endsAt: string; compensationTypeId: string; budgetMode?: string; budgetMin: string; budgetMax: string; serviceOfferingId: string }): string | null {
  const { required } = input;
  if (!input.cityIds.length && !input.remote && !input.availableToTravel) return 'Selecciona una ciudad, trabajo remoto o disponibilidad para viajar.';
  if (required.has('professionIds') && !input.professionIds.length) return 'Selecciona al menos una profesión.';
  if (required.has('instrumentIds') && !input.instrumentIds.length) return 'Selecciona al menos un instrumento.';
  if (required.has('genreIds') && !input.genreIds.length) return 'Selecciona al menos un género.';
  if (required.has('locations') && !input.cityIds.length) return 'Selecciona al menos una ciudad.';
  if (required.has('locationsOrRemote') && !input.cityIds.length && !input.remote) return 'Selecciona una ciudad o activa trabajo remoto.';
  if (required.has('dateRange') && (!input.startsAt || !input.endsAt)) return 'Indica el inicio y el fin de la oportunidad.';
  if (input.startsAt && input.endsAt && new Date(input.endsAt) < new Date(input.startsAt)) return 'La fecha de fin no puede preceder al inicio.';
  if (required.has('compensationTypeId') && !input.compensationTypeId) return 'Selecciona el tipo de compensación.';
  if (required.has('budget') && !input.budgetMin) return 'Indica el presupuesto.';
  if (input.budgetMode === 'range' && (!input.budgetMin || !input.budgetMax)) return 'Indica el rango completo de presupuesto.';
  if (input.budgetMin && input.budgetMax && Number(input.budgetMax) < Number(input.budgetMin)) return 'El presupuesto máximo no puede ser menor que el mínimo.';
  if (required.has('serviceOfferingId') && !input.serviceOfferingId) return 'Selecciona el servicio que ofrece el anuncio.';
  return null;
}

function OpportunityActionPanel({ profiles, classifieds, applyId, contactId, inviteId, contextKind, onDone }: { profiles: ManagedDirectoryProfile[]; classifieds: ManagedClassified[]; applyId: string | null; contactId: string | null; inviteId: string | null; contextKind: 'profile' | 'classified' | 'application' | 'invitation'; onDone: () => void }) {
  const [profileId, setProfileId] = useState('');
  const [message, setMessage] = useState('');
  const [classifiedId, setClassifiedId] = useState('');
  const eligibleClassifieds = classifieds.filter((classified) => classified.authorProfileId === profileId && classified.status === 'published');
  useEffect(() => {
    if (classifiedId && !eligibleClassifieds.some((classified) => classified.id === classifiedId)) setClassifiedId('');
  }, [classifiedId, eligibleClassifieds]);
  const mutation = useMutation({ mutationFn: () => {
    if (applyId) return Directory.apply(applyId, { applicantProfileId: profileId, message: message.trim(), portfolio: [] });
    if (contactId) return Directory.contact({ senderProfileId: profileId, targetProfileId: contactId, contextKind, contextId: contactId, message: message.trim() });
    if (inviteId) return Directory.invite({ senderProfileId: profileId, targetProfileId: inviteId, classifiedId: classifiedId || undefined, message: message.trim() });
    throw new Error('Abre un perfil o clasificado público para iniciar esta acción.');
  }, onSuccess: onDone });
  if (!applyId && !contactId && !inviteId) return <Alert severity="info">Abre un perfil o clasificado desde <Button component={RouterLink} to="/buscar">la búsqueda</Button> para postularte, contactar o invitar.</Alert>;
  return <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}><Stack component="form" spacing={2} onSubmit={(event: FormEvent) => { event.preventDefault(); mutation.mutate(); }}><Typography variant="h5" fontWeight={800}>{applyId ? 'Enviar postulación' : inviteId ? 'Invitar a una oportunidad' : 'Iniciar conversación'}</Typography><FormControl><InputLabel>Actuar como perfil</InputLabel><Select label="Actuar como perfil" value={profileId} onChange={(event) => setProfileId(event.target.value)}><MenuItem value="" disabled>Selecciona explícitamente un perfil</MenuItem>{profiles.map((profile) => <MenuItem key={profile.id} value={profile.id}>{profile.name}</MenuItem>)}</Select></FormControl>{inviteId && <FormControl><InputLabel>Oportunidad</InputLabel><Select label="Oportunidad" value={classifiedId} onChange={(event) => setClassifiedId(event.target.value)}><MenuItem value="">Invitación general</MenuItem>{eligibleClassifieds.map((classified) => <MenuItem key={classified.id} value={classified.id}>{classified.title}</MenuItem>)}</Select></FormControl>}<TextField label="Mensaje" multiline minRows={5} value={message} onChange={(event) => setMessage(event.target.value)} required inputProps={{ minLength: 10, maxLength: 5000 }} helperText="No incluyas teléfono ni correo si aún no quieres compartirlos." />{mutation.error && <Alert severity="error">{mutation.error.message}</Alert>}<Button type="submit" variant="contained" disabled={!profileId || message.trim().length < 10 || mutation.isPending}>{mutation.isPending ? 'Enviando…' : applyId ? 'Postularme' : inviteId ? 'Enviar invitación' : 'Abrir conversación'}</Button></Stack></Paper>;
}
