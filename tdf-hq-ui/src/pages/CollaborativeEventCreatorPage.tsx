import { useEffect, useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
  Alert,
  Autocomplete,
  Avatar,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  FormControlLabel,
  Grid,
  MenuItem,
  Stack,
  Switch,
  TextField,
  ToggleButton,
  ToggleButtonGroup,
  Typography,
} from '@mui/material';
import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import CalendarMonthIcon from '@mui/icons-material/CalendarMonth';
import CheckCircleOutlineIcon from '@mui/icons-material/CheckCircleOutline';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import GroupsOutlinedIcon from '@mui/icons-material/GroupsOutlined';
import LockOutlinedIcon from '@mui/icons-material/LockOutlined';
import PublicIcon from '@mui/icons-material/Public';
import RocketLaunchOutlinedIcon from '@mui/icons-material/RocketLaunchOutlined';
import { DateTime } from 'luxon';
import { Link as RouterLink } from 'react-router-dom';

import { Catalogs } from '../api/catalogs';
import {
  SocialEventsAPI,
  type SocialVenueDTO,
} from '../api/socialEvents';
import type { PartySelectorOption } from '../api/partySelector';
import PageShell from '../components/PageShell';
import { UserSelector } from '../components/party-selector/PartySelector';
import { useSession } from '../session/SessionContext';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import {
  buildInitialCollaborativeEventDraft,
  createCollaborativeEvent,
  type CollaborativeEventCreationResult,
  type CollaborativeEventDraft,
  type EventCollaboratorDraft,
} from './collaborativeEventCreation';

const EVENT_CREATOR_DRAFT_KEY = 'tdf-event-creator:draft:v1';
const LOCAL_DATE_TIME_FORMAT = "yyyy-LL-dd'T'HH:mm";
const DURATION_OPTIONS = [
  { value: 60, label: '1 hora' },
  { value: 90, label: '1 h 30 min' },
  { value: 120, label: '2 horas' },
  { value: 180, label: '3 horas' },
  { value: 240, label: '4 horas' },
  { value: 360, label: '6 horas' },
  { value: 480, label: '8 horas' },
];

const loadDraft = (): CollaborativeEventDraft => {
  const initial = buildInitialCollaborativeEventDraft();
  if (typeof window === 'undefined') return initial;
  try {
    const stored = JSON.parse(
      window.localStorage.getItem(EVENT_CREATOR_DRAFT_KEY) ?? 'null',
    ) as Partial<CollaborativeEventDraft> | null;
    if (!stored || typeof stored !== 'object') return initial;
    return {
      ...initial,
      ...stored,
      durationMinutes: stored.durationMinutes === null
        ? null
        : DURATION_OPTIONS.some(({ value }) => value === stored.durationMinutes)
          ? stored.durationMinutes ?? initial.durationMinutes
          : initial.durationMinutes,
      collaborators: Array.isArray(stored.collaborators)
        ? stored.collaborators.filter(
            (collaborator): collaborator is EventCollaboratorDraft =>
              Boolean(
                collaborator
                && typeof collaborator.partyId === 'string'
                && typeof collaborator.displayName === 'string'
                && (collaborator.role === 'editor' || collaborator.role === 'viewer'),
              ),
          )
        : [],
    };
  } catch {
    return initial;
  }
};

const eventDateLabel = (draft: CollaborativeEventDraft) => {
  const start = DateTime.fromFormat(draft.startAt, LOCAL_DATE_TIME_FORMAT);
  if (!start.isValid) return 'Fecha por definir';
  if (draft.durationMinutes === null) {
    return `${start.setLocale('es').toFormat("ccc d LLL · HH:mm")} · fin por confirmar`;
  }
  const end = start.plus({ minutes: draft.durationMinutes });
  return `${start.setLocale('es').toFormat("ccc d LLL · HH:mm")}–${end.toFormat('HH:mm')}`;
};

const roleLabel = (role: EventCollaboratorDraft['role']) =>
  role === 'editor' ? 'Puede editar' : 'Solo lectura';

export default function CollaborativeEventCreatorPage() {
  const { session } = useSession();
  const { locale } = useLocalePreferences();
  const qc = useQueryClient();
  const [draft, setDraft] = useState<CollaborativeEventDraft>(loadDraft);
  const [creationResult, setCreationResult] =
    useState<CollaborativeEventCreationResult | null>(null);
  const [collaboratorCandidate, setCollaboratorCandidate] = useState<PartySelectorOption | null>(null);
  const sessionPartyId = session?.partyId != null ? String(session.partyId) : '';

  const venuesQuery = useQuery({
    queryKey: ['social-venues'],
    queryFn: () => SocialEventsAPI.listVenues(),
  });
  const eventTypesQuery = useQuery({
    queryKey: ['catalogs', 'event-types', locale],
    queryFn: () => Catalogs.listPublicBatch(['event-types'], { locale, page: 1, pageSize: 200 }),
    staleTime: 5 * 60 * 1000,
  });
  const selectedVenue = useMemo(
    () => (venuesQuery.data ?? []).find(
      (venue) => String(venue.venueId ?? '') === draft.venueId,
    ) ?? null,
    [draft.venueId, venuesQuery.data],
  );
  const eventTypePage = eventTypesQuery.data?.catalogs.find(
    (catalog) => catalog.catalog.code === 'event-types',
  );
  const eventTypeOptions = eventTypePage?.items ?? [];
  const defaultEventTypeId = eventTypePage?.defaults.find(
    (entry) => entry.scopeKind === 'social-event' && entry.scopeId === 'global',
  )?.entityId;
  const selectedEventTypeIsAvailable = eventTypeOptions.some(
    (item) => item.id === draft.eventTypeId,
  );

  useEffect(() => {
    if (draft.eventTypeId || !defaultEventTypeId) return;
    setDraft((current) => current.eventTypeId ? current : { ...current, eventTypeId: defaultEventTypeId });
  }, [defaultEventTypeId, draft.eventTypeId]);

  useEffect(() => {
    if (typeof window === 'undefined' || creationResult) return;
    try {
      window.localStorage.setItem(EVENT_CREATOR_DRAFT_KEY, JSON.stringify(draft));
    } catch {
      // Draft persistence is a convenience; storage restrictions must not block creation.
    }
  }, [creationResult, draft]);

  const creationMutation = useMutation({
    mutationFn: () => createCollaborativeEvent(draft, {
      createEvent: SocialEventsAPI.createEvent,
      addCollaborator: (eventId, collaborator) =>
        SocialEventsAPI.createLogisticsMember(eventId, {
          elmPartyId: collaborator.partyId,
          elmRole: collaborator.role,
        }),
    }),
    onSuccess: (result) => {
      setCreationResult(result);
      try {
        window.localStorage.removeItem(EVENT_CREATOR_DRAFT_KEY);
      } catch {
        // Ignore storage restrictions after the server has accepted the event.
      }
      void qc.invalidateQueries({ queryKey: ['social-events'] });
      void qc.invalidateQueries({ queryKey: ['event-logistics', result.event.eventId] });
    },
  });

  const retryCollaboratorsMutation = useMutation({
    mutationFn: async () => {
      const eventId = creationResult?.event.eventId?.trim();
      if (!eventId || !creationResult) return [];
      return Promise.allSettled(
        creationResult.failedCollaborators.map(({ collaborator }) =>
          SocialEventsAPI.createLogisticsMember(eventId, {
            elmPartyId: collaborator.partyId,
            elmRole: collaborator.role,
          }),
        ),
      );
    },
    onSuccess: (results) => {
      if (!creationResult) return;
      const recovered: EventCollaboratorDraft[] = [];
      const stillFailed: CollaborativeEventCreationResult['failedCollaborators'] = [];
      results.forEach((result, index) => {
        const previous = creationResult.failedCollaborators[index];
        if (!previous) return;
        if (result.status === 'fulfilled') {
          recovered.push(previous.collaborator);
        } else {
          stillFailed.push({
            collaborator: previous.collaborator,
            reason: result.reason instanceof Error
              ? result.reason.message
              : 'No se pudo conceder acceso.',
          });
        }
      });
      setCreationResult({
        ...creationResult,
        addedCollaborators: [...creationResult.addedCollaborators, ...recovered],
        failedCollaborators: stillFailed,
      });
      void qc.invalidateQueries({
        queryKey: ['event-logistics', creationResult.event.eventId],
      });
    },
  });

  const updateDraft = <Key extends keyof CollaborativeEventDraft>(
    key: Key,
    value: CollaborativeEventDraft[Key],
  ) => setDraft((current) => ({ ...current, [key]: value }));

  const addCollaborator = (party: PartySelectorOption) => {
    setDraft((current) => {
      if (String(party.partyId) === sessionPartyId || current.collaborators.some((item) => item.partyId === String(party.partyId))) return current;
      return {
        ...current,
        collaborators: [
          ...current.collaborators,
          {
            partyId: String(party.partyId),
            displayName: party.displayName,
            role: 'editor',
          },
        ],
      };
    });
    setCollaboratorCandidate(null);
  };

  const removeCollaborator = (partyId: string) => setDraft((current) => ({
    ...current,
    collaborators: current.collaborators.filter((collaborator) => collaborator.partyId !== partyId),
  }));

  const updateCollaboratorRole = (
    partyId: string,
    role: EventCollaboratorDraft['role'],
  ) => {
    setDraft((current) => ({
      ...current,
      collaborators: current.collaborators.map((collaborator) =>
        collaborator.partyId === partyId ? { ...collaborator, role } : collaborator
      ),
    }));
  };

  const startAnotherEvent = () => {
    setDraft(buildInitialCollaborativeEventDraft());
    setCreationResult(null);
  };

  if (creationResult) {
    const eventId = String(creationResult.event.eventId);
    return (
      <PageShell
        title="Evento creado"
        subtitle="Tu borrador ya está listo para que el equipo lo desarrolle."
        maxWidth="md"
        actions={(
          <Button
            component={RouterLink}
            to="/social/eventos"
            startIcon={<ArrowBackIcon />}
          >
            Todos los eventos
          </Button>
        )}
      >
        <Card variant="outlined">
          <CardContent>
            <Stack spacing={2.5} alignItems="flex-start">
              <Avatar sx={{ width: 56, height: 56, bgcolor: 'success.main' }}>
                <CheckCircleOutlineIcon />
              </Avatar>
              <Box>
                <Typography variant="h4" fontWeight={800}>
                  {creationResult.event.eventTitle}
                </Typography>
                <Typography color="text.secondary">
                  {eventDateLabel(draft)} · guardado como planificación
                </Typography>
              </Box>
              <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
                <Chip
                  icon={<GroupsOutlinedIcon />}
                  label={`${creationResult.addedCollaborators.length} colaborador${
                    creationResult.addedCollaborators.length === 1 ? '' : 'es'
                  } con acceso`}
                  color="success"
                  variant="outlined"
                />
                <Chip
                  icon={draft.isPublic ? <PublicIcon /> : <LockOutlinedIcon />}
                  label={draft.isPublic ? 'Visible públicamente' : 'Borrador privado'}
                  variant="outlined"
                />
              </Stack>

              {creationResult.failedCollaborators.length > 0 && (
                <Alert
                  severity="warning"
                  action={(
                    <Button
                      color="inherit"
                      size="small"
                      onClick={() => retryCollaboratorsMutation.mutate()}
                      disabled={retryCollaboratorsMutation.isPending}
                    >
                      Reintentar
                    </Button>
                  )}
                  sx={{ width: '100%' }}
                >
                  El evento se creó, pero falta dar acceso a{' '}
                  {creationResult.failedCollaborators
                    .map(({ collaborator }) => collaborator.displayName)
                    .join(', ')}.
                </Alert>
              )}

              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                <Button
                  component={RouterLink}
                  to={`/social/eventos/${encodeURIComponent(eventId)}/logistica`}
                  variant="contained"
                  startIcon={<RocketLaunchOutlinedIcon />}
                >
                  Planificar con el equipo
                </Button>
                <Button
                  component={RouterLink}
                  to={`/social/eventos/${encodeURIComponent(eventId)}`}
                  variant="outlined"
                >
                  Ver evento
                </Button>
                <Button onClick={startAnotherEvent}>Crear otro</Button>
              </Stack>
            </Stack>
          </CardContent>
        </Card>
      </PageShell>
    );
  }

  const mutationError = creationMutation.error instanceof Error
    ? creationMutation.error.message
    : creationMutation.error
      ? 'No se pudo crear el evento.'
      : '';

  return (
    <PageShell
      title="Crear evento"
      subtitle="Empieza con lo esencial, suma al equipo y completa los detalles después."
      actions={(
        <Button
          component={RouterLink}
          to="/social/eventos"
          startIcon={<ArrowBackIcon />}
        >
          Volver
        </Button>
      )}
    >
      <Grid container spacing={3}>
        <Grid item xs={12} lg={8}>
          <Stack spacing={2}>
            {!sessionPartyId && (
              <Alert severity="warning">
                Tu sesión no está vinculada a una persona. Vuelve a iniciar sesión antes de
                crear el evento.
              </Alert>
            )}
            {mutationError && <Alert severity="error">{mutationError}</Alert>}

            <Card variant="outlined">
              <CardContent>
                <Stack spacing={2}>
                  <Box>
                    <Chip label="1 · Lo esencial" size="small" color="primary" />
                    <Typography variant="h5" fontWeight={800} sx={{ mt: 1 }}>
                      ¿Qué vamos a crear?
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      Solo el nombre y la fecha son imprescindibles.
                    </Typography>
                  </Box>
                  {eventTypesQuery.isError && (
                    <Alert severity="error">No se pudieron cargar los tipos de evento publicados.</Alert>
                  )}
                  {draft.eventTypeId && !eventTypesQuery.isLoading && !selectedEventTypeIsAvailable && (
                    <Alert severity="warning">
                      El tipo guardado en este borrador ya no está disponible. Selecciona uno vigente antes de crear el evento.
                    </Alert>
                  )}
                  <ToggleButtonGroup
                    value={draft.eventTypeId}
                    exclusive
                    onChange={(_, value: string | null) => {
                      if (value) updateDraft('eventTypeId', value);
                    }}
                    size="small"
                    sx={{ flexWrap: 'wrap', gap: 0.5 }}
                    disabled={eventTypesQuery.isLoading || eventTypesQuery.isError}
                  >
                    {eventTypeOptions.map((type) => (
                      <ToggleButton
                        key={type.id}
                        value={type.id}
                        sx={{ border: '1px solid !important', borderRadius: '8px !important' }}
                      >
                        {type.name}
                      </ToggleButton>
                    ))}
                  </ToggleButtonGroup>
                  <TextField
                    label="Nombre del evento"
                    value={draft.title}
                    onChange={(event) => updateDraft('title', event.target.value)}
                    placeholder="Ej. TDF Sunset Sessions"
                    required
                    fullWidth
                    inputProps={{ maxLength: 200 }}
                  />
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                    <TextField
                      label="Fecha y hora de inicio"
                      type="datetime-local"
                      value={draft.startAt}
                      onChange={(event) => updateDraft('startAt', event.target.value)}
                      InputLabelProps={{ shrink: true }}
                      required
                      sx={{ flex: 2 }}
                    />
                    <TextField
                      select
                      label="Duración (opcional)"
                      value={draft.durationMinutes ?? ''}
                      onChange={(event) => updateDraft(
                        'durationMinutes',
                        event.target.value === '' ? null : Number(event.target.value),
                      )}
                      sx={{ flex: 1, minWidth: 150 }}
                    >
                      <MenuItem value="">Fin por confirmar</MenuItem>
                      {DURATION_OPTIONS.map((option) => (
                        <MenuItem key={option.value} value={option.value}>
                          {option.label}
                        </MenuItem>
                      ))}
                    </TextField>
                  </Stack>
                  <Autocomplete<SocialVenueDTO, false, false, false>
                    options={venuesQuery.data ?? []}
                    loading={venuesQuery.isLoading}
                    value={selectedVenue}
                    onChange={(_, venue) => updateDraft(
                      'venueId',
                      String(venue?.venueId ?? ''),
                    )}
                    getOptionLabel={(venue) =>
                      `${venue.venueName}${venue.venueCity ? ` · ${venue.venueCity}` : ''}`
                    }
                    isOptionEqualToValue={(option, value) =>
                      option.venueId === value.venueId
                    }
                    renderInput={(params) => (
                      <TextField
                        {...params}
                        label="Lugar (opcional)"
                        placeholder="Busca un venue"
                        helperText="Puedes definirlo o cambiarlo más tarde."
                      />
                    )}
                    noOptionsText="No hay venues disponibles"
                  />
                </Stack>
              </CardContent>
            </Card>

            <Card variant="outlined">
              <CardContent>
                <Stack spacing={2}>
                  <Box>
                    <Chip label="2 · El equipo" size="small" color="primary" />
                    <Typography variant="h5" fontWeight={800} sx={{ mt: 1 }}>
                      ¿Quién lo va a construir contigo?
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      Busca por nombre o usuario. Los editores podrán trabajar en la
                      planificación desde el primer momento.
                    </Typography>
                  </Box>
                  <UserSelector
                    value={collaboratorCandidate}
                    onChange={(party) => {
                      setCollaboratorCandidate(party);
                      if (party) addCollaborator(party);
                    }}
                    field={{ label: 'Añadir colaboradores', helperText: 'Selecciona una cuenta TDF; el ID se conserva internamente.' }}
                    search={{ excludedPartyIds: [Number(sessionPartyId), ...draft.collaborators.map((collaborator) => Number(collaborator.partyId)).filter(Number.isInteger)] }}
                  />
                  {draft.collaborators.length > 0 && (
                    <Stack spacing={1}>
                      {draft.collaborators.map((collaborator) => (
                        <Stack
                          key={collaborator.partyId}
                          direction={{ xs: 'column', sm: 'row' }}
                          spacing={1.5}
                          alignItems={{ sm: 'center' }}
                          sx={{
                            border: '1px solid',
                            borderColor: 'divider',
                            borderRadius: 2,
                            p: 1.25,
                          }}
                        >
                          <Avatar sx={{ width: 36, height: 36 }}>
                            {collaborator.displayName.slice(0, 1).toUpperCase()}
                          </Avatar>
                          <Box sx={{ flex: 1, minWidth: 0 }}>
                            <Typography fontWeight={700}>
                              {collaborator.displayName}
                            </Typography>
                            {collaborator.email && (
                              <Typography
                                variant="caption"
                                color="text.secondary"
                                noWrap
                                display="block"
                              >
                                {collaborator.email}
                              </Typography>
                            )}
                          </Box>
                          <TextField
                            select
                            size="small"
                            label="Acceso"
                            value={collaborator.role}
                            onChange={(event) => updateCollaboratorRole(
                              collaborator.partyId,
                              event.target.value as EventCollaboratorDraft['role'],
                            )}
                            sx={{ minWidth: 160 }}
                          >
                            <MenuItem value="editor">Puede editar</MenuItem>
                            <MenuItem value="viewer">Solo lectura</MenuItem>
                          </TextField>
                          <Button color="inherit" onClick={() => removeCollaborator(collaborator.partyId)}>Quitar</Button>
                        </Stack>
                      ))}
                    </Stack>
                  )}
                </Stack>
              </CardContent>
            </Card>

            <Accordion variant="outlined">
              <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                <Box>
                  <Typography fontWeight={800}>Detalles opcionales</Typography>
                  <Typography variant="body2" color="text.secondary">
                    Descripción, capacidad, precio, tickets y visibilidad
                  </Typography>
                </Box>
              </AccordionSummary>
              <AccordionDetails>
                <Stack spacing={2}>
                  <TextField
                    label="Descripción"
                    value={draft.description}
                    onChange={(event) => updateDraft('description', event.target.value)}
                    multiline
                    minRows={3}
                    placeholder="¿Qué necesita saber el equipo o el público?"
                  />
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                    <TextField
                      label="Capacidad"
                      type="number"
                      value={draft.capacity}
                      onChange={(event) => updateDraft('capacity', event.target.value)}
                      inputProps={{ min: 1, step: 1 }}
                      sx={{ flex: 1 }}
                    />
                    <TextField
                      label="Precio"
                      type="number"
                      value={draft.price}
                      onChange={(event) => updateDraft('price', event.target.value)}
                      inputProps={{ min: 0, step: 0.01 }}
                      InputProps={{ startAdornment: <Typography sx={{ mr: 1 }}>$</Typography> }}
                      helperText="Déjalo vacío si todavía no está definido."
                      sx={{ flex: 1 }}
                    />
                  </Stack>
                  <TextField
                    label="Enlace de tickets"
                    value={draft.ticketUrl}
                    onChange={(event) => updateDraft('ticketUrl', event.target.value)}
                    placeholder="https://…"
                  />
                  <FormControlLabel
                    control={(
                      <Switch
                        checked={draft.isPublic}
                        onChange={(_, checked) => updateDraft('isPublic', checked)}
                      />
                    )}
                    label="Mostrar en el catálogo público de eventos"
                  />
                  <Alert severity={draft.isPublic ? 'info' : 'success'}>
                    {draft.isPublic
                      ? 'El evento será visible de inmediato, aunque seguirá en estado de planificación.'
                      : 'Recomendado: el evento empieza como borrador privado para que el equipo lo prepare.'}
                  </Alert>
                </Stack>
              </AccordionDetails>
            </Accordion>
          </Stack>
        </Grid>

        <Grid item xs={12} lg={4}>
          <Card
            variant="outlined"
            sx={{ position: { lg: 'sticky' }, top: { lg: 120 } }}
          >
            <CardContent>
              <Stack spacing={2}>
                <Box>
                  <Typography variant="overline" color="text.secondary">
                    Vista previa
                  </Typography>
                  <Typography variant="h5" fontWeight={800}>
                    {draft.title.trim() || 'Tu próximo evento'}
                  </Typography>
                </Box>
                <Stack spacing={1}>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <CalendarMonthIcon color="action" fontSize="small" />
                    <Typography variant="body2">{eventDateLabel(draft)}</Typography>
                  </Stack>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <GroupsOutlinedIcon color="action" fontSize="small" />
                    <Typography variant="body2">
                      {draft.collaborators.length === 0
                        ? 'Solo tú por ahora'
                        : `${draft.collaborators.length} colaborador${
                          draft.collaborators.length === 1 ? '' : 'es'
                        }`}
                    </Typography>
                  </Stack>
                  <Stack direction="row" spacing={1} alignItems="center">
                    {draft.isPublic
                      ? <PublicIcon color="action" fontSize="small" />
                      : <LockOutlinedIcon color="action" fontSize="small" />}
                    <Typography variant="body2">
                      {draft.isPublic ? 'Visible públicamente' : 'Borrador privado'}
                    </Typography>
                  </Stack>
                </Stack>
                {draft.collaborators.length > 0 && (
                  <Stack direction="row" spacing={0.75} useFlexGap flexWrap="wrap">
                    {draft.collaborators.map((collaborator) => (
                      <Chip
                        key={collaborator.partyId}
                        size="small"
                        label={`${collaborator.displayName} · ${roleLabel(collaborator.role)}`}
                      />
                    ))}
                  </Stack>
                )}
                <Alert severity="info" icon={false}>
                  Guardaremos el evento como <strong>planificación</strong>. Luego podrán
                  completar cronograma, lugares, responsables, tickets y presupuesto.
                </Alert>
                <Button
                  variant="contained"
                  size="large"
                  startIcon={creationMutation.isPending
                    ? <CircularProgress size={18} color="inherit" />
                    : <RocketLaunchOutlinedIcon />}
                  onClick={() => creationMutation.mutate()}
                  disabled={
                    creationMutation.isPending
                    || !sessionPartyId
                    || !draft.title.trim()
                    || !draft.startAt
                  }
                  fullWidth
                >
                  {creationMutation.isPending
                    ? 'Creando…'
                    : draft.collaborators.length > 0
                      ? `Crear con ${draft.collaborators.length} colaborador${
                        draft.collaborators.length === 1 ? '' : 'es'
                      }`
                      : 'Crear evento'}
                </Button>
                <Typography variant="caption" color="text.secondary" textAlign="center">
                  Tu avance se guarda automáticamente en este navegador.
                </Typography>
              </Stack>
            </CardContent>
          </Card>
        </Grid>
      </Grid>
    </PageShell>
  );
}
