import { useEffect, useMemo, useRef, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Divider,
  Link,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import AddLocationAltIcon from '@mui/icons-material/AddLocationAlt';
import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import DirectionsCarIcon from '@mui/icons-material/DirectionsCar';
import EditOutlinedIcon from '@mui/icons-material/EditOutlined';
import GroupAddIcon from '@mui/icons-material/GroupAdd';
import PrintIcon from '@mui/icons-material/Print';
import RefreshIcon from '@mui/icons-material/Refresh';
import RouteIcon from '@mui/icons-material/Route';
import { Link as RouterLink, useParams } from 'react-router-dom';
import { DateTime } from 'luxon';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { UserSelector } from '../components/party-selector/PartySelector';
import type { PartySelectorOption } from '../api/partySelector';

import PageShell from '../components/PageShell';
import { GOOGLE_MAPS_BROWSER_API_KEY } from '../config/appConfig';
import {
  SocialEventsAPI,
  type EventLogisticsActivityDTO,
  type EventLogisticsMemberDTO,
  type EventLogisticsPlaceDTO,
  type LogisticsActivityType,
  type LogisticsTravelMode,
} from '../api/socialEvents';

interface PlaceDraft {
  venueId: string;
  label: string;
  type: EventLogisticsPlaceDTO['elpType'];
  address: string;
  googlePlaceId: string;
  latitude: string;
  longitude: string;
  instructions: string;
  contactName: string;
  contactPhone: string;
}

interface ActivityDraft {
  type: LogisticsActivityType;
  title: string;
  notes: string;
  start: string;
  end: string;
  placeId: string;
  originPlaceId: string;
  destinationPlaceId: string;
  travelMode: LogisticsTravelMode;
  bufferMinutes: string;
  priority: EventLogisticsActivityDTO['eacPriority'];
  status: EventLogisticsActivityDTO['eacStatus'];
  assigneePartyId: string;
  externalName: string;
  externalPhone: string;
  externalEmail: string;
  dependencyIds: string;
}

interface GoogleLatLng {
  lat: () => number;
  lng: () => number;
}

interface GooglePlace {
  place_id?: string;
  formatted_address?: string;
  name?: string;
  geometry?: { location?: GoogleLatLng };
}

interface GoogleMapInstance {
  setCenter: (position: { lat: number; lng: number }) => void;
  addListener: (eventName: string, handler: (event: { latLng?: GoogleLatLng }) => void) => void;
}

interface GoogleMarkerInstance {
  setPosition: (position: { lat: number; lng: number }) => void;
}

interface GoogleAutocompleteInstance {
  addListener: (eventName: string, handler: () => void) => void;
  getPlace: () => GooglePlace;
}

interface GoogleMapsApi {
  Map: new (element: HTMLElement, options: object) => GoogleMapInstance;
  Marker: new (options: object) => GoogleMarkerInstance;
  places: {
    Autocomplete: new (input: HTMLInputElement, options: object) => GoogleAutocompleteInstance;
  };
}

const googleMapsApi = (): GoogleMapsApi | undefined =>
  (window as unknown as { google?: { maps?: GoogleMapsApi } }).google?.maps;

let mapsLoader: Promise<void> | null = null;
const loadGoogleMaps = (apiKey: string): Promise<void> => {
  if (googleMapsApi()) return Promise.resolve();
  if (mapsLoader) return mapsLoader;
  mapsLoader = new Promise((resolve, reject) => {
    const existing = document.querySelector<HTMLScriptElement>('script[data-tdf-google-maps]');
    if (existing) {
      existing.addEventListener('load', () => resolve(), { once: true });
      existing.addEventListener('error', () => reject(new Error('No se pudo cargar Google Maps.')), { once: true });
      return;
    }
    const script = document.createElement('script');
    script.dataset['tdfGoogleMaps'] = 'true';
    script.async = true;
    script.defer = true;
    script.src = `https://maps.googleapis.com/maps/api/js?key=${encodeURIComponent(apiKey)}&libraries=places&v=weekly`;
    script.onload = () => resolve();
    script.onerror = () => reject(new Error('No se pudo cargar Google Maps.'));
    document.head.appendChild(script);
  });
  return mapsLoader;
};

const emptyPlace = (): PlaceDraft => ({
  venueId: '', label: '', type: 'custom', address: '', googlePlaceId: '', latitude: '', longitude: '',
  instructions: '', contactName: '', contactPhone: '',
});

const localDateTimeFormat = "yyyy-LL-dd'T'HH:mm";
const toLocalInput = (value?: string | null, timezone = 'UTC') => {
  const date = value ? DateTime.fromISO(value) : DateTime.now();
  return date.isValid ? date.setZone(timezone).toFormat(localDateTimeFormat) : '';
};

const toIsoInZone = (value: string, timezone: string, label: string) => {
  const parsed = DateTime.fromFormat(value, localDateTimeFormat, { zone: timezone });
  const iso = parsed.toUTC().toISO();
  if (!parsed.isValid || !iso) throw new Error(`${label} no es una fecha válida en ${timezone}.`);
  return iso;
};

const emptyActivity = (
  eventStart?: string,
  eventEnd?: string | null,
  timezone = 'UTC',
  travelMode: LogisticsTravelMode = 'drive',
): ActivityDraft => ({
  type: 'task', title: '', notes: '', start: toLocalInput(eventStart, timezone), end: eventEnd ? toLocalInput(eventEnd, timezone) : '',
  placeId: '', originPlaceId: '', destinationPlaceId: '', travelMode, bufferMinutes: '',
  priority: 'normal', status: 'planned', assigneePartyId: '', externalName: '', externalPhone: '',
  externalEmail: '', dependencyIds: '',
});

const errorText = (error: unknown) => error instanceof Error ? error.message : 'La operación no pudo completarse.';
const assertValidTimezone = (timezone: string) => {
  try {
    new Intl.DateTimeFormat(undefined, { timeZone: timezone }).format();
  } catch {
    throw new Error('La zona horaria no es un identificador IANA válido.');
  }
};
const secondsLabel = (seconds?: number | null) => {
  if (seconds == null) return 'Sin estimación';
  const hours = Math.floor(seconds / 3600);
  const minutes = Math.ceil((seconds % 3600) / 60);
  return [hours ? `${hours} h` : '', minutes ? `${minutes} min` : ''].filter(Boolean).join(' ');
};
const assignmentFilterKey = (assignment: EventLogisticsActivityDTO['eacAssignments'][number]) =>
  assignment.elaPartyId ? `party:${assignment.elaPartyId}` : `external:${assignment.elaExternalName ?? ''}`;

function PlacesSection({
  places,
  canEdit,
  onEdit,
  onDelete,
}: {
  places: EventLogisticsPlaceDTO[];
  canEdit: boolean;
  onEdit: (place: EventLogisticsPlaceDTO) => void;
  onDelete: (placeId: string) => void;
}) {
  return (
    <Card variant="outlined">
      <CardContent>
        <Stack spacing={1.5}>
          <Typography variant="h6">Lugares</Typography>
          {places.length ? places.map((place) => (
            <Stack
              key={place.elpId}
              direction={{ xs: 'column', sm: 'row' }}
              spacing={1}
              justifyContent="space-between"
              sx={{ p: 1, border: '1px solid', borderColor: 'divider', borderRadius: 2 }}
            >
              <Box>
                <Typography fontWeight={700}>{place.elpLabel}</Typography>
                <Typography variant="body2" color="text.secondary">
                  {place.elpAddress || `${place.elpLatitude}, ${place.elpLongitude}`}
                </Typography>
                {place.elpInstructions && <Typography variant="caption">{place.elpInstructions}</Typography>}
              </Box>
              <Stack className="no-print" direction="row">
                <Button component={Link} href={`https://www.google.com/maps/search/?api=1&query=${place.elpLatitude},${place.elpLongitude}`} size="small" target="_blank">
                  Mapa
                </Button>
                {canEdit && <Button size="small" onClick={() => onEdit(place)}>Editar</Button>}
                {canEdit && <Button size="small" color="error" onClick={() => onDelete(String(place.elpId))}><DeleteOutlineIcon /></Button>}
              </Stack>
            </Stack>
          )) : <Typography color="text.secondary">Aún no hay lugares georreferenciados.</Typography>}
        </Stack>
      </CardContent>
    </Card>
  );
}

function PlaceMapPicker({ draft, onChange }: { draft: PlaceDraft; onChange: (next: PlaceDraft) => void }) {
  const mapRef = useRef<HTMLDivElement>(null);
  const searchRef = useRef<HTMLInputElement>(null);
  const markerRef = useRef<GoogleMarkerInstance | null>(null);
  const mapInstanceRef = useRef<GoogleMapInstance | null>(null);
  const draftRef = useRef(draft);
  const [mapError, setMapError] = useState('');
  const lat = Number(draft.latitude);
  const lng = Number(draft.longitude);
  const hasCoordinates = draft.latitude.trim() !== '' && draft.longitude.trim() !== '' && Number.isFinite(lat) && Number.isFinite(lng);

  useEffect(() => { draftRef.current = draft; }, [draft]);

  useEffect(() => {
    if (!hasCoordinates) return;
    const position = { lat, lng };
    markerRef.current?.setPosition(position);
    mapInstanceRef.current?.setCenter(position);
  }, [hasCoordinates, lat, lng]);

  useEffect(() => {
    if (!GOOGLE_MAPS_BROWSER_API_KEY || !mapRef.current || !searchRef.current) return;
    let active = true;
    void loadGoogleMaps(GOOGLE_MAPS_BROWSER_API_KEY).then(() => {
      if (!active || !mapRef.current || !searchRef.current) return;
      const maps = googleMapsApi();
      if (!maps) return;
      const initial = hasCoordinates ? { lat, lng } : { lat: -0.1807, lng: -78.4678 };
      const map = new maps.Map(mapRef.current, { center: initial, zoom: 13, mapTypeControl: false });
      mapInstanceRef.current = map;
      const marker = new maps.Marker({ map, position: initial, draggable: false });
      markerRef.current = marker;
      map.addListener('click', (event) => {
        const position = event.latLng;
        if (!position) return;
        const next = { lat: position.lat(), lng: position.lng() };
        marker.setPosition(next);
        onChange({ ...draftRef.current, venueId: '', latitude: String(next.lat), longitude: String(next.lng), googlePlaceId: '' });
      });
      const autocomplete = new maps.places.Autocomplete(searchRef.current, { fields: ['place_id', 'formatted_address', 'name', 'geometry'] });
      autocomplete.addListener('place_changed', () => {
        const place = autocomplete.getPlace();
        const position = place.geometry?.location;
        if (!position) return;
        const next = { lat: position.lat(), lng: position.lng() };
        marker.setPosition(next);
        map.setCenter(next);
        onChange({
          ...draftRef.current,
          venueId: '',
          label: draftRef.current.label || place.name || '',
          address: place.formatted_address || draftRef.current.address,
          googlePlaceId: place.place_id || '',
          latitude: String(next.lat),
          longitude: String(next.lng),
        });
      });
    }).catch((error: unknown) => setMapError(errorText(error)));
    return () => { active = false; };
    // The picker is initialized once per form. Field changes are reflected through explicit handlers.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  return (
    <Stack spacing={1}>
      <TextField inputRef={searchRef} label="Buscar lugar en Google" placeholder="Hotel, venue, aeropuerto…" disabled={!GOOGLE_MAPS_BROWSER_API_KEY} />
      {!GOOGLE_MAPS_BROWSER_API_KEY && <Alert severity="info">Configura VITE_GOOGLE_MAPS_BROWSER_API_KEY para búsqueda y selección mediante pin. Las coordenadas manuales siguen disponibles.</Alert>}
      {mapError && <Alert severity="warning">{mapError}</Alert>}
      {GOOGLE_MAPS_BROWSER_API_KEY && <Box ref={mapRef} aria-label="Mapa para seleccionar coordenadas" sx={{ height: 280, borderRadius: 2, overflow: 'hidden', bgcolor: 'action.hover' }} />}
    </Stack>
  );
}

export default function EventLogisticsPage() {
  const { timezone: preferredTimezone, locale } = useLocalePreferences();
  const { eventId = '' } = useParams();
  const qc = useQueryClient();
  const eventQuery = useQuery({ queryKey: ['social-event', eventId], queryFn: () => SocialEventsAPI.getEvent(eventId), enabled: Boolean(eventId) });
  const planQuery = useQuery({ queryKey: ['event-logistics', eventId], queryFn: () => SocialEventsAPI.getLogisticsPlan(eventId), enabled: Boolean(eventId) });
  const venuesQuery = useQuery({ queryKey: ['social-venues'], queryFn: () => SocialEventsAPI.listVenues(), enabled: Boolean(eventId) });
  const [placeDraft, setPlaceDraft] = useState<PlaceDraft>(emptyPlace);
  const [editingPlaceId, setEditingPlaceId] = useState('');
  const [activityDraft, setActivityDraft] = useState<ActivityDraft>(() => emptyActivity());
  const [editingActivity, setEditingActivity] = useState<EventLogisticsActivityDTO | null>(null);
  const [activityAssignee, setActivityAssignee] = useState<PartySelectorOption | null>(null);
  const [memberParty, setMemberParty] = useState<PartySelectorOption | null>(null);
  const [memberRole, setMemberRole] = useState<EventLogisticsMemberDTO['elmRole']>('editor');
  const [memberNotice, setMemberNotice] = useState('');
  const memberFeedbackRef = useRef<HTMLDivElement>(null);
  const [timezone, setTimezone] = useState(preferredTimezone);
  const [defaultMode, setDefaultMode] = useState<LogisticsTravelMode>('drive');
  const [statusFilter, setStatusFilter] = useState('all');
  const [typeFilter, setTypeFilter] = useState('all');
  const [assigneeFilter, setAssigneeFilter] = useState('all');

  useEffect(() => {
    if (eventQuery.data) setActivityDraft((current) => current.title ? current : emptyActivity(eventQuery.data.eventStart, eventQuery.data.eventEnd, timezone, defaultMode));
  }, [defaultMode, eventQuery.data, timezone]);
  useEffect(() => {
    if (planQuery.data) {
      setTimezone(planQuery.data.elgSettings.elsTimezone);
      setDefaultMode(planQuery.data.elgSettings.elsDefaultTravelMode);
    }
  }, [planQuery.data]);
  useEffect(() => {
    if (!planQuery.data) setTimezone(preferredTimezone);
  }, [planQuery.data, preferredTimezone]);

  const refresh = () => qc.invalidateQueries({ queryKey: ['event-logistics', eventId] });
  const settingsMutation = useMutation({
    mutationFn: () => {
      assertValidTimezone(timezone);
      return SocialEventsAPI.updateLogisticsSettings(eventId, { elsTimezone: timezone, elsDefaultTravelMode: defaultMode });
    },
    onSuccess: refresh,
  });
  const memberMutation = useMutation({
    mutationFn: () => {
      if (!memberParty) throw new Error('Selecciona una persona para el equipo.');
      return SocialEventsAPI.createLogisticsMember(eventId, { elmPartyId: String(memberParty.partyId), elmRole: memberRole });
    },
    onSuccess: () => {
      setMemberParty(null);
      setMemberNotice('La persona se añadió al equipo.');
      window.requestAnimationFrame(() => memberFeedbackRef.current?.focus());
      void refresh();
    },
  });
  const deleteMemberMutation = useMutation({ mutationFn: (partyId: string) => SocialEventsAPI.deleteLogisticsMember(eventId, partyId), onSuccess: refresh });
  const placeMutation = useMutation({
    mutationFn: () => {
      const latitude = Number(placeDraft.latitude);
      const longitude = Number(placeDraft.longitude);
      if (!placeDraft.label.trim() || !placeDraft.latitude.trim() || !placeDraft.longitude.trim() || !Number.isFinite(latitude) || !Number.isFinite(longitude)) throw new Error('Nombre y coordenadas válidas son obligatorios.');
      const payload: EventLogisticsPlaceDTO = {
        elpVenueId: placeDraft.venueId || null, elpLabel: placeDraft.label.trim(), elpType: placeDraft.type, elpAddress: placeDraft.address.trim() || null,
        elpGooglePlaceId: placeDraft.googlePlaceId.trim() || null, elpLatitude: latitude, elpLongitude: longitude,
        elpInstructions: placeDraft.instructions.trim() || null, elpContactName: placeDraft.contactName.trim() || null,
        elpContactPhone: placeDraft.contactPhone.trim() || null,
      };
      return editingPlaceId
        ? SocialEventsAPI.updateLogisticsPlace(eventId, editingPlaceId, payload)
        : SocialEventsAPI.createLogisticsPlace(eventId, payload);
    },
    onSuccess: () => { setPlaceDraft(emptyPlace()); setEditingPlaceId(''); void refresh(); },
  });
  const deletePlaceMutation = useMutation({ mutationFn: (placeId: string) => SocialEventsAPI.deleteLogisticsPlace(eventId, placeId), onSuccess: refresh });
  const activityMutation = useMutation({
    mutationFn: () => {
      if (!activityDraft.title.trim()) throw new Error('El título de la actividad es obligatorio.');
      const assignments = activityDraft.assigneePartyId.trim()
        ? [{ elaPartyId: activityDraft.assigneePartyId.trim() }]
        : activityDraft.externalName.trim()
          ? [{ elaExternalName: activityDraft.externalName.trim(), elaExternalPhone: activityDraft.externalPhone.trim() || null, elaExternalEmail: activityDraft.externalEmail.trim() || null }]
          : [];
      const payload: EventLogisticsActivityDTO = {
        eacType: activityDraft.type, eacTitle: activityDraft.title.trim(), eacNotes: activityDraft.notes.trim() || null,
        eacStart: toIsoInZone(activityDraft.start, timezone, 'El inicio'),
        eacEnd: activityDraft.type === 'milestone' ? null : toIsoInZone(activityDraft.end, timezone, 'El fin'),
        eacPlaceId: activityDraft.type === 'travel' ? null : activityDraft.placeId || null,
        eacOriginPlaceId: activityDraft.type === 'travel' ? activityDraft.originPlaceId || null : null,
        eacDestinationPlaceId: activityDraft.type === 'travel' ? activityDraft.destinationPlaceId || null : null,
        eacTravelMode: activityDraft.type === 'travel' ? activityDraft.travelMode : null,
        eacBufferMinutes: activityDraft.bufferMinutes.trim() ? Number(activityDraft.bufferMinutes) : null,
        eacPriority: activityDraft.priority, eacStatus: activityDraft.status,
        eacVersion: editingActivity?.eacVersion,
        eacAssignments: assignments,
        eacDependencyIds: activityDraft.dependencyIds.split(',').map((value) => value.trim()).filter(Boolean),
      };
      return editingActivity?.eacId
        ? SocialEventsAPI.updateLogisticsActivity(eventId, String(editingActivity.eacId), payload)
        : SocialEventsAPI.createLogisticsActivity(eventId, payload);
    },
    onSuccess: () => { setEditingActivity(null); setActivityAssignee(null); setActivityDraft(emptyActivity(eventQuery.data?.eventStart, eventQuery.data?.eventEnd, timezone, defaultMode)); void refresh(); },
  });
  const updateActivityMutation = useMutation({
    mutationFn: ({ activity, status }: { activity: EventLogisticsActivityDTO; status: EventLogisticsActivityDTO['eacStatus'] }) =>
      SocialEventsAPI.updateLogisticsActivity(eventId, String(activity.eacId), { ...activity, eacStatus: status }),
    onSuccess: refresh,
  });
  const deleteActivityMutation = useMutation({ mutationFn: (activityId: string) => SocialEventsAPI.deleteLogisticsActivity(eventId, activityId), onSuccess: refresh });
  const verifyMutation = useMutation({ mutationFn: () => SocialEventsAPI.verifyAllLogisticsRoutes(eventId), onSuccess: refresh });
  const verifyActivityMutation = useMutation({ mutationFn: (activityId: string) => SocialEventsAPI.verifyLogisticsRoute(eventId, activityId), onSuccess: refresh });

  const plan = planQuery.data;
  const canEdit = plan?.elgAccessRole === 'owner' || plan?.elgAccessRole === 'editor';
  const isOwner = plan?.elgAccessRole === 'owner';
  const placesById = useMemo(() => new Map(plan?.elgPlaces.map((place) => [String(place.elpId), place]) ?? []), [plan?.elgPlaces]);
  const assigneeOptions = useMemo(() => {
    const options = new Map<string, string>();
    for (const activity of plan?.elgActivities ?? []) {
      for (const assignment of activity.eacAssignments) {
        options.set(assignmentFilterKey(assignment), assignment.elaDisplayName ?? assignment.elaExternalName ?? assignment.elaPartyId ?? 'Responsable');
      }
    }
    return [...options.entries()];
  }, [plan?.elgActivities]);
  const visibleActivities = useMemo(() => (plan?.elgActivities ?? []).filter((activity) =>
    (statusFilter === 'all' || activity.eacStatus === statusFilter)
      && (typeFilter === 'all' || activity.eacType === typeFilter)
      && (assigneeFilter === 'all' || activity.eacAssignments.some((assignment) => assignmentFilterKey(assignment) === assigneeFilter))),
  [assigneeFilter, plan?.elgActivities, statusFilter, typeFilter]);
  const mutationError = settingsMutation.error ?? memberMutation.error ?? deleteMemberMutation.error ?? placeMutation.error ?? deletePlaceMutation.error ?? activityMutation.error ?? updateActivityMutation.error ?? deleteActivityMutation.error ?? verifyMutation.error ?? verifyActivityMutation.error;

  const startEditingPlace = (place: EventLogisticsPlaceDTO) => {
    setEditingPlaceId(String(place.elpId ?? ''));
    setPlaceDraft({
      venueId: place.elpVenueId ?? '', label: place.elpLabel, type: place.elpType, address: place.elpAddress ?? '', googlePlaceId: place.elpGooglePlaceId ?? '',
      latitude: String(place.elpLatitude), longitude: String(place.elpLongitude), instructions: place.elpInstructions ?? '',
      contactName: place.elpContactName ?? '', contactPhone: place.elpContactPhone ?? '',
    });
  };

  const startEditingActivity = (activity: EventLogisticsActivityDTO) => {
    const assignment = activity.eacAssignments[0];
    setEditingActivity(activity);
    setActivityAssignee(assignment?.elaPartyId
      ? {
        partyId: Number(assignment.elaPartyId), partyType: 'person',
        displayName: assignment.elaDisplayName ?? `Usuario ${assignment.elaPartyId}`,
        username: null, avatarUrl: null, secondaryLabel: null, accountStatus: 'active',
      }
      : null);
    setActivityDraft({
      type: activity.eacType,
      title: activity.eacTitle,
      notes: activity.eacNotes ?? '',
      start: toLocalInput(activity.eacStart, timezone),
      end: toLocalInput(activity.eacEnd ?? activity.eacStart, timezone),
      placeId: activity.eacPlaceId ?? '',
      originPlaceId: activity.eacOriginPlaceId ?? '',
      destinationPlaceId: activity.eacDestinationPlaceId ?? '',
      travelMode: activity.eacTravelMode ?? plan?.elgSettings.elsDefaultTravelMode ?? 'drive',
      bufferMinutes: activity.eacBufferMinutes == null ? '' : String(activity.eacBufferMinutes),
      priority: activity.eacPriority,
      status: activity.eacStatus,
      assigneePartyId: assignment?.elaPartyId ?? '',
      externalName: assignment?.elaExternalName ?? '',
      externalPhone: assignment?.elaExternalPhone ?? '',
      externalEmail: assignment?.elaExternalEmail ?? '',
      dependencyIds: activity.eacDependencyIds.join(', '),
    });
    window.scrollTo({ top: 0, behavior: 'smooth' });
  };

  const selectVenue = (venueId: string) => {
    const venue = venuesQuery.data?.find((candidate) => String(candidate.venueId ?? '') === venueId);
    if (!venue) {
      setPlaceDraft({ ...placeDraft, venueId: '' });
      return;
    }
    setPlaceDraft({
      ...placeDraft,
      venueId,
      label: venue.venueName,
      type: 'venue',
      address: venue.venueAddress ?? '',
      latitude: venue.venueLat == null ? '' : String(venue.venueLat),
      longitude: venue.venueLng == null ? '' : String(venue.venueLng),
      contactName: venue.venueContact ?? '',
      contactPhone: venue.venuePhone ?? '',
      googlePlaceId: '',
    });
  };

  return (
    <PageShell
      title={`Logística · ${eventQuery.data?.eventTitle ?? 'Evento'}`}
      subtitle="Cronograma operativo, responsables, lugares y verificación de rutas"
      loading={eventQuery.isLoading || planQuery.isLoading}
      actions={<Stack className="no-print" direction="row" spacing={1}>
        <Button component={RouterLink} to={`/social/eventos/${eventId}`} startIcon={<ArrowBackIcon />}>Evento</Button>
        <Button startIcon={<PrintIcon />} onClick={() => window.print()}>Imprimir / PDF</Button>
      </Stack>}
    >
      <Stack spacing={2.5} sx={{ '@media print': { '& .no-print': { display: 'none !important' }, '& .MuiCard-root': { breakInside: 'avoid', boxShadow: 'none' } } }}>
        {(planQuery.error || mutationError) && <Alert severity="error">{errorText(planQuery.error ?? mutationError)}</Alert>}
        {plan?.elgIssues.map((issue, index) => <Alert key={`${issue.esiCode}-${issue.esiActivityId}-${index}`} severity={issue.esiSeverity}>{issue.esiMessage}</Alert>)}

        {isOwner && <Card className="no-print" variant="outlined"><CardContent><Stack spacing={1.5}>
          <Typography variant="h6">Configuración y equipo</Typography>
          <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
            <TextField label="Zona horaria IANA" value={timezone} onChange={(event) => setTimezone(event.target.value)} sx={{ flex: 1 }} />
            <TextField select label="Modo predeterminado" value={defaultMode} onChange={(event) => setDefaultMode(event.target.value as LogisticsTravelMode)} sx={{ minWidth: 190 }}>
              <MenuItem value="drive">Vehículo</MenuItem><MenuItem value="walk">Caminata</MenuItem><MenuItem value="bicycle">Bicicleta</MenuItem><MenuItem value="two_wheeler">Motocicleta</MenuItem><MenuItem value="transit">Transporte público</MenuItem>
            </TextField>
            <Button variant="outlined" onClick={() => settingsMutation.mutate()} disabled={settingsMutation.isPending}>Guardar</Button>
          </Stack>
          <Divider />
          <Stack
            component="form"
            direction={{ xs: 'column', md: 'row' }}
            spacing={1}
            onSubmit={(event) => { event.preventDefault(); memberMutation.mutate(); }}
            data-focus-return="team-members-status"
          >
            <Box sx={{ flex: 1 }}><UserSelector field={{ label: 'Persona del equipo', helperText: 'Busca por nombre o @username.' }} value={memberParty} onChange={setMemberParty} search={{ context: 'crm_assignment' }} /></Box>
            <TextField select label="Permiso" value={memberRole} onChange={(event) => setMemberRole(event.target.value as EventLogisticsMemberDTO['elmRole'])} sx={{ minWidth: 150 }}><MenuItem value="editor">Editor</MenuItem><MenuItem value="viewer">Lector</MenuItem></TextField>
            <Button type="submit" variant="contained" startIcon={<GroupAddIcon />} disabled={!memberParty || memberMutation.isPending}>Añadir</Button>
          </Stack>
          {memberNotice && <Alert id="team-members-status" ref={memberFeedbackRef} severity="success" tabIndex={-1}>{memberNotice}</Alert>}
          <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
            {plan.elgMembers.map((member) => <Chip key={member.elmPartyId} label={`${member.elmDisplayName ?? 'Usuario no disponible'} · ${member.elmRole}`} onDelete={() => deleteMemberMutation.mutate(member.elmPartyId)} />)}
          </Stack>
        </Stack></CardContent></Card>}

        {canEdit && <Card className="no-print" variant="outlined"><CardContent><Stack spacing={1.5}>
          <Typography variant="h6">{editingPlaceId ? 'Editar lugar' : 'Añadir lugar'}</Typography>
          <TextField
            select
            label="Reusar venue del directorio (opcional)"
            value={placeDraft.venueId}
            onChange={(event) => selectVenue(event.target.value)}
            helperText="Copia su dirección, coordenadas y contacto; puedes completar o corregir los datos antes de guardar."
          >
            <MenuItem value="">Lugar independiente</MenuItem>
            {venuesQuery.data?.map((venue) => <MenuItem key={venue.venueId} value={venue.venueId ?? ''}>{venue.venueName}{venue.venueCity ? ` · ${venue.venueCity}` : ''}</MenuItem>)}
          </TextField>
          <PlaceMapPicker draft={placeDraft} onChange={setPlaceDraft} />
          <Box sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', md: '2fr 1fr 1fr' }, gap: 1 }}>
            <TextField label="Nombre" value={placeDraft.label} onChange={(event) => setPlaceDraft({ ...placeDraft, label: event.target.value })} />
            <TextField select label="Tipo" value={placeDraft.type} onChange={(event) => setPlaceDraft({ ...placeDraft, type: event.target.value as PlaceDraft['type'] })}><MenuItem value="venue">Venue</MenuItem><MenuItem value="hotel">Hotel</MenuItem><MenuItem value="airport">Aeropuerto</MenuItem><MenuItem value="pickup">Recogida</MenuItem><MenuItem value="custom">Otro</MenuItem></TextField>
            <TextField label="Dirección" value={placeDraft.address} onChange={(event) => setPlaceDraft({ ...placeDraft, address: event.target.value })} />
            <TextField label="Latitud" type="number" value={placeDraft.latitude} onChange={(event) => setPlaceDraft({ ...placeDraft, latitude: event.target.value })} />
            <TextField label="Longitud" type="number" value={placeDraft.longitude} onChange={(event) => setPlaceDraft({ ...placeDraft, longitude: event.target.value })} />
            <TextField label="Instrucciones de acceso" value={placeDraft.instructions} onChange={(event) => setPlaceDraft({ ...placeDraft, instructions: event.target.value })} />
            <TextField label="Contacto" value={placeDraft.contactName} onChange={(event) => setPlaceDraft({ ...placeDraft, contactName: event.target.value })} />
            <TextField type="tel" label="Teléfono" value={placeDraft.contactPhone} onChange={(event) => setPlaceDraft({ ...placeDraft, contactPhone: event.target.value })} />
          </Box>
          <Stack direction="row" spacing={1}><Button variant="contained" startIcon={<AddLocationAltIcon />} onClick={() => placeMutation.mutate()} disabled={placeMutation.isPending}>{editingPlaceId ? 'Actualizar lugar' : 'Guardar lugar'}</Button>{editingPlaceId && <Button onClick={() => { setEditingPlaceId(''); setPlaceDraft(emptyPlace()); }}>Cancelar</Button>}</Stack>
        </Stack></CardContent></Card>}

        <PlacesSection
          places={plan?.elgPlaces ?? []}
          canEdit={canEdit}
          onEdit={startEditingPlace}
          onDelete={(placeId) => deletePlaceMutation.mutate(placeId)}
        />

        {canEdit && <Card className="no-print" variant="outlined"><CardContent><Stack spacing={1.5}>
          <Typography variant="h6">{editingActivity ? 'Editar actividad' : 'Añadir actividad'}</Typography>
          <Box sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', md: 'repeat(3, 1fr)' }, gap: 1 }}>
            <TextField select label="Tipo" value={activityDraft.type} onChange={(event) => {
              const type = event.target.value as LogisticsActivityType;
              setActivityDraft({ ...activityDraft, type, travelMode: type === 'travel' ? defaultMode : activityDraft.travelMode });
            }}><MenuItem value="task">Tarea</MenuItem><MenuItem value="milestone">Hito</MenuItem><MenuItem value="wait">Espera / holgura</MenuItem><MenuItem value="travel">Traslado</MenuItem></TextField>
            <TextField label="Título" value={activityDraft.title} onChange={(event) => setActivityDraft({ ...activityDraft, title: event.target.value })} sx={{ gridColumn: { md: 'span 2' } }} />
            <TextField label="Inicio" type="datetime-local" value={activityDraft.start} onChange={(event) => setActivityDraft({ ...activityDraft, start: event.target.value })} InputLabelProps={{ shrink: true }} />
            {activityDraft.type !== 'milestone' && <TextField label="Fin" type="datetime-local" value={activityDraft.end} onChange={(event) => setActivityDraft({ ...activityDraft, end: event.target.value })} InputLabelProps={{ shrink: true }} />}
            <TextField select label="Prioridad" value={activityDraft.priority} onChange={(event) => setActivityDraft({ ...activityDraft, priority: event.target.value as ActivityDraft['priority'] })}><MenuItem value="low">Baja</MenuItem><MenuItem value="normal">Normal</MenuItem><MenuItem value="high">Alta</MenuItem><MenuItem value="critical">Crítica</MenuItem></TextField>
            {activityDraft.type === 'travel' ? <>
              <TextField select label="Origen" value={activityDraft.originPlaceId} onChange={(event) => setActivityDraft({ ...activityDraft, originPlaceId: event.target.value })}>{plan?.elgPlaces.map((place) => <MenuItem key={place.elpId} value={place.elpId ?? ''}>{place.elpLabel}</MenuItem>)}</TextField>
              <TextField select label="Destino" value={activityDraft.destinationPlaceId} onChange={(event) => setActivityDraft({ ...activityDraft, destinationPlaceId: event.target.value })}>{plan?.elgPlaces.map((place) => <MenuItem key={place.elpId} value={place.elpId ?? ''}>{place.elpLabel}</MenuItem>)}</TextField>
              <TextField select label="Modo" value={activityDraft.travelMode} onChange={(event) => setActivityDraft({ ...activityDraft, travelMode: event.target.value as LogisticsTravelMode })}><MenuItem value="drive">Vehículo</MenuItem><MenuItem value="walk">Caminata</MenuItem><MenuItem value="bicycle">Bicicleta</MenuItem><MenuItem value="two_wheeler">Motocicleta</MenuItem><MenuItem value="transit">Transporte público</MenuItem></TextField>
              <TextField label="Holgura manual (min, opcional)" type="number" value={activityDraft.bufferMinutes} onChange={(event) => setActivityDraft({ ...activityDraft, bufferMinutes: event.target.value })} />
            </> : <TextField select label="Lugar" value={activityDraft.placeId} onChange={(event) => setActivityDraft({ ...activityDraft, placeId: event.target.value })}><MenuItem value="">Sin lugar</MenuItem>{plan?.elgPlaces.map((place) => <MenuItem key={place.elpId} value={place.elpId ?? ''}>{place.elpLabel}</MenuItem>)}</TextField>}
            <UserSelector
              value={activityAssignee}
              onChange={(party) => {
                setActivityAssignee(party);
                setActivityDraft({
                  ...activityDraft,
                  assigneePartyId: party ? String(party.partyId) : '',
                  externalName: party ? '' : activityDraft.externalName,
                });
              }}
              field={{ label: 'Responsable TDF', helperText: 'Busca y selecciona una cuenta TDF; nunca ingreses un ID manualmente.' }}
              search={{ context: 'crm_assignment' }}
            />
            <TextField label="Responsable externo" value={activityDraft.externalName} onChange={(event) => {
              const externalName = event.target.value;
              setActivityAssignee(null);
              setActivityDraft({ ...activityDraft, externalName, assigneePartyId: externalName ? '' : activityDraft.assigneePartyId });
            }} />
            <TextField type="tel" label="Teléfono externo" value={activityDraft.externalPhone} onChange={(event) => setActivityDraft({ ...activityDraft, externalPhone: event.target.value })} />
            <TextField label="Email externo" value={activityDraft.externalEmail} onChange={(event) => setActivityDraft({ ...activityDraft, externalEmail: event.target.value })} />
            <TextField label="Dependencias (IDs separados por coma)" value={activityDraft.dependencyIds} onChange={(event) => setActivityDraft({ ...activityDraft, dependencyIds: event.target.value })} />
            <TextField label="Notas e instrucciones" multiline minRows={2} value={activityDraft.notes} onChange={(event) => setActivityDraft({ ...activityDraft, notes: event.target.value })} sx={{ gridColumn: { md: 'span 2' } }} />
          </Box>
          <Stack direction="row" spacing={1}>
            <Button variant="contained" onClick={() => activityMutation.mutate()} disabled={activityMutation.isPending}>{activityMutation.isPending ? <CircularProgress size={20} /> : editingActivity ? 'Actualizar actividad' : 'Añadir al cronograma'}</Button>
            {editingActivity && <Button onClick={() => { setEditingActivity(null); setActivityAssignee(null); setActivityDraft(emptyActivity(eventQuery.data?.eventStart, eventQuery.data?.eventEnd, timezone, defaultMode)); }}>Cancelar</Button>}
          </Stack>
        </Stack></CardContent></Card>}

        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} className="no-print">
          <TextField select size="small" label="Estado" value={statusFilter} onChange={(event) => setStatusFilter(event.target.value)}><MenuItem value="all">Todos</MenuItem><MenuItem value="planned">Planificado</MenuItem><MenuItem value="confirmed">Confirmado</MenuItem><MenuItem value="in_progress">En curso</MenuItem><MenuItem value="completed">Completado</MenuItem><MenuItem value="cancelled">Cancelado</MenuItem></TextField>
          <TextField select size="small" label="Tipo" value={typeFilter} onChange={(event) => setTypeFilter(event.target.value)}><MenuItem value="all">Todos</MenuItem><MenuItem value="task">Tareas</MenuItem><MenuItem value="milestone">Hitos</MenuItem><MenuItem value="wait">Esperas</MenuItem><MenuItem value="travel">Traslados</MenuItem></TextField>
          <TextField select size="small" label="Responsable" value={assigneeFilter} onChange={(event) => setAssigneeFilter(event.target.value)} sx={{ minWidth: 180 }}><MenuItem value="all">Todos</MenuItem>{assigneeOptions.map(([value, label]) => <MenuItem key={value} value={value}>{label}</MenuItem>)}</TextField>
          {canEdit && <Button startIcon={<RefreshIcon />} onClick={() => verifyMutation.mutate()} disabled={verifyMutation.isPending}>Verificar todos los traslados</Button>}
        </Stack>

        <Stack spacing={1.25}>
          <Typography variant="h5">Cronograma</Typography>
          {visibleActivities.length ? visibleActivities.map((activity) => {
            const verification = activity.eacLatestVerification;
            const origin = activity.eacOriginPlaceId ? placesById.get(activity.eacOriginPlaceId) : undefined;
            const destination = activity.eacDestinationPlaceId ? placesById.get(activity.eacDestinationPlaceId) : undefined;
            const routeUrl = origin && destination ? `https://www.google.com/maps/dir/?api=1&origin=${origin.elpLatitude},${origin.elpLongitude}&destination=${destination.elpLatitude},${destination.elpLongitude}` : '';
            return <Card key={activity.eacId} variant="outlined" sx={{ borderLeft: 5, borderLeftColor: activity.eacPriority === 'critical' ? 'error.main' : activity.eacPriority === 'high' ? 'warning.main' : 'primary.main' }}><CardContent><Stack spacing={1}>
              <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1}>
                <Box><Typography variant="caption" color="text.secondary">{new Date(activity.eacStart).toLocaleString(locale, { timeZone: plan?.elgSettings.elsTimezone ?? timezone })}{activity.eacEnd ? ` – ${new Date(activity.eacEnd).toLocaleString(locale, { timeZone: plan?.elgSettings.elsTimezone ?? timezone })}` : ''}</Typography><Typography variant="h6">{activity.eacTitle}</Typography></Box>
                <Stack direction="row" spacing={0.5} flexWrap="wrap" useFlexGap><Chip size="small" label={activity.eacType} /><Chip size="small" label={activity.eacStatus} /><Chip size="small" label={activity.eacPriority} /></Stack>
              </Stack>
              {activity.eacType === 'travel' && <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}><DirectionsCarIcon color="action" /><Typography>{origin?.elpLabel ?? 'Origen'} → {destination?.elpLabel ?? 'Destino'}</Typography>{routeUrl && <Link href={routeUrl} target="_blank" rel="noreferrer">Abrir ruta</Link>}</Stack>}
              {activity.eacPlaceId && <Typography variant="body2">Lugar: {placesById.get(activity.eacPlaceId)?.elpLabel ?? activity.eacPlaceId}</Typography>}
              {activity.eacAssignments.length > 0 && <Typography variant="body2">Responsables: {activity.eacAssignments.map((assignment) => assignment.elaDisplayName ?? assignment.elaExternalName ?? assignment.elaPartyId).filter(Boolean).join(', ')}</Typography>}
              {activity.eacNotes && <Typography sx={{ whiteSpace: 'pre-wrap' }}>{activity.eacNotes}</Typography>}
              {verification && <Alert icon={<RouteIcon />} severity={verification.ervVerdict === 'feasible' ? 'success' : verification.ervVerdict === 'tight' ? 'warning' : 'error'}>
                Ruta {verification.ervVerdict}: estimado {secondsLabel(verification.ervDurationSeconds)}, holgura {secondsLabel(verification.ervBufferSeconds)}, reservado {secondsLabel(verification.ervAllocatedSeconds)}{verification.ervDistanceMeters ? ` · ${(verification.ervDistanceMeters / 1000).toFixed(1)} km` : ''}.
              </Alert>}
              {canEdit && <Stack className="no-print" direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                <TextField select size="small" label="Estado" value={activity.eacStatus} onChange={(event) => updateActivityMutation.mutate({ activity, status: event.target.value as EventLogisticsActivityDTO['eacStatus'] })}><MenuItem value="planned">Planificado</MenuItem><MenuItem value="confirmed">Confirmado</MenuItem><MenuItem value="in_progress">En curso</MenuItem><MenuItem value="completed">Completado</MenuItem><MenuItem value="cancelled">Cancelado</MenuItem></TextField>
                <Button size="small" startIcon={<EditOutlinedIcon />} onClick={() => startEditingActivity(activity)}>Editar</Button>
                {activity.eacType === 'travel' && <Button size="small" startIcon={<RefreshIcon />} onClick={() => verifyActivityMutation.mutate(String(activity.eacId))} disabled={verifyActivityMutation.isPending}>Recalcular</Button>}
                <Button size="small" color="error" startIcon={<DeleteOutlineIcon />} onClick={() => deleteActivityMutation.mutate(String(activity.eacId))}>Eliminar</Button>
              </Stack>}
            </Stack></CardContent></Card>;
          }) : <Alert severity="info">No hay actividades que coincidan con los filtros.</Alert>}
        </Stack>
      </Stack>
    </PageShell>
  );
}
