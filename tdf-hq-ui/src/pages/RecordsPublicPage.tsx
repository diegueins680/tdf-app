import {
  Alert,
  Avatar,
  Box,
  Button,
  Card,
  CardContent,
  CardMedia,
  Chip,
  Container,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Grid,
  Link as MuiLink,
  Autocomplete,
  CircularProgress,
  MenuItem,
  Stack,
  TextField,
  Typography,
  IconButton,
  Tooltip,
} from '@mui/material';
import { useEffect, useMemo, useState } from 'react';
import { Link as RouterLink } from 'react-router-dom';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { DateTime } from 'luxon';
import PublicBrandBar from '../components/PublicBrandBar';
import { useCmsContent, useCmsContents } from '../hooks/useCmsContent';
import { Bookings } from '../api/bookings';
import { Rooms } from '../api/rooms';
import { Parties } from '../api/parties';
import { Admin } from '../api/admin';
import { Services } from '../api/services';
import { Engineers } from '../api/engineers';
import { setTransientApiToken, useSession } from '../session/SessionContext';
import { canAccessPath } from '../utils/accessControl';
import { STUDIO_WHATSAPP_URL } from '../config/appConfig';
import { extractYoutubeVideoId } from '../utils/media';
import EditIcon from '@mui/icons-material/Edit';
import PlayCircleOutlineIcon from '@mui/icons-material/PlayCircleOutline';

const BOOKING_ZONE = 'America/Bogota';
const DEFAULT_DURATION = 120;
const FALLBACK_SERVICE_OPTIONS = [
  { value: 'Vocal recording', label: 'Grabación vocal' },
  { value: 'Band recording', label: 'Grabación de banda' },
  { value: 'Band rehearsal', label: 'Ensayo de banda' },
  { value: 'DJ booth rental', label: 'Cabina DJ' },
  { value: 'Podcast / Voiceover', label: 'Podcast / Locución' },
  { value: 'Producción musical', label: 'Producción musical' },
  { value: 'Mezcla / Post', label: 'Mezcla / Post' },
] as const;
const FALLBACK_SERVICE_LABELS = Object.fromEntries(
  FALLBACK_SERVICE_OPTIONS.map((option) => [option.value, option.label]),
) as Record<string, string>;

const parseDurationMinutes = (raw: string, fallback: number): number => {
  const parsed = Number(raw);
  if (!Number.isFinite(parsed)) return fallback;
  const rounded = Math.round(parsed);
  return Math.max(30, rounded);
};

const toObject = (value: unknown): Record<string, unknown> | null =>
  value && typeof value === 'object' && !Array.isArray(value) ? (value as Record<string, unknown>) : null;

const toText = (value: unknown): string | null => {
  if (typeof value === 'string') return value;
  if (typeof value === 'number' && Number.isFinite(value)) return String(value);
  return null;
};

const toNonEmptyText = (value: unknown): string | null => {
  const text = toText(value);
  if (!text) return null;
  const trimmed = text.trim();
  return trimmed.length > 0 ? trimmed : null;
};

const toFiniteNumber = (value: unknown): number | null => {
  if (typeof value === 'number' && Number.isFinite(value)) return value;
  if (typeof value !== 'string') return null;
  const trimmed = value.trim();
  if (!trimmed) return null;
  const parsed = Number(trimmed);
  return Number.isFinite(parsed) ? parsed : null;
};

const overlap = (startA: DateTime, endA: DateTime, startB: DateTime, endB: DateTime) =>
  endA > startB && startA < endB;

function BookingRequestDialog({
  open,
  onClose,
  hasToken,
}: {
  open: boolean;
  onClose: () => void;
  hasToken: boolean;
}) {
  const qc = useQueryClient();
  const [contactName, setContactName] = useState('');
  const [email, setEmail] = useState('');
  const [phone, setPhone] = useState('');
  const [serviceType, setServiceType] = useState<string>('');
  const serviceCatalogQuery = useQuery({
    queryKey: ['service-catalog-public'],
    queryFn: () => Services.listPublic(),
    staleTime: 5 * 60 * 1000,
  });
  const serviceOptions = useMemo(() => {
    const names = (serviceCatalogQuery.data ?? [])
      .filter((svc) => svc.scActive !== false)
      .map((svc) => svc.scName);
    return names.length > 0 ? names : FALLBACK_SERVICE_OPTIONS.map((option) => option.value);
  }, [serviceCatalogQuery.data]);
  const defaultServiceType = serviceOptions[0] ?? FALLBACK_SERVICE_OPTIONS[0]?.value ?? 'Recording';
  const selectedServiceLabel = FALLBACK_SERVICE_LABELS[serviceType] ?? serviceType;
  useEffect(() => {
    if (!serviceType && defaultServiceType) {
      setServiceType(defaultServiceType);
    } else if (serviceOptions.length > 0 && serviceType && !serviceOptions.includes(serviceType)) {
      setServiceType(defaultServiceType);
    }
  }, [serviceOptions, serviceType, defaultServiceType]);
  const initialStartValue = useMemo(
    () =>
      DateTime.now()
        .setZone(BOOKING_ZONE)
        .plus({ days: 1 })
        .set({ hour: 10, minute: 0, second: 0, millisecond: 0 })
        .toFormat("yyyy-LL-dd'T'HH:mm"),
    [],
  );
  const [startInput, setStartInput] = useState<string>(initialStartValue);
  const [duration, setDuration] = useState<number>(DEFAULT_DURATION);
  const [notes, setNotes] = useState('');
  const [selectedRoomId, setSelectedRoomId] = useState<string>('');
  const [engineerName, setEngineerName] = useState<string>('');
  const [formError, setFormError] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);

  const bookingsQuery = useQuery({
    queryKey: ['bookings'],
    queryFn: () => Bookings.list(),
    enabled: open && hasToken,
  });
  const roomsQuery = useQuery({
    queryKey: ['rooms'],
    queryFn: () => Rooms.list(),
    enabled: open && hasToken,
  });
  const engineersQuery = useQuery({
    queryKey: ['engineers-public'],
    queryFn: () => Engineers.listPublic(),
    enabled: open && hasToken,
    staleTime: 5 * 60 * 1000,
  });

  const engineerOptions = useMemo(() => {
    const engineers = new Set<string>();
    (engineersQuery.data ?? []).forEach((engineer) => {
      const name = engineer.peName?.trim();
      if (name) engineers.add(name);
    });
    (bookingsQuery.data ?? []).forEach((booking) => {
      const directName = booking.engineerName?.trim();
      if (directName) engineers.add(directName);
      booking.resources.forEach((res) => {
        if (res.brRole.toLowerCase().includes('engineer')) {
          const name = res.brRoomName?.trim();
          if (name) engineers.add(name);
        }
      });
    });
    return Array.from(engineers).sort((a, b) => a.localeCompare(b));
  }, [bookingsQuery.data, engineersQuery.data]);

  const selectedEngineerPartyId = useMemo(() => {
    const engineerLower = engineerName.trim().toLowerCase();
    if (!engineerLower) return null;
    const matched = (engineersQuery.data ?? []).find((engineer) => engineer.peName.trim().toLowerCase() === engineerLower);
    return matched?.peId ?? null;
  }, [engineerName, engineersQuery.data]);

  const parsedStart = useMemo(
    () => DateTime.fromFormat(startInput, "yyyy-LL-dd'T'HH:mm", { zone: BOOKING_ZONE }),
    [startInput],
  );
  const parsedEnd = useMemo(() => parsedStart.plus({ minutes: duration }), [parsedStart, duration]);
  const isTimeValid = parsedStart.isValid && parsedEnd.isValid && parsedEnd > parsedStart;

  const relevantConflicts = useMemo(() => {
    const bookings = bookingsQuery.data ?? [];
    if (!isTimeValid) return [];
    return bookings
      .filter((booking) => {
        const bStart = DateTime.fromISO(booking.startsAt);
        const bEnd = DateTime.fromISO(booking.endsAt);
        if (!bStart.isValid || !bEnd.isValid) return false;
        return overlap(parsedStart.toUTC(), parsedEnd.toUTC(), bStart, bEnd);
      })
      .sort((a, b) => DateTime.fromISO(a.startsAt).toMillis() - DateTime.fromISO(b.startsAt).toMillis());
  }, [bookingsQuery.data, parsedStart, parsedEnd, isTimeValid]);

  const selectedRoom = useMemo(
    () => (roomsQuery.data ?? []).find((room) => room.roomId === selectedRoomId) ?? null,
    [roomsQuery.data, selectedRoomId],
  );

  const roomConflicts = useMemo(() => {
    if (!selectedRoomId || !isTimeValid) return [];
    return relevantConflicts.filter((booking) =>
      booking.resources.some(
        (res) =>
          res.brRoomId === selectedRoomId ||
          (selectedRoom?.rName && res.brRoomName?.toLowerCase() === selectedRoom.rName.toLowerCase()),
      ),
    );
  }, [selectedRoomId, relevantConflicts, selectedRoom, isTimeValid]);

  const engineerConflicts = useMemo(() => {
    if (!engineerName.trim() || !isTimeValid) return [];
    const engineerLower = engineerName.trim().toLowerCase();
    return relevantConflicts.filter((booking) =>
      (selectedEngineerPartyId != null && booking.engineerPartyId === selectedEngineerPartyId) ||
      booking.engineerName?.trim().toLowerCase() === engineerLower ||
      booking.resources.some(
        (res) =>
          res.brRole.toLowerCase().includes('engineer') &&
          res.brRoomName &&
          res.brRoomName.toLowerCase() === engineerLower,
      ),
    );
  }, [engineerName, relevantConflicts, isTimeValid, selectedEngineerPartyId]);

  const nextRoomConflict = roomConflicts[0] ?? null;
  const nextEngineerConflict = engineerConflicts[0] ?? null;

  const isDirty = useMemo(
    () =>
      Boolean(
        contactName ||
          email ||
          phone ||
          notes ||
          selectedRoomId ||
          engineerName ||
          serviceType !== defaultServiceType ||
          startInput !== initialStartValue ||
          duration !== DEFAULT_DURATION,
      ),
    [contactName, email, phone, notes, selectedRoomId, engineerName, serviceType, startInput, duration, initialStartValue, defaultServiceType],
  );

  const formatRange = (isoStart: string, isoEnd: string) =>
    `${DateTime.fromISO(isoStart).setZone(BOOKING_ZONE).toFormat("dd LLL yyyy, HH:mm")} - ${DateTime.fromISO(isoEnd)
      .setZone(BOOKING_ZONE)
      .toFormat("HH:mm")}`;

  const mutation = useMutation({
    mutationFn: async () => {
      if (!parsedStart.isValid || !parsedEnd.isValid) {
        throw new Error('Revisa la fecha y hora seleccionadas.');
      }
      if (!contactName.trim() || !email.trim()) {
        throw new Error('Agrega nombre y correo para continuar.');
      }
      const party = await Parties.create({
        cDisplayName: contactName.trim(),
        cIsOrg: false,
        cPrimaryEmail: email.trim(),
        cPrimaryPhone: phone.trim() || null,
      });
      const augmentedNotes = [
        'Solicitud web (Lanzamientos)',
        `Servicio: ${selectedServiceLabel}`,
        selectedRoom?.rName ? `Sala: ${selectedRoom.rName}` : null,
        engineerName.trim() ? `Ingeniero solicitado: ${engineerName.trim()}` : null,
        phone ? `Teléfono: ${phone}` : null,
        notes ? `Notas: ${notes}` : null,
      ]
        .filter(Boolean)
        .join(' · ');

      if (email.trim()) {
        try {
          await Admin.createUser({
            partyId: party.partyId,
            username: email.trim(),
            roles: ['Customer'],
          });
        } catch (err) {
          console.warn('No se pudo crear el usuario, continuando con la reserva', err);
        }
      }

      const startIso = parsedStart.toUTC().toISO();
      const endIso = parsedEnd.toUTC().toISO();

      return Bookings.create({
        cbTitle: `${selectedServiceLabel} - ${contactName}`,
        cbStartsAt: startIso ?? '',
        cbEndsAt: endIso ?? '',
        cbStatus: 'Tentative',
        cbNotes: augmentedNotes || null,
        cbServiceType: serviceType,
        cbPartyId: party.partyId,
        cbEngineerPartyId: selectedEngineerPartyId,
        cbEngineerName: engineerName.trim() || null,
        cbResourceIds: selectedRoomId ? [selectedRoomId] : null,
      });
    },
    onSuccess: (created) => {
      setSuccessMessage(`Sesión creada para ${created.title} el ${DateTime.fromISO(created.startsAt).setZone(BOOKING_ZONE).toFormat("dd LLL yyyy, HH:mm")}.`);
      setFormError(null);
      void qc.invalidateQueries({ queryKey: ['bookings'] });
    },
    onError: (err) => {
      setFormError(err instanceof Error ? err.message : 'No se pudo reservar la sesión.');
    },
  });

  const handleSubmit = (evt: React.FormEvent) => {
    evt.preventDefault();
    if (!hasToken) {
      setFormError('La reserva en línea no está disponible ahora. Escríbenos y te confirmamos manualmente.');
      return;
    }
    if (!parsedStart.isValid || !parsedEnd.isValid || parsedEnd <= parsedStart) {
      setFormError('Revisa las fechas y duración.');
      return;
    }
    if (!Number.isFinite(duration) || duration <= 0) {
      setFormError('La duración debe ser mayor a 0 minutos.');
      return;
    }
    if (roomConflicts.length > 0) {
      const first = roomConflicts[0]!;
      setFormError(
        `La sala ${selectedRoom?.rName ?? ''} no está disponible (${formatRange(first.startsAt, first.endsAt)}). Elige otro horario o sala.`,
      );
      return;
    }
    if (engineerConflicts.length > 0) {
      const first = engineerConflicts[0]!;
      setFormError(
        `El ingeniero ${engineerName} tiene un cruce (${formatRange(first.startsAt, first.endsAt)}). Ajusta el horario o elige otro ingeniero.`,
      );
      return;
    }
    if (relevantConflicts.length > 0) {
      const first = relevantConflicts[0]!;
      setFormError(`Hay un cruce con otra reserva (${first.title}) en ${formatRange(first.startsAt, first.endsAt)}. Ajusta la hora.`);
      return;
    }
    mutation.mutate();
  };

  const resetAndClose = () => {
    if (mutation.isPending) return;
    setFormError(null);
    setSuccessMessage(null);
    onClose();
  };

  useEffect(() => {
    if (!(open && isDirty && !successMessage)) return;
    const handleBeforeUnload = (event: BeforeUnloadEvent) => {
      event.preventDefault();
      event.returnValue = '';
    };
    window.addEventListener('beforeunload', handleBeforeUnload);
    return () => window.removeEventListener('beforeunload', handleBeforeUnload);
  }, [open, isDirty, successMessage]);

  return (
    <Dialog open={open} onClose={resetAndClose} maxWidth="md" fullWidth>
      <form onSubmit={handleSubmit}>
        <DialogTitle>Reservar sesión en el estudio</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={2}>
            {!hasToken && (
              <Alert
                severity="warning"
                action={(
                  <Button
                    color="inherit"
                    size="small"
                    component="a"
                    href={STUDIO_WHATSAPP_URL}
                    target="_blank"
                    rel="noreferrer"
                    sx={{ textTransform: 'none' }}
                  >
                    WhatsApp
                  </Button>
                )}
              >
                La reserva directa no está habilitada en este momento. Puedes revisar lanzamientos y escribirnos para coordinar tu sesión.
              </Alert>
            )}
            {successMessage && <Alert severity="success">{successMessage}</Alert>}
            {roomConflicts.length > 0 && (
              <Alert severity="warning">
                La sala {selectedRoom?.rName ?? ''} ya está reservada en este horario. Ajusta la hora o elige otra sala.
              </Alert>
            )}
            {engineerConflicts.length > 0 && (
              <Alert severity="warning">
                El ingeniero {engineerName} ya tiene sesión en ese horario. Ajusta la hora o elige otra persona.
              </Alert>
            )}
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
              <TextField
                label="Nombre / artista"
                value={contactName}
                onChange={(e) => setContactName(e.target.value)}
                required
                fullWidth
              />
              <TextField
                label="Correo"
                type="email"
                value={email}
                onChange={(e) => setEmail(e.target.value)}
                required
                fullWidth
              />
            </Stack>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
              <TextField
                label="Teléfono"
                value={phone}
                onChange={(e) => setPhone(e.target.value)}
                fullWidth
              />
              <TextField
                label="Servicio"
                select
                value={serviceType}
                onChange={(e) => setServiceType(e.target.value)}
                fullWidth
                helperText="Elige la opción que mejor describe tu sesión para prepararla mejor."
              >
                {serviceOptions.map((opt) => (
                  <MenuItem key={opt} value={opt}>
                    {FALLBACK_SERVICE_LABELS[opt] ?? opt}
                  </MenuItem>
                ))}
              </TextField>
            </Stack>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
              <TextField
                label="Inicio"
                type="datetime-local"
                value={startInput}
                onChange={(e) => setStartInput(e.target.value)}
                fullWidth
                InputLabelProps={{ shrink: true }}
              />
              <TextField
                label="Duración (min)"
                type="number"
                value={duration}
                onChange={(e) => setDuration(parseDurationMinutes(e.target.value, duration))}
                fullWidth
                inputProps={{ min: 30, step: 15 }}
              />
            </Stack>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
              <TextField
                label="Sala"
                select
                value={selectedRoomId}
                onChange={(e) => setSelectedRoomId(e.target.value)}
                fullWidth
                SelectProps={{ native: true }}
                helperText={
                  selectedRoom
                    ? nextRoomConflict
                      ? `Cruce: ${formatRange(nextRoomConflict.startsAt, nextRoomConflict.endsAt)}`
                      : `Seleccionaste ${selectedRoom.rName} (sin choques en la ventana elegida)`
                    : roomsQuery.data?.length
                      ? 'Elige una sala para validar disponibilidad.'
                      : undefined
                }
              >
                <option value="">Cualquier sala disponible</option>
                {(roomsQuery.data ?? []).map((room) => (
                  <option key={room.roomId} value={room.roomId}>
                    {room.rName}
                  </option>
                ))}
              </TextField>
              <Autocomplete
                options={engineerOptions}
                freeSolo
                value={engineerName || null}
                onChange={(_, value) => setEngineerName(value ?? '')}
                onInputChange={(_, value, reason) => {
                  if (reason === 'clear') setEngineerName('');
                  if (reason === 'input') setEngineerName(value);
                }}
                renderInput={(params) => (
                  <TextField
                    {...params}
                    label="Ingeniero (opcional)"
                    placeholder={engineerOptions.length ? 'Selecciona o escribe para validar choques' : 'Escribe el nombre del ingeniero'}
                    InputProps={{
                      ...params.InputProps,
                      endAdornment: (
                        <>
                          {engineersQuery.isFetching ? <CircularProgress size={16} /> : null}
                          {params.InputProps.endAdornment}
                        </>
                      ),
                    }}
                    helperText={
                      engineerName
                        ? nextEngineerConflict
                          ? `Cruce: ${formatRange(nextEngineerConflict.startsAt, nextEngineerConflict.endsAt)}`
                          : 'Sin choques en este horario.'
                        : engineersQuery.isError
                          ? 'Catálogo no disponible; puedes escribir el nombre manualmente.'
                          : 'Selecciona un ingeniero para asegurar disponibilidad.'
                    }
                    fullWidth
                  />
                )}
              />
            </Stack>
            <TextField
              label="Notas adicionales"
              multiline
              minRows={3}
              value={notes}
              onChange={(e) => setNotes(e.target.value)}
              fullWidth
            />
            {formError && <Alert severity="error">{formError}</Alert>}
            {relevantConflicts.length > 0 && !formError && (
              <Alert severity="warning">
                Hay cruces en el horario elegido. Ajusta la hora para evitar conflictos.
              </Alert>
            )}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={resetAndClose} disabled={mutation.isPending}>
            {hasToken ? 'Cancelar' : 'Cerrar'}
          </Button>
          {hasToken ? (
            <Button variant="contained" type="submit" disabled={mutation.isPending}>
              {mutation.isPending ? 'Reservando…' : 'Confirmar reserva'}
            </Button>
          ) : (
            <Button
              variant="contained"
              component="a"
              href={STUDIO_WHATSAPP_URL}
              target="_blank"
              rel="noreferrer"
              sx={{ textTransform: 'none' }}
            >
              Escribir por WhatsApp
            </Button>
          )}
        </DialogActions>
      </form>
    </Dialog>
  );
}

const SectionTitle = ({
  title,
  kicker,
  actions,
}: {
  title: string;
  kicker?: string;
  actions?: React.ReactNode;
}) => (
  <Stack spacing={1} direction="row" alignItems="center" justifyContent="space-between" sx={{ mb: 2 }}>
    <Stack direction="row" spacing={1} alignItems="center">
      {kicker && (
        <Chip
          label={kicker}
          size="small"
          sx={{ bgcolor: 'rgba(255,255,255,0.12)', color: 'primary.contrastText', fontWeight: 600 }}
        />
      )}
      <Typography variant="h4" sx={{ fontWeight: 800, letterSpacing: '-0.02em' }}>
        {title}
      </Typography>
    </Stack>
    {actions && <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>{actions}</Box>}
  </Stack>
);

const GradientCard = ({
  title,
  children,
  actions,
}: {
  title: string;
  children: React.ReactNode;
  actions?: React.ReactNode;
}) => (
  <Box
    sx={{
      p: 3,
      borderRadius: 3,
      background: 'linear-gradient(180deg, rgba(10,16,28,0.84), rgba(10,16,28,0.72))',
      backdropFilter: 'blur(14px)',
      border: '1px solid rgba(255,255,255,0.1)',
      boxShadow: '0 30px 60px rgba(0,0,0,0.35)',
    }}
  >
    <Stack direction="row" justifyContent="space-between" alignItems="center" sx={{ mb: 2 }}>
      <Typography variant="h6" sx={{ fontWeight: 700 }}>
        {title}
      </Typography>
      {actions}
    </Stack>
    {children}
  </Box>
);

interface RecordingItem {
  title: string;
  artist: string;
  description: string;
  image: string;
  recordedAt: string;
  vibe: string;
  youtubeId?: string;
  url?: string;
  duration?: string;
  sortOrder: number;
}
interface ReleaseLink {
  platform: string;
  url: string;
  accent: string;
}

interface ReleaseItem {
  title: string;
  artist: string;
  blurb: string;
  cover: string;
  releasedOn: string;
  links: ReleaseLink[];
  primaryUrl?: string;
  duration?: string;
  sortOrder: number;
}
interface SessionItem {
  title: string;
  guests: string;
  youtubeId: string;
  embedUrl: string;
  duration: string;
  description: string;
  url: string;
  sortOrder: number;
}

const mapCmsSessionPayload = (
  payload: unknown,
  fallbackTitle: string | null,
  fallbackSortOrder: number,
): SessionItem | null => {
  const data = toObject(payload);
  if (!data) return null;
  const explicitId =
    toNonEmptyText(data['youtubeId']) ??
    toNonEmptyText(data['youtubeID']) ??
    toNonEmptyText(data['id']);
  const url = toNonEmptyText(data['url']) ?? toNonEmptyText(data['youtubeUrl']);
  const youtubeId = explicitId ?? extractYoutubeVideoId(url);
  if (!youtubeId) return null;
  const sessionUrl = url ?? `https://www.youtube.com/watch?v=${youtubeId}`;
  return {
    youtubeId,
    embedUrl: `https://www.youtube.com/embed/${youtubeId}`,
    title: toNonEmptyText(data['title']) ?? fallbackTitle ?? 'Sesión en vivo TDF',
    duration: toText(data['duration']) ?? '',
    guests: toText(data['guests']) ?? toText(data['artist']) ?? '',
    description: toText(data['description']) ?? '',
    url: sessionUrl,
    sortOrder: toFiniteNumber(data['sortOrder']) ?? toFiniteNumber(data['order']) ?? fallbackSortOrder,
  };
};

const sortSessions = (items: SessionItem[]): SessionItem[] =>
  [...items].sort((a, b) => a.sortOrder - b.sortOrder || a.title.localeCompare(b.title));

const youtubeThumbnail = (youtubeId: string): string =>
  `https://i.ytimg.com/vi/${youtubeId}/hqdefault.jpg`;

const mapCmsRecordingPayload = (
  payload: unknown,
  fallbackTitle: string | null,
  fallbackSortOrder: number,
): RecordingItem | null => {
  const data = toObject(payload);
  if (!data) return null;
  const explicitId =
    toNonEmptyText(data['youtubeId']) ??
    toNonEmptyText(data['youtubeID']) ??
    toNonEmptyText(data['videoId']) ??
    toNonEmptyText(data['id']);
  const url = toNonEmptyText(data['url']) ?? toNonEmptyText(data['youtubeUrl']);
  const youtubeId = explicitId ?? extractYoutubeVideoId(url);
  const title = toNonEmptyText(data['title']) ?? toNonEmptyText(data['name']) ?? fallbackTitle;
  const image =
    toNonEmptyText(data['image']) ??
    toNonEmptyText(data['thumbnail']) ??
    toNonEmptyText(data['thumbnailUrl']) ??
    (youtubeId ? youtubeThumbnail(youtubeId) : null);
  if (!title || !image) return null;
  return {
    title,
    artist: toText(data['artist']) ?? toText(data['guests']) ?? toText(data['channel']) ?? '',
    recordedAt:
      toText(data['recordedAt']) ??
      toText(data['date']) ??
      toText(data['publishedAt']) ??
      toText(data['duration']) ??
      '',
    duration: toText(data['duration']) ?? undefined,
    vibe: toText(data['vibe']) ?? toText(data['tag']) ?? toText(data['series']) ?? 'Video',
    description: toText(data['description']) ?? '',
    image,
    youtubeId: youtubeId ?? undefined,
    url: url ?? (youtubeId ? `https://www.youtube.com/watch?v=${youtubeId}` : undefined),
    sortOrder: toFiniteNumber(data['sortOrder']) ?? toFiniteNumber(data['order']) ?? fallbackSortOrder,
  };
};

const sortRecordings = (items: RecordingItem[]): RecordingItem[] =>
  [...items].sort((a, b) => a.sortOrder - b.sortOrder || a.title.localeCompare(b.title));

const formatDurationMs = (value: unknown): string | null => {
  const ms = toFiniteNumber(value);
  if (ms == null || ms < 0) return null;
  const totalSeconds = Math.floor(ms / 1000);
  const minutes = Math.floor(totalSeconds / 60);
  const seconds = totalSeconds % 60;
  return `${minutes}:${String(seconds).padStart(2, '0')}`;
};

const withSpotifyLink = (links: ReleaseLink[], url: string | null): ReleaseLink[] => {
  if (!url) return links;
  if (links.some((link) => link.url === url || link.platform.toLowerCase() === 'spotify')) return links;
  return [{ platform: 'Spotify', url, accent: '#1db954' }, ...links];
};

const mapCmsReleasePayload = (
  payload: unknown,
  fallbackTitle: string | null,
  fallbackSortOrder: number,
  fallbackCover: string,
): ReleaseItem | null => {
  const data = toObject(payload);
  if (!data) return null;
  const linksRaw = Array.isArray(data['links']) ? data['links'] : [];
  const mappedLinks: ReleaseLink[] = linksRaw.flatMap((item) => {
    const link = toObject(item);
    if (!link) return [];
    const url = toNonEmptyText(link['url']);
    if (!url) return [];
    return [{
      platform: toNonEmptyText(link['platform']) ?? 'Enlace',
      url,
      accent: toNonEmptyText(link['accent']) ?? '#a5b4fc',
    }];
  });
  const title = toNonEmptyText(data['title']) ?? toNonEmptyText(data['name']) ?? fallbackTitle;
  if (!title) return null;
  const spotifyUrl =
    toNonEmptyText(data['spotifyUrl']) ??
    toNonEmptyText(data['url']) ??
    toNonEmptyText(data['trackUrl']);
  const links = withSpotifyLink(mappedLinks, spotifyUrl);
  return {
    title,
    artist: toNonEmptyText(data['artist']) ?? toNonEmptyText(data['subtitle']) ?? 'TDF Records',
    releasedOn:
      toText(data['releasedOn']) ??
      toText(data['date']) ??
      toText(data['releaseDate']) ??
      formatDurationMs(data['durationMs']) ??
      toText(data['duration']) ??
      '',
    duration: toText(data['duration']) ?? formatDurationMs(data['durationMs']) ?? undefined,
    blurb:
      toText(data['description']) ??
      toText(data['blurb']) ??
      toText(data['album']) ??
      '',
    cover:
      toNonEmptyText(data['cover']) ??
      toNonEmptyText(data['image']) ??
      toNonEmptyText(data['coverUrl']) ??
      toNonEmptyText(data['artworkUrl']) ??
      fallbackCover,
    links,
    primaryUrl: spotifyUrl ?? links[0]?.url,
    sortOrder: toFiniteNumber(data['sortOrder']) ?? toFiniteNumber(data['order']) ?? fallbackSortOrder,
  };
};

const sortReleases = (items: ReleaseItem[]): ReleaseItem[] =>
  [...items].sort((a, b) => a.sortOrder - b.sortOrder || a.title.localeCompare(b.title));

const RecordingsGrid = ({ items }: { items: RecordingItem[] }) => (
  <Grid container spacing={3}>
    {items.map((item) => (
      <Grid item key={`${item.sortOrder}-${item.title}`} xs={12} md={4}>
        <Card
          sx={{
            height: '100%',
            bgcolor: 'rgba(255,255,255,0.03)',
            border: '1px solid rgba(255,255,255,0.08)',
            borderRadius: 3,
            overflow: 'hidden',
            display: 'flex',
            flexDirection: 'column',
          }}
        >
          <CardMedia
            component={item.url ? MuiLink : 'div'}
            href={item.url}
            target={item.url ? '_blank' : undefined}
            rel={item.url ? 'noopener noreferrer' : undefined}
            underline="none"
            sx={{
              pt: '60%',
              backgroundImage: `linear-gradient(180deg, rgba(0,0,0,0.1), rgba(0,0,0,0.35)), url(${item.image})`,
              backgroundSize: 'cover',
              backgroundPosition: 'center',
              display: 'block',
              position: 'relative',
              '&::after': item.url
                ? {
                    content: '""',
                    position: 'absolute',
                    inset: 0,
                    background: 'radial-gradient(circle at center, rgba(15,23,42,0.05), rgba(15,23,42,0.35))',
                  }
                : undefined,
            }}
          />
          <CardContent sx={{ flexGrow: 1, display: 'flex', flexDirection: 'column', gap: 1.5 }}>
            <Stack direction="row" spacing={1} alignItems="center">
              <Chip label={item.vibe} size="small" sx={{ bgcolor: 'rgba(255,255,255,0.08)' }} />
              {(item.duration || item.recordedAt) && (
                <Typography variant="caption" sx={{ color: 'text.secondary' }}>
                  {item.duration ?? item.recordedAt}
                </Typography>
              )}
            </Stack>
            <Typography variant="h6" sx={{ fontWeight: 800, letterSpacing: '-0.01em' }}>
              {item.url ? (
                <MuiLink
                  href={item.url}
                  target="_blank"
                  rel="noopener noreferrer"
                  underline="hover"
                  color="inherit"
                >
                  {item.title}
                </MuiLink>
              ) : (
                item.title
              )}
            </Typography>
            <Typography variant="subtitle2" sx={{ color: 'text.secondary', fontWeight: 700 }}>
              {item.artist}
            </Typography>
            <Typography variant="body2" sx={{ color: 'text.secondary' }}>
              {item.description}
            </Typography>
            {item.url && (
              <Box sx={{ mt: 'auto' }}>
                <Button
                  component={MuiLink}
                  href={item.url}
                  target="_blank"
                  rel="noopener noreferrer"
                  variant="outlined"
                  size="small"
                  startIcon={<PlayCircleOutlineIcon />}
                  sx={{ textTransform: 'none' }}
                >
                  Ver video
                </Button>
              </Box>
            )}
          </CardContent>
        </Card>
      </Grid>
    ))}
  </Grid>
);

const ReleasesGrid = ({ items }: { items: ReleaseItem[] }) => (
  <Grid container spacing={3}>
    {items.map((release) => {
      const releaseHref = release.primaryUrl ?? release.links?.[0]?.url;
      const releaseMeta = release.duration ?? release.releasedOn;
      return (
        <Grid item xs={12} md={6} key={`${release.sortOrder}-${release.title}`}>
          <Card
            sx={{
              display: 'flex',
              flexDirection: { xs: 'column', sm: 'row' },
              gap: 2,
              bgcolor: 'rgba(255,255,255,0.03)',
              border: '1px solid rgba(255,255,255,0.08)',
              borderRadius: 3,
              overflow: 'hidden',
            }}
          >
            <Box
              sx={{
                width: { xs: '100%', sm: 200 },
                minHeight: 200,
                background: release.cover
                  ? `linear-gradient(180deg, rgba(0,0,0,0.1), rgba(0,0,0,0.35)), url(${release.cover})`
                  : 'linear-gradient(135deg, rgba(29,185,84,0.28), rgba(59,130,246,0.2))',
                backgroundSize: 'cover',
                backgroundPosition: 'center',
                borderRight: { sm: '1px solid rgba(255,255,255,0.06)' },
              }}
            />
            <CardContent sx={{ flex: 1, display: 'flex', flexDirection: 'column', gap: 1.5 }}>
              <Stack direction="row" spacing={1} alignItems="center">
                <Chip label="Canción" size="small" sx={{ bgcolor: 'rgba(255,255,255,0.08)' }} />
                {releaseMeta && (
                  <Typography variant="caption" sx={{ color: 'text.secondary' }}>
                    {releaseMeta}
                  </Typography>
                )}
              </Stack>
              <Typography variant="h6" sx={{ fontWeight: 800 }}>
                {releaseHref ? (
                  <MuiLink
                    href={releaseHref}
                    target="_blank"
                    rel="noopener noreferrer"
                    underline="hover"
                    color="inherit"
                  >
                    {release.title}
                  </MuiLink>
                ) : (
                  release.title
                )}
              </Typography>
              <Typography variant="subtitle2" sx={{ color: 'text.secondary', fontWeight: 700 }}>
                {release.artist}
              </Typography>
              {release.blurb && (
                <Typography variant="body2" sx={{ color: 'text.secondary' }}>
                  {release.blurb}
                </Typography>
              )}
              <Stack direction="row" spacing={1} flexWrap="wrap">
                {release.links.map((link) => (
                  <Button
                    key={link.platform}
                    component={MuiLink}
                    href={link.url}
                    target="_blank"
                    rel="noopener noreferrer"
                    variant="contained"
                    size="small"
                    sx={{
                      textTransform: 'none',
                      bgcolor: link.accent,
                      color: '#0b0d12',
                      fontWeight: 700,
                      '&:hover': { opacity: 0.9, bgcolor: link.accent },
                    }}
                  >
                    {link.platform}
                  </Button>
                ))}
              </Stack>
            </CardContent>
          </Card>
        </Grid>
      );
    })}
  </Grid>
);

const SessionsGrid = ({ items }: { items: SessionItem[] }) => (
  <Grid container spacing={3}>
    {items.map((video) => {
      const sessionHref = video.url ?? `https://www.youtube.com/watch?v=${video.youtubeId}`;
      return (
        <Grid item xs={12} md={4} key={video.youtubeId}>
          <Card
            sx={{
              height: '100%',
              bgcolor: 'rgba(255,255,255,0.03)',
              border: '1px solid rgba(255,255,255,0.08)',
              borderRadius: 3,
              overflow: 'hidden',
              display: 'flex',
              flexDirection: 'column',
            }}
          >
            <Box sx={{ position: 'relative', pt: '56.25%', backgroundColor: '#0f1117' }}>
              <Box
                component="iframe"
                src={video.embedUrl}
                title={video.title}
                allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                allowFullScreen
                sx={{
                  position: 'absolute',
                  inset: 0,
                  width: '100%',
                  height: '100%',
                  border: 0,
                }}
              />
            </Box>
            <CardContent sx={{ flexGrow: 1, display: 'flex', flexDirection: 'column', gap: 1 }}>
              <Stack direction="row" spacing={1} alignItems="center">
                <Chip label="Sesión en vivo" size="small" sx={{ bgcolor: 'rgba(255,255,255,0.08)' }} />
                <Typography variant="caption" sx={{ color: 'text.secondary' }}>
                  {video.duration}
                </Typography>
              </Stack>
              <Typography variant="h6" sx={{ fontWeight: 800 }}>
                <MuiLink
                  href={sessionHref}
                  target="_blank"
                  rel="noopener noreferrer"
                  underline="hover"
                  color="inherit"
                >
                  {video.title}
                </MuiLink>
              </Typography>
              <Typography variant="subtitle2" sx={{ color: 'text.secondary', fontWeight: 700 }}>
                {video.guests}
              </Typography>
              <Typography variant="body2" sx={{ color: 'text.secondary' }}>
                {video.description}
              </Typography>
            </CardContent>
          </Card>
        </Grid>
      );
    })}
  </Grid>
);

export default function RecordsPublicPage() {
  const sessionsCollectionQuery = useCmsContent('records-sessions', 'es');
  const sessionsQuery = useCmsContents('records-session-', 'es');
  const releasesCollectionQuery = useCmsContent('records-releases', 'es');
  const legacyReleasesQuery = useCmsContents('records-release-', 'es');
  const recordingsCollectionQuery = useCmsContent('records-recordings', 'es');
  const legacyRecordingsQuery = useCmsContents('records-recording-', 'es');
  const [dialogOpen, setDialogOpen] = useState(false);
  const envVars = import.meta.env as Record<string, string | undefined>;
  const bookingToken = envVars['VITE_PUBLIC_BOOKING_TOKEN'] ?? envVars['VITE_API_DEMO_TOKEN'] ?? '';
  const hasBookingToken = Boolean(bookingToken);
  const { session } = useSession();
  const canMaintainCms = useMemo(
    () => canAccessPath('/configuracion/cms', session?.roles, session?.modules),
    [session?.modules, session?.roles],
  );

  useEffect(() => {
    if (session) {
      setTransientApiToken(null);
      return undefined;
    }
    if (!bookingToken) {
      setTransientApiToken(null);
      return undefined;
    }
    setTransientApiToken(String(bookingToken));
    return () => {
      setTransientApiToken(null);
    };
  }, [bookingToken, session]);

  const collectionSessions: SessionItem[] = useMemo(() => {
    const payload = sessionsCollectionQuery.data?.ccdPayload;
    const payloadObject = toObject(payload);
    const rawVideos = Array.isArray(payload)
      ? payload
      : Array.isArray(payloadObject?.['videos'])
        ? payloadObject['videos']
        : [];
    return sortSessions(
      rawVideos
        .map((item, index) => mapCmsSessionPayload(item, null, index + 1))
        .filter((item): item is SessionItem => item != null),
    );
  }, [sessionsCollectionQuery.data]);

  const sessions: SessionItem[] = useMemo(() => {
    if (collectionSessions.length > 0) return collectionSessions;
    const mapped =
      sessionsQuery.data
        ?.map((entry): SessionItem | null => {
          const slugOrder = toFiniteNumber(entry.ccdSlug.split('-').at(-1));
          return mapCmsSessionPayload(entry.ccdPayload, toNonEmptyText(entry.ccdTitle), slugOrder ?? 999);
        })
        .filter((item): item is SessionItem => item != null) ?? [];
    return sortSessions(mapped);
  }, [collectionSessions, sessionsQuery.data]);
  const sessionsLoading = sessionsCollectionQuery.isLoading || (collectionSessions.length === 0 && sessionsQuery.isLoading);
  const sessionsError = sessionsCollectionQuery.isError && sessionsQuery.isError;

  const collectionReleases: ReleaseItem[] = useMemo(() => {
    const payload = releasesCollectionQuery.data?.ccdPayload;
    const payloadObject = toObject(payload);
    const collectionCover =
      toNonEmptyText(payloadObject?.['cover']) ??
      toNonEmptyText(payloadObject?.['playlistCover']) ??
      toNonEmptyText(payloadObject?.['image']) ??
      '';
    const rawItems = Array.isArray(payload)
      ? payload
      : Array.isArray(payloadObject?.['tracks'])
        ? payloadObject['tracks']
        : Array.isArray(payloadObject?.['songs'])
          ? payloadObject['songs']
          : Array.isArray(payloadObject?.['releases'])
            ? payloadObject['releases']
            : [];
    return sortReleases(
      rawItems
        .map((item, index) => mapCmsReleasePayload(item, null, index + 1, collectionCover))
        .filter((item): item is ReleaseItem => item != null),
    );
  }, [releasesCollectionQuery.data]);

  const legacyReleases: ReleaseItem[] = useMemo(() => {
    const mapped =
      legacyReleasesQuery.data
        ?.map((entry): ReleaseItem | null => {
          const slugOrder = toFiniteNumber(entry.ccdSlug.split('-').at(-1));
          return mapCmsReleasePayload(entry.ccdPayload, toNonEmptyText(entry.ccdTitle), slugOrder ?? 999, '');
        })
        .filter((item): item is ReleaseItem => item != null) ?? [];
    return sortReleases(mapped);
  }, [legacyReleasesQuery.data]);

  const releases: ReleaseItem[] = useMemo(
    () => (collectionReleases.length > 0 ? collectionReleases : legacyReleases),
    [collectionReleases, legacyReleases],
  );
  const releasesLoading =
    releasesCollectionQuery.isLoading || (collectionReleases.length === 0 && legacyReleasesQuery.isLoading);
  const releasesError = releasesCollectionQuery.isError && legacyReleasesQuery.isError;

  const sessionVideoIds = useMemo(
    () => new Set(sessions.map((sessionItem) => sessionItem.youtubeId).filter(Boolean)),
    [sessions],
  );

  const collectionRecordings: RecordingItem[] = useMemo(() => {
    const payload = recordingsCollectionQuery.data?.ccdPayload;
    const payloadObject = toObject(payload);
    const rawItems = Array.isArray(payload)
      ? payload
      : Array.isArray(payloadObject?.['videos'])
        ? payloadObject['videos']
        : Array.isArray(payloadObject?.['recordings'])
          ? payloadObject['recordings']
          : Array.isArray(payloadObject?.['items'])
            ? payloadObject['items']
            : [];
    return sortRecordings(
      rawItems
        .map((item, index) => mapCmsRecordingPayload(item, null, index + 1))
        .filter((item): item is RecordingItem => item != null),
    );
  }, [recordingsCollectionQuery.data]);

  const legacyRecordings: RecordingItem[] = useMemo(() => {
    const mapped =
      legacyRecordingsQuery.data
        ?.map((entry): RecordingItem | null => {
          const slugOrder = toFiniteNumber(entry.ccdSlug.split('-').at(-1));
          return mapCmsRecordingPayload(entry.ccdPayload, toNonEmptyText(entry.ccdTitle), slugOrder ?? 999);
        })
        .filter((item): item is RecordingItem => item != null) ?? [];
    return sortRecordings(mapped);
  }, [legacyRecordingsQuery.data]);

  const recordings: RecordingItem[] = useMemo(() => {
    const source = collectionRecordings.length > 0 ? collectionRecordings : legacyRecordings;
    return source.filter((item) => !item.youtubeId || !sessionVideoIds.has(item.youtubeId));
  }, [collectionRecordings, legacyRecordings, sessionVideoIds]);
  const recordingsLoading =
    recordingsCollectionQuery.isLoading || (collectionRecordings.length === 0 && legacyRecordingsQuery.isLoading);
  const recordingsError = recordingsCollectionQuery.isError && legacyRecordingsQuery.isError;

  const heroTitle = 'Historias desde el estudio, lanzamientos y sesiones en vivo de TDF en un solo lugar.';
  const heroEyebrow = canMaintainCms ? 'TDF Records — CMS público' : 'TDF Records · Estudio y lanzamientos';
  const heroSubtitle =
    'Descubre lo que suena y se graba en TDF: sesiones recientes, lanzamientos oficiales y videos en vivo desde un solo lugar.';
  const heroSupportText = hasBookingToken
    ? 'Cuéntanos qué quieres grabar y te mostraremos disponibilidad, salas e ingenieros desde aquí.'
    : 'La reserva directa no está disponible ahora mismo. Escríbenos por WhatsApp y coordinamos tu sesión manualmente.';
  const heroCta = hasBookingToken ? 'Reservar sesión' : 'Coordinar por WhatsApp';
  const heroSecondaryCta = 'Ver lanzamientos';
  const recordingsIntro =
    'Videos recientes del canal TDF Records que muestran el ambiente, el sonido y la energía del estudio.';
  const releasesIntro =
    'Escucha canciones del playlist RELEASES by TDF y salta directo a Spotify.';
  const sessionsIntro =
    'Sesiones en vivo, tomas especiales y performances para descubrir el catálogo en movimiento.';

  return (
    <Box
      sx={{
        minHeight: '100vh',
        bgcolor: '#07090f',
        color: '#e5e7eb',
        background:
          'radial-gradient(circle at 20% 20%, rgba(120,119,198,0.18), transparent 25%), radial-gradient(circle at 80% 0%, rgba(45,212,191,0.16), transparent 25%)',
      }}
    >
      <Box
        sx={{
          position: 'relative',
          overflow: 'hidden',
          borderBottom: '1px solid rgba(255,255,255,0.06)',
          background:
            'linear-gradient(135deg, rgba(12,18,28,0.9), rgba(12,18,28,0.6)), url(https://images.unsplash.com/photo-1483412033650-1015ddeb83d1?auto=format&fit=crop&w=1800&q=80) center/cover',
        }}
      >
        <BookingRequestDialog open={dialogOpen} onClose={() => setDialogOpen(false)} hasToken={hasBookingToken} />
        <Container maxWidth="lg" sx={{ py: { xs: 8, md: 12 } }}>
          <Box sx={{ display: 'flex', justifyContent: 'center', mb: 3 }}>
            <PublicBrandBar tagline="TDF Records · Estudio · Lanzamientos · Sesiones" />
          </Box>
          <Stack spacing={3} maxWidth="md">
            <Chip
              label={heroEyebrow}
              sx={{
                width: 'fit-content',
                bgcolor: 'rgba(255,255,255,0.08)',
                color: '#e5e7eb',
                fontWeight: 700,
              }}
            />
            {canMaintainCms && (
              <Stack direction="row" spacing={1}>
                <Tooltip title="Abrir CMS (records-*)">
                  <Button
                    component={RouterLink}
                    to="/configuracion/cms"
                    variant="outlined"
                    size="small"
                    startIcon={<EditIcon />}
                    sx={{ borderColor: 'rgba(255,255,255,0.2)', color: '#e5e7eb' }}
                  >
                    Gestionar CMS
                  </Button>
                </Tooltip>
                <Tooltip title="Ir a CRUD de lanzamientos">
                  <Button
                    component={RouterLink}
                    to="/label/releases"
                    variant="outlined"
                    size="small"
                    sx={{ borderColor: 'rgba(255,255,255,0.2)', color: '#e5e7eb' }}
                  >
                    Gestionar lanzamientos
                  </Button>
                </Tooltip>
              </Stack>
            )}
            <Typography variant="h2" sx={{ fontWeight: 900, letterSpacing: '-0.03em', lineHeight: 1.05 }}>
              {heroTitle}
            </Typography>
            <Typography variant="h6" sx={{ color: 'rgba(226,232,240,0.82)', maxWidth: 640 }}>
              {heroSubtitle}
            </Typography>
            <Stack spacing={1.5}>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
                {hasBookingToken ? (
                  <Button
                    variant="contained"
                    size="large"
                    sx={{
                      bgcolor: '#7c3aed',
                      color: '#f8fafc',
                      fontWeight: 800,
                      px: 3,
                      textTransform: 'none',
                      '&:hover': { bgcolor: '#6d28d9' },
                    }}
                    onClick={() => setDialogOpen(true)}
                  >
                    {heroCta}
                  </Button>
                ) : (
                  <Button
                    variant="contained"
                    size="large"
                    component="a"
                    href={STUDIO_WHATSAPP_URL}
                    target="_blank"
                    rel="noreferrer"
                    sx={{
                      bgcolor: '#22c55e',
                      color: '#08110d',
                      fontWeight: 800,
                      px: 3,
                      textTransform: 'none',
                      '&:hover': { bgcolor: '#16a34a' },
                    }}
                  >
                    {heroCta}
                  </Button>
                )}
                <Button
                  variant="outlined"
                  size="large"
                  href="#releases"
                  sx={{
                    borderColor: 'rgba(255,255,255,0.3)',
                    color: '#e5e7eb',
                    textTransform: 'none',
                  }}
                >
                  {heroSecondaryCta}
                </Button>
                <Button
                  component={RouterLink}
                  to="/fans"
                  variant="text"
                  size="large"
                  sx={{ color: '#e5e7eb', textTransform: 'none' }}
                >
                  Comunidad
                </Button>
              </Stack>
              <Typography variant="body2" color="rgba(226,232,240,0.8)">
                {heroSupportText}
              </Typography>
            </Stack>
          </Stack>
          <Stack
            direction={{ xs: 'column', md: 'row' }}
            spacing={3}
            sx={{ mt: 6, flexWrap: 'wrap' }}
          >
            <GradientCard
              title="Videos recientes"
              actions={
                canMaintainCms ? (
                  <Tooltip title="Editar videos recientes (CMS records-recordings)">
                    <IconButton
                      component={RouterLink}
                      to="/configuracion/cms?slug=records-recordings"
                      aria-label="Editar videos recientes en CMS"
                      size="small"
                      sx={{ color: 'rgba(229,231,235,0.9)' }}
                    >
                      <EditIcon fontSize="small" />
                    </IconButton>
                  </Tooltip>
                ) : null
              }
            >
              <Stack spacing={0.75} sx={{ mb: 1 }}>
                <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.82)' }}>
                  Explora sesiones, DJ sets y momentos del canal TDF Records sin repetir videos ya destacados.
                </Typography>
                {canMaintainCms && (
                  <Typography variant="caption" sx={{ color: 'rgba(148,163,184,0.92)' }}>
                    Edita este bloque desde Configuración → CMS con el slug `records-recordings`.
                  </Typography>
                )}
              </Stack>
              <Stack direction="row" spacing={1} alignItems="center">
                {recordings.slice(0, 3).map((item) => (
                  <Avatar
                    key={item.title}
                    alt={item.title}
                    src={item.image}
                    sx={{ width: 40, height: 40, border: '2px solid rgba(255,255,255,0.2)' }}
                  />
                ))}
              </Stack>
            </GradientCard>
            <GradientCard
              title="Sesiones en vivo TDF"
              actions={
                canMaintainCms ? (
                  <Tooltip title="Editar sesiones (CMS records-sessions)">
                    <IconButton
                      component={RouterLink}
                      to="/configuracion/cms?slug=records-sessions"
                      aria-label="Editar sesiones en vivo TDF en CMS"
                      size="small"
                      sx={{ color: 'rgba(229,231,235,0.9)' }}
                    >
                      <EditIcon fontSize="small" />
                    </IconButton>
                  </Tooltip>
                ) : null
              }
            >
              <Stack spacing={1}>
                {sessions.slice(0, 2).map((video) => {
                  const sessionHref = video.url;
                  return (
                    <Stack key={video.youtubeId} direction="row" spacing={1} alignItems="center">
                      <Chip label="Sesión" size="small" sx={{ bgcolor: 'rgba(255,255,255,0.08)' }} />
                      <Typography variant="body2" sx={{ fontWeight: 700 }}>
                        <MuiLink
                          href={sessionHref}
                          target="_blank"
                          rel="noopener noreferrer"
                          underline="hover"
                          color="inherit"
                        >
                          {video.title}
                        </MuiLink>
                      </Typography>
                    </Stack>
                  );
                })}
              </Stack>
              {canMaintainCms && (
                <Typography variant="caption" sx={{ color: 'rgba(148,163,184,0.92)', mt: 1.5 }}>
                  Edita este bloque desde Configuración → CMS con el slug `records-sessions`.
                </Typography>
              )}
            </GradientCard>
            <GradientCard
              title="Lanzamientos"
              actions={
                canMaintainCms ? (
                  <Stack direction="row" spacing={0.5}>
                    <Tooltip title="Editar canciones (CMS records-releases)">
                      <IconButton
                        component={RouterLink}
                        to="/configuracion/cms?slug=records-releases"
                        aria-label="Editar canciones en CMS"
                        size="small"
                        sx={{ color: 'rgba(229,231,235,0.9)' }}
                      >
                        <EditIcon fontSize="small" />
                      </IconButton>
                    </Tooltip>
                    <Tooltip title="Ir a CRUD de lanzamientos">
                      <IconButton
                        component={RouterLink}
                        to="/label/releases"
                        aria-label="Ir al CRUD de lanzamientos"
                        size="small"
                        sx={{ color: 'rgba(229,231,235,0.9)' }}
                      >
                        <EditIcon fontSize="small" />
                      </IconButton>
                    </Tooltip>
                  </Stack>
                ) : null
              }
            >
              <Stack spacing={1}>
                {releases.slice(0, 4).map((release) => {
                  const releaseHref = release.primaryUrl ?? release.links?.[0]?.url;
                  return (
                    <Typography key={`${release.sortOrder}-${release.title}`} variant="body2" sx={{ fontWeight: 700 }}>
                      {releaseHref ? (
                        <MuiLink
                          href={releaseHref}
                          target="_blank"
                          rel="noopener noreferrer"
                          underline="hover"
                          color="inherit"
                        >
                          {release.title}
                        </MuiLink>
                      ) : (
                        release.title
                      )}{' '}
                      · {release.artist}
                    </Typography>
                  );
                })}
                {releases.length === 0 && (
                  <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.82)' }}>
                    Publica canciones en CMS para activar este resumen.
                  </Typography>
                )}
              </Stack>
              {canMaintainCms && (
                <Typography variant="caption" sx={{ color: 'rgba(148,163,184,0.92)', mt: 1.5 }}>
                  Edita este bloque desde Configuración → CMS con el slug `records-releases`.
                </Typography>
              )}
            </GradientCard>
          </Stack>
        </Container>
      </Box>

      <Container maxWidth="lg" sx={{ py: { xs: 6, md: 10 } }}>
        <Box sx={{ mb: 6 }}>
          <SectionTitle
            title="Grabaciones recientes"
            kicker="Estudio"
            actions={
              canMaintainCms && (
                <Button
                  component={RouterLink}
                  to="/configuracion/cms?slug=records-recordings"
                  size="small"
                  variant="outlined"
                  startIcon={<EditIcon />}
                >
                  Gestionar CMS
                </Button>
              )
            }
          />
          <Stack spacing={0.75} sx={{ maxWidth: 760, mb: 3 }}>
            <Typography variant="body1" sx={{ color: 'text.secondary' }}>
              {recordingsIntro}
            </Typography>
            {canMaintainCms && (
              <Typography variant="caption" sx={{ color: 'rgba(148,163,184,0.92)' }}>
                Mantén el arreglo `videos[]` desde el slug `records-recordings`; también se aceptan slugs legacy `records-recording-*`.
              </Typography>
            )}
          </Stack>
          {recordingsLoading ? (
            <Stack direction="row" spacing={1.5} alignItems="center" sx={{ color: 'text.secondary' }}>
              <CircularProgress size={20} color="inherit" />
              <Typography variant="body2">Cargando videos desde CMS…</Typography>
            </Stack>
          ) : recordingsError ? (
            <Alert severity="warning">No pudimos cargar los videos publicados desde el CMS.</Alert>
          ) : recordings.length > 0 ? (
            <RecordingsGrid items={recordings} />
          ) : (
            <Alert
              severity={canMaintainCms ? 'info' : 'warning'}
              action={
                canMaintainCms ? (
                  <Button
                    component={RouterLink}
                    to="/configuracion/cms?slug=records-recordings"
                    size="small"
                    color="inherit"
                  >
                    Abrir CMS
                  </Button>
                ) : undefined
              }
            >
              No hay videos recientes publicados en CMS todavía.
            </Alert>
          )}
        </Box>

        <Box id="releases" sx={{ mb: 6 }}>
          <SectionTitle
            title="Lanzamientos TDF Records"
            kicker="Sello"
            actions={
              canMaintainCms && (
                <Stack direction="row" spacing={1}>
                  <Button
                    component={RouterLink}
                    to="/configuracion/cms?slug=records-releases"
                    size="small"
                    variant="outlined"
                    startIcon={<EditIcon />}
                  >
                    CMS
                  </Button>
                  <Button
                    component={RouterLink}
                    to="/label/releases"
                    size="small"
                    variant="outlined"
                  >
                    Gestionar lanzamientos
                  </Button>
                </Stack>
              )
            }
          />
          <Stack spacing={0.75} sx={{ maxWidth: 760, mb: 3 }}>
            <Typography variant="body1" sx={{ color: 'text.secondary' }}>
              {releasesIntro}
            </Typography>
            {canMaintainCms && (
              <Typography variant="caption" sx={{ color: 'rgba(148,163,184,0.92)' }}>
                Mantén el arreglo `tracks[]` desde el slug `records-releases`; también se aceptan slugs legacy `records-release-*`.
              </Typography>
            )}
          </Stack>
          {releasesLoading ? (
            <Stack direction="row" spacing={1.5} alignItems="center" sx={{ color: 'text.secondary' }}>
              <CircularProgress size={20} color="inherit" />
              <Typography variant="body2">Cargando canciones desde CMS…</Typography>
            </Stack>
          ) : releasesError ? (
            <Alert severity="warning">No pudimos cargar las canciones publicadas desde el CMS.</Alert>
          ) : releases.length > 0 ? (
            <ReleasesGrid items={releases} />
          ) : (
            <Alert
              severity={canMaintainCms ? 'info' : 'warning'}
              action={
                canMaintainCms ? (
                  <Button
                    component={RouterLink}
                    to="/configuracion/cms?slug=records-releases"
                    size="small"
                    color="inherit"
                  >
                    Abrir CMS
                  </Button>
                ) : undefined
              }
            >
              No hay canciones publicadas en CMS todavía.
            </Alert>
          )}
        </Box>

        <Box sx={{ mb: 6 }}>
          <SectionTitle
            title="Sesiones en vivo TDF"
            kicker="YouTube"
            actions={
              canMaintainCms && (
                <Button
                  component={RouterLink}
                  to="/configuracion/cms?slug=records-sessions"
                  size="small"
                  variant="outlined"
                  startIcon={<EditIcon />}
                >
                  Gestionar CMS
                </Button>
              )
            }
          />
          <Stack spacing={0.75} sx={{ maxWidth: 760, mb: 3 }}>
            <Typography variant="body1" sx={{ color: 'text.secondary' }}>
              {sessionsIntro}
            </Typography>
            {canMaintainCms && (
              <Typography variant="caption" sx={{ color: 'rgba(148,163,184,0.92)' }}>
                Mantén el arreglo `videos[]` desde el slug `records-sessions`; también se aceptan slugs legacy `records-session-*`.
              </Typography>
            )}
          </Stack>
          {sessionsLoading ? (
            <Stack direction="row" spacing={1.5} alignItems="center" sx={{ color: 'text.secondary' }}>
              <CircularProgress size={20} color="inherit" />
              <Typography variant="body2">Cargando sesiones desde CMS…</Typography>
            </Stack>
          ) : sessionsError ? (
            <Alert severity="warning">No pudimos cargar las sesiones publicadas desde el CMS.</Alert>
          ) : sessions.length > 0 ? (
            <SessionsGrid items={sessions} />
          ) : (
            <Alert
              severity={canMaintainCms ? 'info' : 'warning'}
              action={
                canMaintainCms ? (
                  <Button
                    component={RouterLink}
                    to="/configuracion/cms?slug=records-sessions"
                    size="small"
                    color="inherit"
                  >
                    Abrir CMS
                  </Button>
                ) : undefined
              }
            >
              No hay sesiones publicadas en CMS todavía.
            </Alert>
          )}
        </Box>

        {canMaintainCms && (
          <GradientCard
            title="Atajos de CMS"
            actions={
              <Button
                component={RouterLink}
                to="/configuracion/cms"
                size="small"
                variant="outlined"
                startIcon={<EditIcon />}
              >
                Abrir CMS
              </Button>
            }
          >
            <Typography variant="body2" sx={{ color: 'text.secondary', mb: 1 }}>
              Usa el panel en Configuración → CMS con los slugs `records-releases`, `records-sessions` y `records-recordings` para crear borradores, publicar y versionar contenido en es/en.
            </Typography>
          </GradientCard>
        )}
      </Container>
    </Box>
  );
}
