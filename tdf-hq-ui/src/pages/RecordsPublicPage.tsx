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
import { recordings as defaultRecordings, releases as defaultReleases, sessionVideos as defaultSessions } from '../constants/recordsContent';
import PublicBrandBar from '../components/PublicBrandBar';
import { useCmsContents } from '../hooks/useCmsContent';
import { Bookings } from '../api/bookings';
import { Rooms } from '../api/rooms';
import { Parties } from '../api/parties';
import { Admin } from '../api/admin';
import { Services } from '../api/services';
import { Engineers } from '../api/engineers';
import { setTransientApiToken, useSession } from '../session/SessionContext';
import { canAccessPath } from '../utils/accessControl';
import { STUDIO_WHATSAPP_URL } from '../config/appConfig';
import EditIcon from '@mui/icons-material/Edit';

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

type RecordingItem = typeof defaultRecordings[number];
type ReleaseItem = (typeof defaultReleases)[number] & { primaryUrl?: string };
type SessionItem = (typeof defaultSessions)[number] & { url?: string };

const defaultReleaseItems: ReleaseItem[] = defaultReleases.map((release) => ({
  ...release,
  primaryUrl: release.links?.[0]?.url,
}));

const defaultSessionItems: SessionItem[] = defaultSessions.map((session) => ({
  ...session,
  url: `https://www.youtube.com/watch?v=${session.youtubeId}`,
}));

const RecordingsGrid = ({ items }: { items: RecordingItem[] }) => (
  <Grid container spacing={3}>
    {items.map((item) => (
      <Grid item key={item.title} xs={12} md={4}>
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
            component="div"
            sx={{
              pt: '60%',
              backgroundImage: `linear-gradient(180deg, rgba(0,0,0,0.1), rgba(0,0,0,0.35)), url(${item.image})`,
              backgroundSize: 'cover',
              backgroundPosition: 'center',
            }}
          />
          <CardContent sx={{ flexGrow: 1, display: 'flex', flexDirection: 'column', gap: 1.5 }}>
            <Stack direction="row" spacing={1} alignItems="center">
              <Chip label={item.vibe} size="small" sx={{ bgcolor: 'rgba(255,255,255,0.08)' }} />
              <Typography variant="caption" sx={{ color: 'text.secondary' }}>
                {item.recordedAt}
              </Typography>
            </Stack>
            <Typography variant="h6" sx={{ fontWeight: 800, letterSpacing: '-0.01em' }}>
              {item.title}
            </Typography>
            <Typography variant="subtitle2" sx={{ color: 'text.secondary', fontWeight: 700 }}>
              {item.artist}
            </Typography>
            <Typography variant="body2" sx={{ color: 'text.secondary' }}>
              {item.description}
            </Typography>
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
      return (
        <Grid item xs={12} md={6} key={release.title}>
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
                backgroundImage: `linear-gradient(180deg, rgba(0,0,0,0.1), rgba(0,0,0,0.35)), url(${release.cover})`,
                backgroundSize: 'cover',
                backgroundPosition: 'center',
                borderRight: { sm: '1px solid rgba(255,255,255,0.06)' },
              }}
            />
            <CardContent sx={{ flex: 1, display: 'flex', flexDirection: 'column', gap: 1.5 }}>
              <Stack direction="row" spacing={1} alignItems="center">
                <Chip label="Lanzamiento" size="small" sx={{ bgcolor: 'rgba(255,255,255,0.08)' }} />
                <Typography variant="caption" sx={{ color: 'text.secondary' }}>
                  {release.releasedOn}
                </Typography>
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
              <Typography variant="body2" sx={{ color: 'text.secondary' }}>
                {release.blurb}
              </Typography>
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
                src={`https://www.youtube.com/embed/${video.youtubeId}`}
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
  const sessionsQuery = useCmsContents('records-session-', 'es');
  const releasesQuery = useCmsContents('records-release-', 'es');
  const recordingsQuery = useCmsContents('records-recording-', 'es');
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
    if (session?.apiToken) {
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
  }, [bookingToken, session?.apiToken]);

  const sessions: SessionItem[] = useMemo(() => {
    const mapped =
      sessionsQuery.data
        ?.map((entry): SessionItem | null => {
          const payload = toObject(entry.ccdPayload);
          if (!payload) return null;
          const youtubeId =
            toNonEmptyText(payload['youtubeId']) ??
            toNonEmptyText(payload['youtubeID']) ??
            toNonEmptyText(payload['id']);
          if (!youtubeId) return null;
          const url = toNonEmptyText(payload['url']) ?? `https://www.youtube.com/watch?v=${youtubeId}`;
          return {
            youtubeId,
            title: toNonEmptyText(payload['title']) ?? toNonEmptyText(entry.ccdTitle) ?? 'Sesión en vivo TDF',
            duration: toText(payload['duration']) ?? '',
            guests: toText(payload['guests']) ?? '',
            description: toText(payload['description']) ?? '',
            url,
          };
        })
        .filter((item): item is SessionItem => item != null) ?? [];
    return mapped.length > 0 ? mapped : defaultSessionItems;
  }, [sessionsQuery.data]);

  const [firstDefaultRelease] = defaultReleaseItems;
  const defaultReleaseCover = firstDefaultRelease?.cover ?? '';

  const releases: ReleaseItem[] = useMemo(() => {
    const mapped =
      releasesQuery.data
        ?.map((entry): ReleaseItem | null => {
          const payload = toObject(entry.ccdPayload);
          if (!payload) return null;
          const linksRaw = Array.isArray(payload['links']) ? payload['links'] : [];
          const links: ReleaseItem['links'] = linksRaw.flatMap((item) => {
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
          const title = toNonEmptyText(payload['title']) ?? toNonEmptyText(entry.ccdTitle);
          if (!title) return null;
          return {
            title,
            artist: toNonEmptyText(payload['artist']) ?? 'TDF House Band',
            releasedOn: toText(payload['releasedOn']) ?? toText(payload['date']) ?? '',
            blurb: toText(payload['description']) ?? toText(payload['blurb']) ?? '',
            cover: toNonEmptyText(payload['cover']) ?? toNonEmptyText(payload['image']) ?? defaultReleaseCover,
            links,
            primaryUrl: toNonEmptyText(payload['url']) ?? links[0]?.url,
          };
        })
        .filter((item): item is ReleaseItem => item != null) ?? [];
    return mapped.length > 0 ? mapped : defaultReleaseItems;
  }, [releasesQuery.data, defaultReleaseCover]);

  const recordings: RecordingItem[] = useMemo(() => {
    const mapped =
      recordingsQuery.data
        ?.map((entry): RecordingItem | null => {
          const payload = toObject(entry.ccdPayload);
          if (!payload) return null;
          const title = toNonEmptyText(payload['title']);
          const image = toNonEmptyText(payload['image']);
          if (!title || !image) return null;
          return {
            title,
            artist: toText(payload['artist']) ?? '',
            recordedAt: toText(payload['recordedAt']) ?? toText(payload['date']) ?? '',
            vibe: toText(payload['vibe']) ?? toText(payload['tag']) ?? 'En vivo',
            description: toText(payload['description']) ?? '',
            image,
          };
        })
        .filter((item): item is RecordingItem => item != null) ?? [];
    return mapped.length > 0 ? mapped : defaultRecordings;
  }, [recordingsQuery.data]);

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
    'Una selección de grabaciones recientes para mostrar el ambiente, el sonido y la energía del estudio.';
  const releasesIntro =
    'Explora los lanzamientos más recientes de TDF Records y salta directo a la plataforma que prefieras.';
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
              title="Fotos del estudio"
              actions={
                canMaintainCms ? (
                  <Tooltip title="Editar fotos (CMS records-recording-*)">
                    <IconButton
                      component={RouterLink}
                      to="/configuracion/cms?slug=records-recording-"
                      aria-label="Editar fotos del estudio en CMS"
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
                  Explora salas, sesiones y momentos detrás de cámaras capturados durante grabaciones recientes.
                </Typography>
                {canMaintainCms && (
                  <Typography variant="caption" sx={{ color: 'rgba(148,163,184,0.92)' }}>
                    Edita este bloque desde Configuración → CMS con los slugs `records-recording-*`.
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
                  <Tooltip title="Editar sesiones (CMS records-session-*)">
                    <IconButton
                      component={RouterLink}
                      to="/configuracion/cms?slug=records-session-"
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
                  const sessionHref = video.url ?? `https://www.youtube.com/watch?v=${video.youtubeId}`;
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
                  Edita este bloque desde Configuración → CMS con los slugs `records-session-*`.
                </Typography>
              )}
            </GradientCard>
            <GradientCard
              title="Lanzamientos"
              actions={
                canMaintainCms ? (
                  <Stack direction="row" spacing={0.5}>
                    <Tooltip title="Editar lanzamientos (CMS records-release-*)">
                      <IconButton
                        component={RouterLink}
                        to="/configuracion/cms?slug=records-release-"
                        aria-label="Editar lanzamientos en CMS"
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
                {releases.map((release) => {
                  const releaseHref = release.primaryUrl ?? release.links?.[0]?.url;
                  return (
                    <Typography key={release.title} variant="body2" sx={{ fontWeight: 700 }}>
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
              </Stack>
              {canMaintainCms && (
                <Typography variant="caption" sx={{ color: 'rgba(148,163,184,0.92)', mt: 1.5 }}>
                  Edita este bloque desde Configuración → CMS con los slugs `records-release-*`.
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
                  to="/configuracion/cms?slug=records-recording-"
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
                Edita este grid desde Configuración → CMS con los slugs `records-recording-*`.
              </Typography>
            )}
          </Stack>
          <RecordingsGrid items={recordings} />
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
                    to="/configuracion/cms?slug=records-release-"
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
                Gestiona los enlaces y el contenido desde Configuración → CMS o desde el módulo de lanzamientos.
              </Typography>
            )}
          </Stack>
          <ReleasesGrid items={releases} />
        </Box>

        <Box sx={{ mb: 6 }}>
          <SectionTitle
            title="Sesiones en vivo TDF"
            kicker="YouTube"
            actions={
              canMaintainCms && (
                <Button
                  component={RouterLink}
                  to="/configuracion/cms?slug=records-session-"
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
                Inserta el ID de YouTube en los slugs `records-session-*` para embeber cada sesión aquí.
              </Typography>
            )}
          </Stack>
          <SessionsGrid items={sessions} />
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
              Usa el panel en Configuración → CMS con los slugs `records-release-*`, `records-session-*` y `records-recording-*` para crear borradores, publicar y versionar contenido en es/en.
            </Typography>
          </GradientCard>
        )}
      </Container>
    </Box>
  );
}
