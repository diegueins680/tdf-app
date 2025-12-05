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
  Stack,
  TextField,
  Typography,
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
import { useSession } from '../session/SessionContext';

const BOOKING_ZONE = 'America/Bogota';
const SERVICE_OPTIONS = [
  'Vocal recording',
  'Band recording',
  'Band rehearsal',
  'DJ booth rental',
  'Podcast / Voiceover',
  'Producción musical',
  'Mezcla / Post',
];

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
  const [serviceType, setServiceType] = useState<string>(SERVICE_OPTIONS[0] ?? 'Recording');
  const [startInput, setStartInput] = useState<string>(
    DateTime.now()
      .setZone(BOOKING_ZONE)
      .plus({ days: 1 })
      .set({ hour: 10, minute: 0, second: 0, millisecond: 0 })
      .toFormat("yyyy-LL-dd'T'HH:mm"),
  );
  const [duration, setDuration] = useState<number>(120);
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

  const engineerOptions = useMemo(() => {
    const engineers = new Set<string>();
    (bookingsQuery.data ?? []).forEach((booking) => {
      booking.resources.forEach((res) => {
        if (res.brRole.toLowerCase().includes('engineer')) {
          const name = res.brRoomName?.trim();
          if (name) engineers.add(name);
        }
      });
    });
    return Array.from(engineers).sort((a, b) => a.localeCompare(b));
  }, [bookingsQuery.data]);

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
      booking.resources.some(
        (res) =>
          res.brRole.toLowerCase().includes('engineer') &&
          res.brRoomName &&
          res.brRoomName.toLowerCase() === engineerLower,
      ),
    );
  }, [engineerName, relevantConflicts, isTimeValid]);

  const nextRoomConflict = roomConflicts[0] ?? null;
  const nextEngineerConflict = engineerConflicts[0] ?? null;

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
        `Solicitud web (Records CMS)`,
        `Servicio: ${serviceType}`,
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
        cbTitle: `${serviceType} - ${contactName}`,
        cbStartsAt: startIso ?? '',
        cbEndsAt: endIso ?? '',
        cbStatus: 'Tentative',
        cbNotes: augmentedNotes || null,
        cbServiceType: serviceType,
        cbPartyId: party.partyId,
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
      setFormError('No hay token de API configurado para procesar reservas.');
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

  return (
    <Dialog open={open} onClose={resetAndClose} maxWidth="md" fullWidth>
      <form onSubmit={handleSubmit}>
        <DialogTitle>Reservar sesión en el estudio</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={2}>
            {!hasToken && (
              <Alert severity="warning">
                Configura VITE_PUBLIC_BOOKING_TOKEN o VITE_API_DEMO_TOKEN para habilitar la reserva directa.
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
                helperText="Usa nombres predefinidos para reservar cabinas por defecto."
                SelectProps={{ native: true }}
              >
                {SERVICE_OPTIONS.map((opt) => (
                  <option key={opt} value={opt}>
                    {opt}
                  </option>
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
                onChange={(e) => setDuration(Number(e.target.value))}
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
                freeSolo={engineerOptions.length === 0}
                value={engineerName || null}
                onChange={(_, value) => setEngineerName(value ?? '')}
                onInputChange={(_, value, reason) => {
                  if (reason === 'clear') setEngineerName('');
                  if (reason === 'input' && engineerOptions.length === 0) setEngineerName(value);
                }}
                renderInput={(params) => (
                  <TextField
                    {...params}
                    label="Ingeniero (opcional)"
                    placeholder={engineerOptions.length ? 'Selecciona para validar choques' : 'Escribe el nombre del ingeniero'}
                    helperText={
                      engineerName
                        ? nextEngineerConflict
                          ? `Cruce: ${formatRange(nextEngineerConflict.startsAt, nextEngineerConflict.endsAt)}`
                          : 'Sin choques en este horario.'
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
            Cancelar
          </Button>
          <Button variant="contained" type="submit" disabled={mutation.isPending || !hasToken}>
            {mutation.isPending ? 'Reservando…' : 'Confirmar reserva'}
          </Button>
        </DialogActions>
      </form>
    </Dialog>
  );
}

const SectionTitle = ({ title, kicker }: { title: string; kicker?: string }) => (
  <Stack spacing={1} direction="row" alignItems="center" sx={{ mb: 2 }}>
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
);

const GradientCard = ({
  title,
  children,
}: {
  title: string;
  children: React.ReactNode;
}) => (
  <Box
    sx={{
      p: 3,
      borderRadius: 3,
      bgcolor: 'rgba(255,255,255,0.02)',
      border: '1px solid rgba(255,255,255,0.08)',
      boxShadow: '0 30px 60px rgba(0,0,0,0.35)',
    }}
  >
    <Typography variant="h6" sx={{ fontWeight: 700, mb: 2 }}>
      {title}
    </Typography>
    {children}
  </Box>
);

type RecordingItem = typeof defaultRecordings[number];
type ReleaseItem = typeof defaultReleases[number];
type SessionItem = typeof defaultSessions[number];

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
    {items.map((release) => (
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
              <Chip label="Release" size="small" sx={{ bgcolor: 'rgba(255,255,255,0.08)' }} />
              <Typography variant="caption" sx={{ color: 'text.secondary' }}>
                {release.releasedOn}
              </Typography>
            </Stack>
            <Typography variant="h6" sx={{ fontWeight: 800 }}>
              {release.title}
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
    ))}
  </Grid>
);

const SessionsGrid = ({ items }: { items: SessionItem[] }) => (
  <Grid container spacing={3}>
    {items.map((video) => (
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
              <Chip label="TDF Session" size="small" sx={{ bgcolor: 'rgba(255,255,255,0.08)' }} />
              <Typography variant="caption" sx={{ color: 'text.secondary' }}>
                {video.duration}
              </Typography>
            </Stack>
            <Typography variant="h6" sx={{ fontWeight: 800 }}>
              {video.title}
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
    ))}
  </Grid>
);

export default function RecordsPublicPage() {
  const sessionsQuery = useCmsContents('records-session-', 'es');
  const releasesQuery = useCmsContents('records-release-', 'es');
  const recordingsQuery = useCmsContents('records-recording-', 'es');
  const [dialogOpen, setDialogOpen] = useState(false);
  const bookingToken =
    import.meta.env.VITE_PUBLIC_BOOKING_TOKEN ?? import.meta.env.VITE_API_DEMO_TOKEN ?? '';
  const hasBookingToken = Boolean(bookingToken);
  const { session, login, setApiToken } = useSession();

  useEffect(() => {
    if (!bookingToken) return;
    if (!session) {
      login(
        {
          username: 'records-public',
          displayName: 'Records Public',
          roles: [],
          apiToken: bookingToken,
        },
        { remember: false },
      );
    } else if (!session.apiToken) {
      setApiToken(String(bookingToken));
    }
  }, [bookingToken, session, login, setApiToken]);

  const sessions: SessionItem[] = useMemo(() => {
    const mapped =
      sessionsQuery.data
        ?.map((entry) => {
          const payload = entry.ccdPayload;
          if (!payload || typeof payload !== 'object') return null;
          const p = payload as Partial<{
            youtubeId: string;
            youtubeID: string;
            id: string;
            title: string;
            duration: string;
            guests: string;
            description: string;
          }>;
          const youtubeId = p.youtubeId ?? p.youtubeID ?? p.id;
          if (!youtubeId) return null;
          return {
            youtubeId,
            title: p.title ?? entry.ccdTitle ?? 'TDF Session',
            duration: p.duration ?? '',
            guests: p.guests ?? '',
            description: p.description ?? '',
          };
        })
        .filter(Boolean) ?? [];
    return (mapped as SessionItem[]).length ? (mapped as SessionItem[]) : defaultSessions;
  }, [sessionsQuery.data]);

  const [firstDefaultRelease] = defaultReleases;
  const defaultReleaseCover = firstDefaultRelease?.cover ?? '';

  const releases: ReleaseItem[] = useMemo(() => {
    const mapped =
      releasesQuery.data
        ?.map((entry) => {
          const payload = entry.ccdPayload;
          if (!payload || typeof payload !== 'object') return null;
          const p = payload as Partial<{
            links: { platform?: string; url?: string; accent?: string }[];
            title: string;
            artist: string;
            releasedOn: string;
            date: string;
            description: string;
            blurb: string;
            cover: string;
            image: string;
          }>;
          const linksRaw = Array.isArray(p.links) ? p.links : [];
          const links =
            linksRaw
              .map((l) =>
                l?.url
                  ? {
                      platform: l.platform ?? 'Link',
                      url: l.url,
                      accent: l.accent ?? '#a5b4fc',
                    }
                  : null,
              )
              .filter(Boolean) ?? [];
          if (!p.title && !entry.ccdTitle) return null;
          return {
            title: p.title ?? entry.ccdTitle ?? 'Release',
            artist: p.artist ?? 'TDF House Band',
            releasedOn: p.releasedOn ?? p.date ?? '',
            blurb: p.description ?? p.blurb ?? '',
            cover: p.cover ?? p.image ?? defaultReleaseCover,
            links,
          };
        })
        .filter(Boolean) ?? [];
    return (mapped as ReleaseItem[]).length ? (mapped as ReleaseItem[]) : defaultReleases;
  }, [releasesQuery.data, defaultReleaseCover]);

  const recordings: RecordingItem[] = useMemo(() => {
    const mapped =
      recordingsQuery.data
        ?.map((entry) => {
          const payload = entry.ccdPayload;
          if (!payload || typeof payload !== 'object') return null;
          const p = payload as Partial<{
            title: string;
            image: string;
            artist: string;
            recordedAt: string;
            date: string;
            vibe: string;
            tag: string;
            description: string;
          }>;
          if (!p.title || !p.image) return null;
          return {
            title: p.title,
            artist: p.artist ?? '',
            recordedAt: p.recordedAt ?? p.date ?? '',
            vibe: p.vibe ?? p.tag ?? 'Live',
            description: p.description ?? '',
            image: p.image,
          };
        })
        .filter(Boolean) ?? [];
    return (mapped as RecordingItem[]).length ? (mapped as RecordingItem[]) : defaultRecordings;
  }, [recordingsQuery.data]);

  const heroTitle = 'Historias desde el estudio, releases y TDF Sessions en un solo lugar.';
  const heroSubtitle =
    'Mantén al día la página pública de TDF Records con fotos, lanzamientos y videos curados desde el CMS.';
  const heroCta = 'Reservar sesión';
  const heroSecondaryCta = 'Ver lanzamientos';

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
            <PublicBrandBar tagline="TDF Records · Estudio · Label · Sessions" />
          </Box>
          <Stack spacing={3} maxWidth="md">
            <Chip
              label="TDF Records — Public CMS"
              sx={{
                width: 'fit-content',
                bgcolor: 'rgba(255,255,255,0.08)',
                color: '#e5e7eb',
                fontWeight: 700,
              }}
            />
            <Typography variant="h2" sx={{ fontWeight: 900, letterSpacing: '-0.03em', lineHeight: 1.05 }}>
              {heroTitle}
            </Typography>
            <Typography variant="h6" sx={{ color: 'rgba(226,232,240,0.82)', maxWidth: 640 }}>
              {heroSubtitle}
            </Typography>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
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
                Fan Hub
              </Button>
              {!hasBookingToken && (
                <Typography variant="body2" color="rgba(226,232,240,0.8)">
                  Para agendar automáticamente, agrega VITE_PUBLIC_BOOKING_TOKEN o usa el token demo.
                </Typography>
              )}
            </Stack>
          </Stack>
          <Stack
            direction={{ xs: 'column', md: 'row' }}
            spacing={3}
            sx={{ mt: 6, flexWrap: 'wrap' }}
          >
            <GradientCard title="Fotos del estudio">
              <Typography variant="body2" sx={{ color: 'text.secondary', mb: 1 }}>
                Actualiza imágenes y textos desde Configuración → CMS (slugs records-recording-*).
              </Typography>
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
            <GradientCard title="TDF Sessions">
              <Stack spacing={1}>
                {sessions.slice(0, 2).map((video) => (
                  <Stack key={video.youtubeId} direction="row" spacing={1} alignItems="center">
                    <Chip label="Video" size="small" sx={{ bgcolor: 'rgba(255,255,255,0.08)' }} />
                    <Typography variant="body2" sx={{ fontWeight: 700 }}>
                      {video.title}
                    </Typography>
                  </Stack>
                ))}
              </Stack>
            </GradientCard>
            <GradientCard title="Releases">
              <Stack spacing={1}>
                {releases.map((release) => (
                  <Typography key={release.title} variant="body2" sx={{ fontWeight: 700 }}>
                    {release.title} · {release.artist}
                  </Typography>
                ))}
              </Stack>
            </GradientCard>
          </Stack>
        </Container>
      </Box>

      <Container maxWidth="lg" sx={{ py: { xs: 6, md: 10 } }}>
        <Box sx={{ mb: 6 }}>
          <SectionTitle title="Grabaciones recientes" kicker="Estudio" />
          <Typography variant="body1" sx={{ color: 'text.secondary', maxWidth: 760, mb: 3 }}>
            Mantén este grid con las sesiones más frescas: foto hero, fecha y un párrafo corto sobre cómo se grabó. Edita desde Configuración → CMS.
          </Typography>
          <RecordingsGrid items={recordings} />
        </Box>

        <Box id="releases" sx={{ mb: 6 }}>
          <SectionTitle title="Lanzamientos TDF Records" kicker="Label" />
          <Typography variant="body1" sx={{ color: 'text.secondary', maxWidth: 760, mb: 3 }}>
            Cada release incluye los links oficiales. Edita los enlaces a plataformas en el CMS.
          </Typography>
          <ReleasesGrid items={releases} />
        </Box>

        <Box sx={{ mb: 6 }}>
          <SectionTitle title="TDF Sessions" kicker="YouTube" />
          <Typography variant="body1" sx={{ color: 'text.secondary', maxWidth: 760, mb: 3 }}>
            Inserta el ID de YouTube en la lista de sesiones para embeberlo aquí. Ideal para compartir en la página pública sin esfuerzo.
          </Typography>
          <SessionsGrid items={sessions} />
        </Box>

        <GradientCard title="Cómo actualizar este CMS">
          <Typography variant="body2" sx={{ color: 'text.secondary', mb: 1 }}>
            Usa el panel en Configuración → CMS con slugs records-release-*, records-session-* y records-recording-* para crear borradores, publicar y versionar contenido en es/en.
          </Typography>
        </GradientCard>
      </Container>
    </Box>
  );
}
