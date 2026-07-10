import { useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Chip,
  CircularProgress,
  Divider,
  List,
  ListItemButton,
  ListItemText,
  Popover,
  Stack,
  Typography,
} from '@mui/material';
import type { PartyDTO, PartyRelatedDTO } from '../api/types';
import { Parties } from '../api/parties';
import { useNavigate } from 'react-router-dom';
import LazyPaginatedList from './LazyPaginatedList';

const fmtDateTime = (iso: string) => {
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return iso;
  return d.toLocaleString('es-EC', {
    weekday: 'short',
    month: 'short',
    day: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
  });
};

const byStartDesc = <T,>(getIso: (value: T) => string) => (a: T, b: T) => {
  const ta = new Date(getIso(a)).getTime();
  const tb = new Date(getIso(b)).getTime();
  return (Number.isNaN(tb) ? 0 : tb) - (Number.isNaN(ta) ? 0 : ta);
};

type GoToPath = (path: string) => void;
type RelatedBooking = PartyRelatedDTO['prBookings'][number];
type RelatedClassSession = PartyRelatedDTO['prClassSessions'][number];
type RelatedTrack = PartyRelatedDTO['prLabelTracks'][number];

interface QuickNavButtonProps {
  label: string;
  path: string;
  onGo: GoToPath;
}

function QuickNavButton({ label, path, onGo }: QuickNavButtonProps) {
  return (
    <Button
      size="small"
      variant="outlined"
      tabIndex={0}
      onClick={(event) => {
        event.currentTarget.focus();
        onGo(path);
      }}
    >
      {label}
    </Button>
  );
}

interface RelatedListItemProps {
  primary: string;
  secondary: string;
  path: string;
  onGo: GoToPath;
}

function RelatedListItem({ primary, secondary, path, onGo }: RelatedListItemProps) {
  return (
    <ListItemButton
      tabIndex={0}
      onClick={(event) => {
        event.currentTarget.focus();
        onGo(path);
      }}
    >
      <ListItemText primary={primary} secondary={secondary} />
    </ListItemButton>
  );
}

interface BookingColumnProps {
  title: string;
  count: number;
  bookings: readonly RelatedBooking[];
  onGo: GoToPath;
}

function BookingColumn(props: BookingColumnProps) {
  const { title, count, bookings, onGo } = props;

  if (count === 0) return null;

  return (
    <Box sx={{ flex: 1, minWidth: 0 }}>
      <Typography variant="body2" fontWeight={700}>
        {title} ({count})
      </Typography>
      <LazyPaginatedList
        items={bookings}
        pagination={{ itemLabel: 'reservas', initialRowsPerPage: 5 }}
        renderItems={(visibleBookings) => (
          <List dense disablePadding>
            {visibleBookings.map((booking) => (
              <RelatedListItem
                key={booking.prbBookingId}
                primary={booking.prbServiceType ?? booking.prbTitle ?? `Booking #${booking.prbBookingId}`}
                secondary={`${fmtDateTime(booking.prbStartsAt)} · ${booking.prbStatus}`}
                path={`/estudio/calendario?bookingId=${booking.prbBookingId}`}
                onGo={onGo}
              />
            ))}
          </List>
        )}
      />
    </Box>
  );
}

interface BookingsSectionProps {
  customerCount: number;
  engineerCount: number;
  customerBookings: readonly RelatedBooking[];
  engineerBookings: readonly RelatedBooking[];
  onGo: GoToPath;
}

function BookingsSection(props: BookingsSectionProps) {
  const { customerCount, engineerCount, customerBookings, engineerBookings, onGo } = props;

  if (customerCount === 0 && engineerCount === 0) return null;

  return (
    <Box>
      <Typography variant="overline" color="text.secondary">
        Reservas
      </Typography>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems="flex-start">
        <BookingColumn title="Cliente" count={customerCount} bookings={customerBookings} onGo={onGo} />
        <BookingColumn title="Ingeniero" count={engineerCount} bookings={engineerBookings} onGo={onGo} />
      </Stack>
    </Box>
  );
}

interface ClassColumnProps {
  title: string;
  count: number;
  sessions: readonly RelatedClassSession[];
  classRole: 'student' | 'teacher';
  onGo: GoToPath;
}

function ClassColumn(props: ClassColumnProps) {
  const { title, count, sessions, classRole, onGo } = props;

  if (count === 0) return null;

  return (
    <Box sx={{ flex: 1, minWidth: 0 }}>
      <Typography variant="body2" fontWeight={700}>
        {title} ({count})
      </Typography>
      <LazyPaginatedList
        items={sessions}
        pagination={{ itemLabel: 'clases', initialRowsPerPage: 5 }}
        renderItems={(visibleClasses) => (
          <List dense disablePadding>
            {visibleClasses.map((session) => {
              const partyParam = classRole === 'student' ? `studentId=${session.prcStudentId}` : `teacherId=${session.prcTeacherId}`;
              const primary =
                classRole === 'student'
                  ? session.prcSubjectName ?? `Materia #${session.prcSubjectId}`
                  : `${session.prcSubjectName ?? `Materia #${session.prcSubjectId}`} · ${
                      session.prcStudentName ?? `Alumno #${session.prcStudentId}`
                    }`;

              return (
                <RelatedListItem
                  key={`${classRole}-${session.prcClassSessionId}`}
                  primary={primary}
                  secondary={`${fmtDateTime(session.prcStartAt)} · ${session.prcStatus}`}
                  path={`/escuela/clases?${partyParam}&classSessionId=${session.prcClassSessionId}&at=${encodeURIComponent(
                    session.prcStartAt,
                  )}`}
                  onGo={onGo}
                />
              );
            })}
          </List>
        )}
      />
    </Box>
  );
}

interface ClassesSectionProps {
  studentCount: number;
  teacherCount: number;
  studentClasses: readonly RelatedClassSession[];
  teacherClasses: readonly RelatedClassSession[];
  onGo: GoToPath;
}

function ClassesSection(props: ClassesSectionProps) {
  const { studentCount, teacherCount, studentClasses, teacherClasses, onGo } = props;

  if (studentCount === 0 && teacherCount === 0) return null;

  return (
    <Box>
      <Typography variant="overline" color="text.secondary">
        Clases
      </Typography>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems="flex-start">
        <ClassColumn title="Estudiante" count={studentCount} sessions={studentClasses} classRole="student" onGo={onGo} />
        <ClassColumn title="Profesor" count={teacherCount} sessions={teacherClasses} classRole="teacher" onGo={onGo} />
      </Stack>
    </Box>
  );
}

interface TracksSectionProps {
  tracks: readonly RelatedTrack[];
  partyId: number | string;
  onGo: GoToPath;
}

function TracksSection({ tracks, partyId, onGo }: TracksSectionProps) {
  if (tracks.length === 0) return null;

  return (
    <Box>
      <Typography variant="overline" color="text.secondary">
        Tracks
      </Typography>
      <LazyPaginatedList
        items={tracks}
        pagination={{ itemLabel: 'tracks', initialRowsPerPage: 10 }}
        renderItems={(visibleTracks) => (
          <List dense disablePadding>
            {visibleTracks.map((track) => (
              <RelatedListItem
                key={track.prtId}
                primary={track.prtTitle}
                secondary={`${track.prtStatus} · actualizado: ${fmtDateTime(track.prtUpdatedAt)}`}
                path={`/label/tracks?ownerId=${partyId}&trackId=${encodeURIComponent(track.prtId)}`}
                onGo={onGo}
              />
            ))}
          </List>
        )}
      />
    </Box>
  );
}

interface RelatedHistoryProps {
  bookingsCustomerCount: number;
  bookingsEngineerCount: number;
  classesStudentCount: number;
  classesTeacherCount: number;
  bookingsCustomer: readonly RelatedBooking[];
  bookingsEngineer: readonly RelatedBooking[];
  classesStudent: readonly RelatedClassSession[];
  classesTeacher: readonly RelatedClassSession[];
  tracks: readonly RelatedTrack[];
  partyId: number | string;
  onGo: GoToPath;
}

function RelatedHistory(props: RelatedHistoryProps) {
  const {
    bookingsCustomerCount,
    bookingsEngineerCount,
    classesStudentCount,
    classesTeacherCount,
    bookingsCustomer,
    bookingsEngineer,
    classesStudent,
    classesTeacher,
    tracks,
    partyId,
    onGo,
  } = props;

  return (
    <Stack spacing={1.5}>
      <BookingsSection
        customerCount={bookingsCustomerCount}
        engineerCount={bookingsEngineerCount}
        customerBookings={bookingsCustomer}
        engineerBookings={bookingsEngineer}
        onGo={onGo}
      />
      <ClassesSection
        studentCount={classesStudentCount}
        teacherCount={classesTeacherCount}
        studentClasses={classesStudent}
        teacherClasses={classesTeacher}
        onGo={onGo}
      />
      <TracksSection tracks={tracks} partyId={partyId} onGo={onGo} />
    </Stack>
  );
}

interface PartyRelatedPopoverProps {
  party: PartyDTO | null;
  anchorEl: HTMLElement | null;
  onClose: () => void;
}

export default function PartyRelatedPopover({ party, anchorEl, onClose }: PartyRelatedPopoverProps) {
  const navigate = useNavigate();
  const open = Boolean(party && anchorEl);

  const relatedQuery = useQuery<PartyRelatedDTO>({
    queryKey: ['party-related', party?.partyId ?? 'none'],
    queryFn: () => {
      if (!party) throw new Error('Party no disponible');
      return Parties.related(party.partyId);
    },
    enabled: open,
    staleTime: 30_000,
    retry: 1,
  });

  const bookings = useMemo(() => relatedQuery.data?.prBookings ?? [], [relatedQuery.data]);
  const classSessions = useMemo(() => relatedQuery.data?.prClassSessions ?? [], [relatedQuery.data]);
  const tracks = useMemo(() => relatedQuery.data?.prLabelTracks ?? [], [relatedQuery.data]);

  const bookingsCustomer = useMemo(
    () =>
      bookings
        .filter((b) => b.prbRole === 'cliente')
        .sort(byStartDesc((b) => b.prbStartsAt))
        .slice(0, 8),
    [bookings],
  );
  const bookingsEngineer = useMemo(
    () =>
      bookings
        .filter((b) => b.prbRole === 'ingeniero')
        .sort(byStartDesc((b) => b.prbStartsAt))
        .slice(0, 8),
    [bookings],
  );
  const classesStudent = useMemo(
    () =>
      classSessions
        .filter((c) => c.prcRole === 'estudiante')
        .sort(byStartDesc((c) => c.prcStartAt))
        .slice(0, 8),
    [classSessions],
  );
  const classesTeacher = useMemo(
    () =>
      classSessions
        .filter((c) => c.prcRole === 'profesor')
        .sort(byStartDesc((c) => c.prcStartAt))
        .slice(0, 8),
    [classSessions],
  );
  const tracksSorted = useMemo(
    () =>
      [...tracks]
        .sort(byStartDesc((t) => t.prtUpdatedAt))
        .slice(0, 10),
    [tracks],
  );
  const bookingsCustomerCount = bookings.filter((b) => b.prbRole === 'cliente').length;
  const bookingsEngineerCount = bookings.filter((b) => b.prbRole === 'ingeniero').length;
  const classesStudentCount = classSessions.filter((c) => c.prcRole === 'estudiante').length;
  const classesTeacherCount = classSessions.filter((c) => c.prcRole === 'profesor').length;
  const hasBookings = bookingsCustomerCount > 0 || bookingsEngineerCount > 0;
  const hasClasses = classesStudentCount > 0 || classesTeacherCount > 0;
  const hasTracks = tracks.length > 0;
  const hasRelatedHistory = hasBookings || hasClasses || hasTracks;

  const go = (path: string) => {
    navigate(path);
    onClose();
  };

  return (
    <Popover
      open={open}
      anchorEl={anchorEl}
      onClose={onClose}
      anchorOrigin={{ vertical: 'bottom', horizontal: 'left' }}
      transformOrigin={{ vertical: 'top', horizontal: 'left' }}
      PaperProps={{ sx: { width: { xs: 360, sm: 520 }, maxWidth: '90vw', p: 2 } }}
    >
      <Stack spacing={1.25}>
        <Stack direction="row" spacing={1} alignItems="center" justifyContent="space-between">
          <Stack spacing={0}>
            <Typography fontWeight={800}>{party?.displayName ?? 'Contacto'}</Typography>
            <Typography variant="body2" color="text.secondary">
              Navegación rápida por historial relacionado
            </Typography>
          </Stack>
          <Stack direction="row" spacing={1} alignItems="center">
            {party?.isOrg && <Chip size="small" label="ORG" />}
            {party?.hasUserAccount && <Chip size="small" label="Usuario" color="primary" variant="outlined" />}
          </Stack>
        </Stack>

        <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
          {party?.partyId != null && (
            <>
              <QuickNavButton label="Perfil" path={`/perfil/${party.partyId}`} onGo={go} />
              {bookingsCustomerCount > 0 && (
                <QuickNavButton label="Reservas (cliente)" path={`/estudio/calendario?partyId=${party.partyId}`} onGo={go} />
              )}
              {bookingsEngineerCount > 0 && (
                <QuickNavButton
                  label="Reservas (ingeniero)"
                  path={`/estudio/calendario?engineerPartyId=${party.partyId}`}
                  onGo={go}
                />
              )}
              {classesStudentCount > 0 && (
                <QuickNavButton label="Clases (estudiante)" path={`/escuela/clases?studentId=${party.partyId}`} onGo={go} />
              )}
              {classesTeacherCount > 0 && (
                <QuickNavButton label="Clases (profesor)" path={`/escuela/clases?teacherId=${party.partyId}`} onGo={go} />
              )}
              {hasTracks && (
                <QuickNavButton label="Tracks" path={`/label/tracks?ownerId=${party.partyId}`} onGo={go} />
              )}
            </>
          )}
        </Stack>

        <Divider />

        {relatedQuery.isLoading && (
          <Box sx={{ display: 'flex', justifyContent: 'center', py: 2 }}>
            <CircularProgress size={22} />
          </Box>
        )}
        {relatedQuery.isError && (
          <Alert severity="error">
            {relatedQuery.error instanceof Error ? relatedQuery.error.message : 'No se pudieron cargar los relacionados.'}
          </Alert>
        )}

        {!relatedQuery.isLoading && relatedQuery.data && (
          hasRelatedHistory ? (
            <RelatedHistory
              bookingsCustomerCount={bookingsCustomerCount}
              bookingsEngineerCount={bookingsEngineerCount}
              classesStudentCount={classesStudentCount}
              classesTeacherCount={classesTeacherCount}
              bookingsCustomer={bookingsCustomer}
              bookingsEngineer={bookingsEngineer}
              classesStudent={classesStudent}
              classesTeacher={classesTeacher}
              tracks={tracksSorted}
              partyId={relatedQuery.data.prPartyId}
              onGo={go}
            />
          ) : (
            <Alert severity="info" variant="outlined">
              No hay historial relacionado todavía. Usa Perfil para revisar o completar este contacto.
            </Alert>
          )
        )}
      </Stack>
    </Popover>
  );
}
