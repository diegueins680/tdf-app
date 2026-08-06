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
import { useTranslation } from 'react-i18next';
import LazyPaginatedList from './LazyPaginatedList';
import { formatDateForUser } from '../utils/formatters';

const fmtDateTime = (iso: string) =>
  formatDateForUser(iso, {
    weekday: 'short',
    month: 'short',
    day: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
  });

const byStartDesc = <T,>(getIso: (value: T) => string) => (a: T, b: T) => {
  const ta = new Date(getIso(a)).getTime();
  const tb = new Date(getIso(b)).getTime();
  return (Number.isNaN(tb) ? 0 : tb) - (Number.isNaN(ta) ? 0 : ta);
};

const BOOKING_CUSTOMER_ROLES = new Set(['customer', 'cliente']);
const BOOKING_ENGINEER_ROLES = new Set(['engineer', 'ingeniero']);
const CLASS_STUDENT_ROLES = new Set(['student', 'estudiante']);
const CLASS_TEACHER_ROLES = new Set(['teacher', 'profesor']);
const hasRelatedRole = (role: string, supportedRoles: ReadonlySet<string>) =>
  supportedRoles.has(role.trim().toLowerCase());

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
  const { t } = useTranslation();

  if (count === 0) return null;

  return (
    <Box sx={{ flex: 1, minWidth: 0 }}>
      <Typography variant="body2" fontWeight={700}>
        {title} ({count})
      </Typography>
      <LazyPaginatedList
        items={bookings}
        pagination={{ itemLabel: t('partyRelated.bookingItems'), initialRowsPerPage: 5 }}
        renderItems={(visibleBookings) => (
          <List dense disablePadding>
            {visibleBookings.map((booking) => (
              <RelatedListItem
                key={booking.prbBookingId}
                primary={booking.prbServiceType ?? booking.prbTitle ?? t('partyRelated.bookingFallback', {
                  id: booking.prbBookingId,
                })}
                secondary={`${fmtDateTime(booking.prbStartsAt)} · ${t(
                  `partyRelated.statuses.${booking.prbStatus.trim().toLowerCase()}`,
                  { defaultValue: booking.prbStatus },
                )}`}
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
  const { t } = useTranslation();

  if (customerCount === 0 && engineerCount === 0) return null;

  return (
    <Box>
      <Typography variant="overline" color="text.secondary">
        {t('partyRelated.bookings')}
      </Typography>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems="flex-start">
        <BookingColumn title={t('partyRelated.customer')} count={customerCount} bookings={customerBookings} onGo={onGo} />
        <BookingColumn title={t('partyRelated.engineer')} count={engineerCount} bookings={engineerBookings} onGo={onGo} />
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
  const { t } = useTranslation();

  if (count === 0) return null;

  return (
    <Box sx={{ flex: 1, minWidth: 0 }}>
      <Typography variant="body2" fontWeight={700}>
        {title} ({count})
      </Typography>
      <LazyPaginatedList
        items={sessions}
        pagination={{ itemLabel: t('partyRelated.classItems'), initialRowsPerPage: 5 }}
        renderItems={(visibleClasses) => (
          <List dense disablePadding>
            {visibleClasses.map((session) => {
              const partyParam = classRole === 'student' ? `studentId=${session.prcStudentId}` : `teacherId=${session.prcTeacherId}`;
              const primary =
                classRole === 'student'
                  ? session.prcSubjectName ?? t('partyRelated.subjectFallback', { id: session.prcSubjectId })
                  : `${session.prcSubjectName ?? t('partyRelated.subjectFallback', { id: session.prcSubjectId })} · ${
                      session.prcStudentName ?? t('partyRelated.studentFallback', { id: session.prcStudentId })
                    }`;

              return (
                <RelatedListItem
                  key={`${classRole}-${session.prcClassSessionId}`}
                  primary={primary}
                  secondary={`${fmtDateTime(session.prcStartAt)} · ${t(
                    `partyRelated.statuses.${session.prcStatus.trim().toLowerCase()}`,
                    { defaultValue: session.prcStatus },
                  )}`}
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
  const { t } = useTranslation();

  if (studentCount === 0 && teacherCount === 0) return null;

  return (
    <Box>
      <Typography variant="overline" color="text.secondary">
        {t('partyRelated.classes')}
      </Typography>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems="flex-start">
        <ClassColumn title={t('partyRelated.student')} count={studentCount} sessions={studentClasses} classRole="student" onGo={onGo} />
        <ClassColumn title={t('partyRelated.teacher')} count={teacherCount} sessions={teacherClasses} classRole="teacher" onGo={onGo} />
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
  const { t } = useTranslation();
  if (tracks.length === 0) return null;

  return (
    <Box>
      <Typography variant="overline" color="text.secondary">
        {t('partyRelated.tracks')}
      </Typography>
      <LazyPaginatedList
        items={tracks}
        pagination={{ itemLabel: t('partyRelated.trackItems'), initialRowsPerPage: 10 }}
        renderItems={(visibleTracks) => (
          <List dense disablePadding>
            {visibleTracks.map((track) => (
              <RelatedListItem
                key={track.prtId}
                primary={track.prtTitle}
                secondary={`${t(`partyRelated.statuses.${track.prtStatus.trim().toLowerCase()}`, {
                  defaultValue: track.prtStatus,
                })} · ${t('partyRelated.updated', { date: fmtDateTime(track.prtUpdatedAt) })}`}
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
  const { t } = useTranslation();
  const open = Boolean(party && anchorEl);

  const relatedQuery = useQuery<PartyRelatedDTO>({
    queryKey: ['party-related', party?.partyId ?? 'none'],
    queryFn: () => {
      if (!party) throw new Error(t('partyRelated.unavailable'));
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
        .filter((b) => hasRelatedRole(b.prbRole, BOOKING_CUSTOMER_ROLES))
        .sort(byStartDesc((b) => b.prbStartsAt))
        .slice(0, 8),
    [bookings],
  );
  const bookingsEngineer = useMemo(
    () =>
      bookings
        .filter((b) => hasRelatedRole(b.prbRole, BOOKING_ENGINEER_ROLES))
        .sort(byStartDesc((b) => b.prbStartsAt))
        .slice(0, 8),
    [bookings],
  );
  const classesStudent = useMemo(
    () =>
      classSessions
        .filter((c) => hasRelatedRole(c.prcRole, CLASS_STUDENT_ROLES))
        .sort(byStartDesc((c) => c.prcStartAt))
        .slice(0, 8),
    [classSessions],
  );
  const classesTeacher = useMemo(
    () =>
      classSessions
        .filter((c) => hasRelatedRole(c.prcRole, CLASS_TEACHER_ROLES))
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
  const bookingsCustomerCount = bookings.filter((b) => hasRelatedRole(b.prbRole, BOOKING_CUSTOMER_ROLES)).length;
  const bookingsEngineerCount = bookings.filter((b) => hasRelatedRole(b.prbRole, BOOKING_ENGINEER_ROLES)).length;
  const classesStudentCount = classSessions.filter((c) => hasRelatedRole(c.prcRole, CLASS_STUDENT_ROLES)).length;
  const classesTeacherCount = classSessions.filter((c) => hasRelatedRole(c.prcRole, CLASS_TEACHER_ROLES)).length;
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
            <Typography fontWeight={800}>{party?.displayName ?? t('partyRelated.contact')}</Typography>
            <Typography variant="body2" color="text.secondary">
              {t('partyRelated.subtitle')}
            </Typography>
          </Stack>
          <Stack direction="row" spacing={1} alignItems="center">
            {party?.isOrg && <Chip size="small" label={t('partyRelated.organization')} />}
            {party?.hasUserAccount && <Chip size="small" label={t('partyRelated.user')} color="primary" variant="outlined" />}
          </Stack>
        </Stack>

        <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
          {party?.partyId != null && (
            <>
              <QuickNavButton label={t('partyRelated.profile')} path={`/perfil/${party.partyId}`} onGo={go} />
              {bookingsCustomerCount > 0 && (
                <QuickNavButton label={t('partyRelated.customerBookings')} path={`/estudio/calendario?partyId=${party.partyId}`} onGo={go} />
              )}
              {bookingsEngineerCount > 0 && (
                <QuickNavButton
                  label={t('partyRelated.engineerBookings')}
                  path={`/estudio/calendario?engineerPartyId=${party.partyId}`}
                  onGo={go}
                />
              )}
              {classesStudentCount > 0 && (
                <QuickNavButton label={t('partyRelated.studentClasses')} path={`/escuela/clases?studentId=${party.partyId}`} onGo={go} />
              )}
              {classesTeacherCount > 0 && (
                <QuickNavButton label={t('partyRelated.teacherClasses')} path={`/escuela/clases?teacherId=${party.partyId}`} onGo={go} />
              )}
              {hasTracks && (
                <QuickNavButton label={t('partyRelated.tracks')} path={`/label/tracks?ownerId=${party.partyId}`} onGo={go} />
              )}
            </>
          )}
        </Stack>

        <Divider />

        {relatedQuery.isLoading && (
          <Box sx={{ display: 'flex', justifyContent: 'center', py: 2 }}>
            <CircularProgress aria-label={t('partyRelated.loading')} size={22} />
          </Box>
        )}
        {relatedQuery.isError && (
          <Alert severity="error">
            {t('partyRelated.loadError')}
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
              {t('partyRelated.empty')}
            </Alert>
          )
        )}
      </Stack>
    </Popover>
  );
}
