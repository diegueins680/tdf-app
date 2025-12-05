import { useCallback, useMemo, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Card,
  CardContent,
  Chip,
  Grid,
  LinearProgress,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { Bookings } from '../api/bookings';
import { Payments } from '../api/payments';
import { Trials } from '../api/trials';

const currency = (cents: number) =>
  cents.toLocaleString('es-EC', { style: 'currency', currency: 'USD', maximumFractionDigits: 2, minimumFractionDigits: 2 }).replace('USD', '$');

const formatDateTime = (iso: string) => {
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return '';
  return d.toLocaleString('es-EC', { weekday: 'short', month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' });
};

const toLocalInput = (iso: string) => {
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return '';
  const pad = (v: number) => String(v).padStart(2, '0');
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())}T${pad(d.getHours())}:${pad(d.getMinutes())}`;
};

const inputToDate = (val: string) => {
  if (!val) return null;
  const d = new Date(val);
  return Number.isNaN(d.getTime()) ? null : d;
};

export default function ReportsPage() {
  const bookingsQuery = useQuery({
    queryKey: ['reports-bookings'],
    queryFn: Bookings.list,
  });
  const paymentsQuery = useQuery({
    queryKey: ['reports-payments'],
    queryFn: () => Payments.list(),
  });
  const [fromInput, setFromInput] = useState(() => {
    const start = new Date();
    start.setDate(start.getDate() - 30);
    start.setHours(0, 0, 0, 0);
    return toLocalInput(start.toISOString());
  });
  const [toInput, setToInput] = useState(() => {
    const end = new Date();
    end.setHours(23, 59, 0, 0);
    return toLocalInput(end.toISOString());
  });
  const [serviceFilter, setServiceFilter] = useState<string>('all');
  const [bookingStatusFilter, setBookingStatusFilter] = useState<string>('all');
  const [classStatusFilter, setClassStatusFilter] = useState<string>('all');
  const [teacherFilter, setTeacherFilter] = useState<number | 'all'>('all');

  const fromDate = useMemo(() => inputToDate(fromInput), [fromInput]);
  const toDate = useMemo(() => inputToDate(toInput), [toInput]);
  const fromIso = fromDate?.toISOString();
  const toIso = toDate?.toISOString();

  const classesQuery = useQuery({
    queryKey: ['reports-classes', fromIso, toIso],
    queryFn: () =>
      Trials.listClassSessions({
        from: fromIso,
        to: toIso,
      }),
  });

  const bookings = useMemo(() => bookingsQuery.data ?? [], [bookingsQuery.data]);
  const payments = useMemo(() => paymentsQuery.data ?? [], [paymentsQuery.data]);
  const classes = useMemo(() => classesQuery.data ?? [], [classesQuery.data]);

  const services = useMemo(
    () => Array.from(new Set(bookings.map((b) => b.serviceType).filter((s): s is string => Boolean(s)))),
    [bookings],
  );
  const bookingStatuses = useMemo(
    () => Array.from(new Set(bookings.map((b) => b.status).filter((s): s is string => Boolean(s)))),
    [bookings],
  );
  const classStatuses = useMemo(
    () => Array.from(new Set(classes.map((c) => c.status).filter((s): s is string => Boolean(s)))),
    [classes],
  );
  const teacherOptions = useMemo(
    () =>
      Array.from(
        new Map(
          classes
            .filter((c) => c.teacherId && c.teacherName)
            .map((c) => [c.teacherId, c.teacherName ?? '']),
        ).entries(),
      ).map(([id, name]) => ({ id, name })),
    [classes],
  );

  const inRange = useCallback(
    (iso: string) => {
      const d = new Date(iso);
      if (Number.isNaN(d.getTime())) return false;
      if (fromDate && d < fromDate) return false;
      if (toDate && d > toDate) return false;
      return true;
    },
    [fromDate, toDate],
  );

  const filteredBookings = useMemo(
    () =>
      bookings.filter((b) => {
        if (!inRange(b.startsAt)) return false;
        if (serviceFilter !== 'all' && b.serviceType !== serviceFilter) return false;
        if (bookingStatusFilter !== 'all' && b.status !== bookingStatusFilter) return false;
        return true;
      }),
    [bookings, bookingStatusFilter, inRange, serviceFilter],
  );

  const filteredPayments = useMemo(
    () => payments.filter((p) => inRange(p.payPaidAt)),
    [payments, inRange],
  );

  const filteredClasses = useMemo(
    () =>
      classes.filter((c) => {
        if (!inRange(c.startAt)) return false;
        if (teacherFilter !== 'all' && c.teacherId !== teacherFilter) return false;
        if (classStatusFilter !== 'all' && c.status !== classStatusFilter) return false;
        return true;
      }),
    [classes, classStatusFilter, inRange, teacherFilter],
  );

  const now = useMemo(() => new Date(), []);
  const sevenDaysOut = useMemo(() => {
    const d = new Date();
    d.setDate(d.getDate() + 7);
    return d;
  }, []);

  const kpis = useMemo(() => {
    const upcomingBookings = filteredBookings.filter((b) => {
      const start = new Date(b.startsAt);
      return start >= now && start <= sevenDaysOut;
    });
    const upcomingClasses = filteredClasses.filter((c) => new Date(c.startAt) >= now);

    const revenueInRange = filteredPayments.reduce((acc, p) => acc + (p.payAmountCents ?? 0), 0);
    const revenueToday = filteredPayments
      .filter((p) => {
        const paid = new Date(p.payPaidAt);
        const today = new Date();
        return paid.toDateString() === today.toDateString();
      })
      .reduce((acc, p) => acc + (p.payAmountCents ?? 0), 0);

    return {
      upcomingBookingsCount: upcomingBookings.length,
      upcomingClassesCount: upcomingClasses.length,
      revenueRange: revenueInRange,
      revenueToday,
    };
  }, [filteredBookings, filteredClasses, filteredPayments, now, sevenDaysOut]);

  const summaryLoading = bookingsQuery.isLoading || paymentsQuery.isLoading || classesQuery.isLoading;
  const summaryError = bookingsQuery.error ?? paymentsQuery.error ?? classesQuery.error ?? null;

  const topBookings = filteredBookings
    .filter((b) => new Date(b.startsAt) >= now)
    .sort((a, b) => new Date(a.startsAt).getTime() - new Date(b.startsAt).getTime())
    .slice(0, 5);

  const topClasses = filteredClasses
    .filter((c) => new Date(c.startAt) >= now)
    .sort((a, b) => new Date(a.startAt).getTime() - new Date(b.startAt).getTime())
    .slice(0, 5);

  return (
    <Stack spacing={3}>
      <Stack spacing={0.5}>
        <Typography variant="overline" color="text.secondary">Estudio</Typography>
        <Typography variant="h4" fontWeight={800}>Reportes</Typography>
        <Typography color="text.secondary">
          KPIs rápidos de reservas, clases y cobros recientes. Ajusta filtros para ver el rango que necesitas.
        </Typography>
      </Stack>

      <Grid container spacing={2}>
        <Grid item xs={12} md={3}>
          <TextField
            label="Desde"
            type="datetime-local"
            value={fromInput}
            onChange={(e) => setFromInput(e.target.value)}
            InputLabelProps={{ shrink: true }}
            fullWidth
          />
        </Grid>
        <Grid item xs={12} md={3}>
          <TextField
            label="Hasta"
            type="datetime-local"
            value={toInput}
            onChange={(e) => setToInput(e.target.value)}
            InputLabelProps={{ shrink: true }}
            fullWidth
          />
        </Grid>
        <Grid item xs={12} md={2}>
          <TextField
            select
            label="Servicio"
            value={serviceFilter}
            onChange={(e) => setServiceFilter(e.target.value)}
            fullWidth
            SelectProps={{ native: true }}
          >
            <option value="all">Todos</option>
            {services.map((s) => (
              <option key={s} value={s}>
                {s}
              </option>
            ))}
          </TextField>
        </Grid>
        <Grid item xs={12} md={2}>
          <TextField
            select
            label="Estado reserva"
            value={bookingStatusFilter}
            onChange={(e) => setBookingStatusFilter(e.target.value)}
            fullWidth
            SelectProps={{ native: true }}
          >
            <option value="all">Todos</option>
            {bookingStatuses.map((s) => (
              <option key={s} value={s}>
                {s}
              </option>
            ))}
          </TextField>
        </Grid>
        <Grid item xs={12} md={2}>
          <TextField
            select
            label="Estado clase"
            value={classStatusFilter}
            onChange={(e) => setClassStatusFilter(e.target.value)}
            fullWidth
            SelectProps={{ native: true }}
          >
            <option value="all">Todos</option>
            {classStatuses.map((s) => (
              <option key={s} value={s}>
                {s}
              </option>
            ))}
          </TextField>
        </Grid>
        <Grid item xs={12} md={3}>
          <TextField
            select
            label="Profesor"
            value={teacherFilter}
            onChange={(e) => setTeacherFilter(e.target.value === 'all' ? 'all' : Number(e.target.value))}
            fullWidth
            SelectProps={{ native: true }}
          >
            <option value="all">Todos</option>
            {teacherOptions.map((t) => (
              <option key={t.id} value={t.id}>
                {t.name}
              </option>
            ))}
          </TextField>
        </Grid>
      </Grid>

      <Grid container spacing={2}>
        <Grid item xs={12} md={3}>
          <Card variant="outlined">
            <CardContent>
              <Typography color="text.secondary" variant="body2">Ingresos en rango</Typography>
              <Typography variant="h5" fontWeight={800}>{currency(kpis.revenueRange / 100)}</Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={12} md={3}>
          <Card variant="outlined">
            <CardContent>
              <Typography color="text.secondary" variant="body2">Ingresos hoy</Typography>
              <Typography variant="h5" fontWeight={800}>{currency(kpis.revenueToday / 100)}</Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={12} md={3}>
          <Card variant="outlined">
            <CardContent>
              <Typography color="text.secondary" variant="body2">Reservas próximas 7 días</Typography>
              <Typography variant="h5" fontWeight={800}>{kpis.upcomingBookingsCount}</Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={12} md={3}>
          <Card variant="outlined">
            <CardContent>
              <Typography color="text.secondary" variant="body2">Clases próximas</Typography>
              <Typography variant="h5" fontWeight={800}>{kpis.upcomingClassesCount}</Typography>
            </CardContent>
          </Card>
        </Grid>
      </Grid>

      {summaryLoading && <LinearProgress />}
      {summaryError && (
        <Alert severity="error">
          {summaryError instanceof Error ? summaryError.message : 'No se pudieron cargar los datos.'}
        </Alert>
      )}

      <Grid container spacing={2}>
        <Grid item xs={12} md={6}>
          <Card variant="outlined">
            <CardContent>
              <Stack direction="row" alignItems="center" justifyContent="space-between">
                <Typography variant="h6" fontWeight={800}>Reservas próximas</Typography>
                <Chip label={`${topBookings.length}`} size="small" />
              </Stack>
              <Stack spacing={1.25} sx={{ mt: 2 }}>
                {topBookings.length === 0 && <Typography color="text.secondary">No hay reservas próximas.</Typography>}
                {topBookings.map((b) => (
                  <Box key={b.bookingId} sx={{ border: '1px solid', borderColor: 'divider', borderRadius: 2, p: 1.25 }}>
                  <Typography fontWeight={700}>{b.title ?? b.serviceOrderTitle ?? 'Reserva'}</Typography>
                    <Typography variant="body2" color="text.secondary">{formatDateTime(b.startsAt)}</Typography>
                    <Typography variant="body2" color="text.secondary">
                    {b.partyDisplayName ?? b.customerName ?? 'Cliente no asignado'} · {b.status}
                    </Typography>
                  </Box>
                ))}
              </Stack>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={12} md={6}>
          <Card variant="outlined">
            <CardContent>
              <Stack direction="row" alignItems="center" justifyContent="space-between">
                <Typography variant="h6" fontWeight={800}>Clases próximas</Typography>
                <Chip label={`${topClasses.length}`} size="small" />
              </Stack>
              <Stack spacing={1.25} sx={{ mt: 2 }}>
                {topClasses.length === 0 && <Typography color="text.secondary">No hay clases próximas.</Typography>}
                {topClasses.map((cls) => (
                  <Box key={cls.classSessionId} sx={{ border: '1px solid', borderColor: 'divider', borderRadius: 2, p: 1.25 }}>
                    <Typography fontWeight={700}>{cls.subjectName ?? 'Materia'}</Typography>
                    <Typography variant="body2" color="text.secondary">{formatDateTime(cls.startAt)}</Typography>
                    <Typography variant="body2" color="text.secondary">
                      {cls.teacherName ?? 'Profesor'} · {cls.studentName ? `Alumno: ${cls.studentName}` : `Alumno ID ${cls.studentId}`}
                    </Typography>
                  </Box>
                ))}
              </Stack>
            </CardContent>
          </Card>
        </Grid>
      </Grid>
    </Stack>
  );
}
