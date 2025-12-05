import { useMemo } from 'react';
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

const addDays = (iso: string, days: number) => {
  const d = new Date(iso);
  d.setDate(d.getDate() + days);
  return d.toISOString();
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
  const classesQuery = useQuery({
    queryKey: ['reports-classes'],
    queryFn: () =>
      Trials.listClassSessions({
        from: new Date().toISOString(),
        to: addDays(new Date().toISOString(), 30),
      }),
  });

  const bookings = useMemo(() => bookingsQuery.data ?? [], [bookingsQuery.data]);
  const payments = useMemo(() => paymentsQuery.data ?? [], [paymentsQuery.data]);
  const classes = useMemo(() => classesQuery.data ?? [], [classesQuery.data]);

  const now = useMemo(() => new Date(), []);
  const sevenDaysOut = useMemo(() => {
    const d = new Date();
    d.setDate(d.getDate() + 7);
    return d;
  }, []);

  const kpis = useMemo(() => {
    const upcomingBookings = bookings.filter((b) => {
      const start = new Date(b.startsAt);
      return start >= now && start <= sevenDaysOut;
    });
    const upcomingClasses = classes.filter((c) => new Date(c.startAt) >= now);

    const last30Payments = payments.filter((p) => {
      const paid = new Date(p.payPaidAt);
      const cut = new Date();
      cut.setDate(cut.getDate() - 30);
      return paid >= cut && paid <= now;
    });

    const revenue30 = last30Payments.reduce((acc, p) => acc + (p.payAmountCents ?? 0), 0);
    const revenueToday = payments
      .filter((p) => {
        const paid = new Date(p.payPaidAt);
        const today = new Date();
        return paid.toDateString() === today.toDateString();
      })
      .reduce((acc, p) => acc + (p.payAmountCents ?? 0), 0);

    return {
      upcomingBookingsCount: upcomingBookings.length,
      upcomingClassesCount: upcomingClasses.length,
      revenue30,
      revenueToday,
    };
  }, [bookings, classes, payments, now, sevenDaysOut]);

  const summaryLoading = bookingsQuery.isLoading || paymentsQuery.isLoading || classesQuery.isLoading;
  const summaryError = bookingsQuery.error ?? paymentsQuery.error ?? classesQuery.error ?? null;

  const topBookings = bookings
    .filter((b) => new Date(b.startsAt) >= now)
    .sort((a, b) => new Date(a.startsAt).getTime() - new Date(b.startsAt).getTime())
    .slice(0, 5);

  const topClasses = classes
    .filter((c) => new Date(c.startAt) >= now)
    .sort((a, b) => new Date(a.startAt).getTime() - new Date(b.startAt).getTime())
    .slice(0, 5);

  return (
    <Stack spacing={3}>
      <Stack spacing={0.5}>
        <Typography variant="overline" color="text.secondary">Estudio</Typography>
        <Typography variant="h4" fontWeight={800}>Reportes</Typography>
        <Typography color="text.secondary">
          KPIs rápidos de reservas, clases y cobros recientes.
        </Typography>
      </Stack>

      <Grid container spacing={2}>
        <Grid item xs={12} md={3}>
          <Card variant="outlined">
            <CardContent>
              <Typography color="text.secondary" variant="body2">Ingresos últimos 30 días</Typography>
              <Typography variant="h5" fontWeight={800}>{currency(kpis.revenue30 / 100)}</Typography>
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
