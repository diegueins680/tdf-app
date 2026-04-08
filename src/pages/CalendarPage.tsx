import { useMemo, useState } from 'react';
import {
  Avatar,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Divider,
  Grid,
  IconButton,
  Link,
  List,
  ListItem,
  ListItemAvatar,
  ListItemText,
  Stack,
  Tab,
  Tabs,
  Tooltip,
  Typography,
} from '@mui/material';
import CalendarMonthIcon from '@mui/icons-material/CalendarMonth';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import EventBusyIcon from '@mui/icons-material/EventBusy';
import LaunchIcon from '@mui/icons-material/Launch';
import PeopleAltIcon from '@mui/icons-material/PeopleAlt';
import ReplayIcon from '@mui/icons-material/Replay';
import ScheduleIcon from '@mui/icons-material/Schedule';

type CalendarMode = 'week' | 'month' | 'agenda';

const GOOGLE_CALENDAR_EMBED_BASE =
  'https://calendar.google.com/calendar/embed?src=tdfrecords.com_9q5d4qk4rgr5ihj58i056d10m8%40group.calendar.google.com' +
  '&ctz=America%2FGuayaquil&showPrint=0&showTabs=0&showCalendars=0&showDate=1&showTitle=0&showTz=0';

const CTA_LINKS = [
  {
    label: 'Solicitar fecha · Rentals',
    href: 'https://cal.com/tdfdomo/rentals',
    color: 'primary' as const,
  },
  {
    label: 'Solicitar fecha · Artístico',
    href: 'https://cal.com/tdfdomo/artistico',
    color: 'secondary' as const,
  },
  {
    label: 'Solicitar fecha · Familiar',
    href: 'https://cal.com/tdfdomo/familiar',
    color: 'inherit' as const,
  },
];

const PIPELINE_SUMMARY = [
  { label: 'Rentals confirmados', value: '6 eventos', subtitle: 'Próximos 30 días', icon: CalendarMonthIcon },
  { label: 'Producción artística', value: '4 montajes', subtitle: 'Equipos asignados', icon: PeopleAltIcon },
  { label: 'Reservas familiares', value: '3 celebraciones', subtitle: 'Con logística completa', icon: EventAvailableIcon },
];

const UPCOMING_BLOCKS = [
  {
    id: 'DOMO-2410',
    title: 'Montaje sonido para showcase',
    span: 'Hoy · 14:00 – 18:00',
    owner: 'Producción',
    status: 'Confirmado',
  },
  {
    id: 'DOMO-2411',
    title: 'Ensayo general / Tour Solar',
    span: 'Mañana · 09:00 – 13:00',
    owner: 'Artístico',
    status: 'Pendiente rider',
  },
  {
    id: 'DOMO-2412',
    title: 'Boda Andrea + Diego',
    span: 'Sábado · 16:00 – 02:00',
    owner: 'Eventos',
    status: 'Coordinación onsite',
  },
];

const QUICK_NOTES = [
  {
    title: 'Bloqueos fijos',
    description: 'Mantenimiento cada lunes 08:00 – 11:00 · Limpieza profunda jueves 20:00 – 22:00.',
  },
  {
    title: 'Checklist Rentals',
    description: 'Confirmar depósito 48h antes · Enviar layout definitivo · Coordinar staff montaje.',
  },
  {
    title: 'Próxima reunión operativa',
    description: 'Martes 09:30 · Tema: calendario noviembre y turnos staff.',
  },
];

const CONTACTS = [
  {
    name: 'Carla Pérez',
    role: 'Coordinación Rentals',
    email: 'carla@tdfrecords.com',
    phone: '+593 99 123 4567',
  },
  {
    name: 'Mateo Rojas',
    role: 'Producción Técnica',
    email: 'mateo@tdfrecords.com',
    phone: '+593 98 765 4321',
  },
  {
    name: 'Equipo Operaciones',
    role: 'Guardia domo',
    email: 'ops@tdfrecords.com',
    phone: '+593 96 555 0000',
  },
];

const MODE_LABELS: Record<CalendarMode, string> = {
  week: 'Semana',
  month: 'Mes',
  agenda: 'Agenda',
};

export default function CalendarPage() {
  const [mode, setMode] = useState<CalendarMode>('week');

  const embedSrc = useMemo(() => {
    const suffix = mode === 'month' ? 'MONTH' : mode === 'agenda' ? 'AGENDA' : 'WEEK';
    return `${GOOGLE_CALENDAR_EMBED_BASE}&mode=${suffix}`;
  }, [mode]);

  return (
    <Box component="main" sx={{ bgcolor: '#f7f9fc', minHeight: '100vh', py: { xs: 4, md: 6 }, px: { xs: 2, md: 6 } }}>
      <Stack spacing={4} maxWidth={1280} mx="auto">
        <Stack direction={{ xs: 'column', md: 'row' }} spacing={3} alignItems={{ md: 'center' }}>
          <Box flex={1}>
            <Typography variant="h3" component="h1" fontWeight={600} gutterBottom>
              Calendario operativo del Domo
            </Typography>
            <Typography variant="body1" color="text.secondary">
              Vista unificada de reservas <strong>Rentals</strong>, <strong>Artístico/Producción</strong> y <strong>Familiar/Privado</strong>.
              Usa los accesos rápidos para agendar y coordinar con los equipos.
            </Typography>
          </Box>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} flexWrap="wrap">
            <Button
              component="a"
              href="/"
              variant="outlined"
              color="inherit"
              sx={{ fontWeight: 600 }}
            >
              ← Volver a TDF HQ
            </Button>
            {CTA_LINKS.map(link => (
              <Button
                key={link.href}
                component="a"
                href={link.href}
                target="_blank"
                rel="noreferrer"
                variant="contained"
                color={link.color}
                endIcon={<LaunchIcon fontSize="small" />}
                sx={{ fontWeight: 600 }}
              >
                {link.label}
              </Button>
            ))}
          </Stack>
        </Stack>

        <Grid container spacing={3}>
          {PIPELINE_SUMMARY.map(({ label, value, subtitle, icon: Icon }) => (
            <Grid key={label} item xs={12} sm={6} md={4}>
              <Card elevation={0} sx={{ borderRadius: 3, border: '1px solid', borderColor: 'divider' }}>
                <CardContent>
                  <Stack direction="row" spacing={2} alignItems="center">
                    <Avatar sx={{ bgcolor: 'primary.main', color: 'primary.contrastText' }}>
                      <Icon fontSize="small" />
                    </Avatar>
                    <Box>
                      <Typography variant="subtitle2" color="text.secondary">
                        {label}
                      </Typography>
                      <Typography variant="h6" fontWeight={600}>
                        {value}
                      </Typography>
                      <Typography variant="caption" color="text.secondary">
                        {subtitle}
                      </Typography>
                    </Box>
                  </Stack>
                </CardContent>
              </Card>
            </Grid>
          ))}
        </Grid>

        <Grid container spacing={3}>
          <Grid item xs={12} md={8}>
            <Card elevation={0} sx={{ borderRadius: 3, border: '1px solid', borderColor: 'divider' }}>
              <CardContent>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} justifyContent="space-between" alignItems={{ sm: 'center' }}>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <ScheduleIcon fontSize="small" color="primary" />
                    <Typography variant="h6" fontWeight={600}>
                      Agenda compartida
                    </Typography>
                    <Chip label="Sincronizado" color="success" size="small" />
                  </Stack>
                  <Tabs
                    value={mode}
                    onChange={(_, value: CalendarMode) => setMode(value)}
                    variant="scrollable"
                    allowScrollButtonsMobile
                  >
                    {Object.entries(MODE_LABELS).map(([key, label]) => (
                      <Tab
                        key={key}
                        value={key}
                        label={label}
                        iconPosition="start"
                        icon={<CalendarMonthIcon fontSize="small" />}
                      />
                    ))}
                  </Tabs>
                </Stack>
                <Box mt={3}>
                  <Box
                    sx={{
                      position: 'relative',
                      paddingTop: '62%',
                      borderRadius: 2,
                      overflow: 'hidden',
                      border: '1px solid',
                      borderColor: 'divider',
                      bgcolor: 'background.paper',
                    }}
                  >
                    <Box
                      component="iframe"
                      title="Calendario DOMO"
                      src={embedSrc}
                      sx={{
                        position: 'absolute',
                        inset: 0,
                        width: '100%',
                        height: '100%',
                        border: 0,
                      }}
                      allowFullScreen
                    />
                  </Box>
                  <Typography variant="caption" color="text.secondary" display="block" mt={1.5}>
                    Para ver detalles internos (precios, notas, contactos) inicia sesión con tu cuenta del equipo.
                  </Typography>
                </Box>
              </CardContent>
            </Card>
          </Grid>

          <Grid item xs={12} md={4}>
            <Stack spacing={3}>
              <Card elevation={0} sx={{ borderRadius: 3, border: '1px solid', borderColor: 'divider' }}>
                <CardContent>
                  <Stack direction="row" justifyContent="space-between" alignItems="center">
                    <Typography variant="h6" fontWeight={600}>
                      Highlights semana
                    </Typography>
                    <Tooltip title="Actualizar desde FullCalendar">
                      <IconButton size="small">
                        <ReplayIcon fontSize="small" />
                      </IconButton>
                    </Tooltip>
                  </Stack>
                  <List dense disablePadding>
                    {UPCOMING_BLOCKS.map(block => (
                      <ListItem key={block.id} alignItems="flex-start" disableGutters sx={{ py: 1.5 }}>
                        <ListItemAvatar>
                          <Avatar sx={{ bgcolor: 'secondary.main', color: 'secondary.contrastText' }}>
                            <EventBusyIcon fontSize="small" />
                          </Avatar>
                        </ListItemAvatar>
                        <ListItemText
                          primary={
                            <Stack direction="row" justifyContent="space-between" alignItems="center" spacing={1}>
                              <Typography variant="subtitle2" fontWeight={600}>
                                {block.title}
                              </Typography>
                              <Chip label={block.status} size="small" color={block.status === 'Confirmado' ? 'success' : 'warning'} />
                            </Stack>
                          }
                          secondary={
                            <>
                              <Typography variant="body2" color="text.secondary">
                                {block.span}
                              </Typography>
                              <Typography variant="caption" color="text.secondary">
                                Responsable: {block.owner}
                              </Typography>
                            </>
                          }
                        />
                      </ListItem>
                    ))}
                  </List>
                </CardContent>
              </Card>

              <Card elevation={0} sx={{ borderRadius: 3, border: '1px solid', borderColor: 'divider' }}>
                <CardContent>
                  <Typography variant="h6" fontWeight={600} gutterBottom>
                    Contactos clave
                  </Typography>
                  <List dense disablePadding>
                    {CONTACTS.map(contact => (
                      <ListItem key={contact.email} disableGutters sx={{ py: 1.25 }}>
                        <ListItemAvatar>
                          <Avatar>{contact.name.slice(0, 2).toUpperCase()}</Avatar>
                        </ListItemAvatar>
                        <ListItemText
                          primary={
                            <Typography variant="subtitle2" fontWeight={600}>
                              {contact.name}
                            </Typography>
                          }
                          secondary={
                            <Stack spacing={0.25}>
                              <Typography variant="body2" color="text.secondary">
                                {contact.role}
                              </Typography>
                              <Typography variant="caption" color="text.secondary">
                                {contact.phone}
                              </Typography>
                              <Link href={`mailto:${contact.email}`} variant="caption">
                                {contact.email}
                              </Link>
                            </Stack>
                          }
                        />
                      </ListItem>
                    ))}
                  </List>
                </CardContent>
              </Card>
            </Stack>
          </Grid>
        </Grid>

        <Card elevation={0} sx={{ borderRadius: 3, border: '1px solid', borderColor: 'divider' }}>
          <CardContent>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={3} alignItems={{ md: 'flex-start' }}>
              <Box flex={1}>
                <Stack direction="row" spacing={1} alignItems="center" mb={1.5}>
                  <EventAvailableIcon color="primary" />
                  <Typography variant="h6" fontWeight={600}>
                    Notas operativas
                  </Typography>
                </Stack>
                <Grid container spacing={2}>
                  {QUICK_NOTES.map(note => (
                    <Grid key={note.title} item xs={12} md={4}>
                      <Box
                        sx={{
                          borderRadius: 2,
                          bgcolor: 'background.default',
                          border: '1px solid',
                          borderColor: 'divider',
                          p: 2,
                          height: '100%',
                        }}
                      >
                        <Typography variant="subtitle2" fontWeight={600} gutterBottom>
                          {note.title}
                        </Typography>
                        <Typography variant="body2" color="text.secondary">
                          {note.description}
                        </Typography>
                      </Box>
                    </Grid>
                  ))}
                </Grid>
              </Box>
              <Divider flexItem orientation="vertical" sx={{ display: { xs: 'none', md: 'block' } }} />
              <Box minWidth={{ md: 240 }}>
                <Typography variant="subtitle2" fontWeight={600} gutterBottom>
                  Recordatorios rápidos
                </Typography>
                <Stack spacing={1.5}>
                  <Chip icon={<EventBusyIcon />} label="Bloqueos de mantenimiento" variant="outlined" />
                  <Chip icon={<PeopleAltIcon />} label="Equipo Onsite asignado" variant="outlined" />
                  <Chip icon={<ScheduleIcon />} label="Turnos staff publicados" variant="outlined" />
                </Stack>
              </Box>
            </Stack>
          </CardContent>
        </Card>
      </Stack>
    </Box>
  );
}
