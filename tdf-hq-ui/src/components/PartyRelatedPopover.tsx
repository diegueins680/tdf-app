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
              <Button size="small" variant="outlined" onClick={() => go(`/perfil/${party.partyId}`)}>
                Perfil
              </Button>
              <Button size="small" variant="outlined" onClick={() => go(`/estudio/calendario?partyId=${party.partyId}`)}>
                Reservas (cliente)
              </Button>
              <Button
                size="small"
                variant="outlined"
                onClick={() => go(`/estudio/calendario?engineerPartyId=${party.partyId}`)}
              >
                Reservas (ingeniero)
              </Button>
              <Button size="small" variant="outlined" onClick={() => go(`/escuela/clases?studentId=${party.partyId}`)}>
                Clases (estudiante)
              </Button>
              <Button size="small" variant="outlined" onClick={() => go(`/escuela/clases?teacherId=${party.partyId}`)}>
                Clases (profesor)
              </Button>
              <Button size="small" variant="outlined" onClick={() => go(`/label/tracks?ownerId=${party.partyId}`)}>
                Tracks
              </Button>
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
          <Stack spacing={1.5}>
            <Box>
              <Typography variant="overline" color="text.secondary">
                Reservas
              </Typography>
              {bookingsCustomer.length === 0 && bookingsEngineer.length === 0 ? (
                <Typography variant="body2" color="text.secondary">
                  No hay reservas asociadas.
                </Typography>
              ) : (
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems="flex-start">
                  <Box sx={{ flex: 1, minWidth: 0 }}>
                    <Typography variant="body2" fontWeight={700}>
                      Cliente ({bookings.filter((b) => b.prbRole === 'cliente').length})
                    </Typography>
                    <List dense disablePadding>
                      {bookingsCustomer.map((b) => (
                        <ListItemButton
                          key={`customer-${b.prbBookingId}`}
                          onClick={() => go(`/estudio/calendario?bookingId=${b.prbBookingId}`)}
                        >
                          <ListItemText
                            primary={b.prbServiceType ?? b.prbTitle ?? `Booking #${b.prbBookingId}`}
                            secondary={`${fmtDateTime(b.prbStartsAt)} · ${b.prbStatus}`}
                          />
                        </ListItemButton>
                      ))}
                    </List>
                  </Box>
                  <Box sx={{ flex: 1, minWidth: 0 }}>
                    <Typography variant="body2" fontWeight={700}>
                      Ingeniero ({bookings.filter((b) => b.prbRole === 'ingeniero').length})
                    </Typography>
                    <List dense disablePadding>
                      {bookingsEngineer.map((b) => (
                        <ListItemButton
                          key={`engineer-${b.prbBookingId}`}
                          onClick={() => go(`/estudio/calendario?bookingId=${b.prbBookingId}`)}
                        >
                          <ListItemText
                            primary={b.prbServiceType ?? b.prbTitle ?? `Booking #${b.prbBookingId}`}
                            secondary={`${fmtDateTime(b.prbStartsAt)} · ${b.prbStatus}`}
                          />
                        </ListItemButton>
                      ))}
                    </List>
                  </Box>
                </Stack>
              )}
            </Box>

            <Box>
              <Typography variant="overline" color="text.secondary">
                Clases
              </Typography>
              {classesStudent.length === 0 && classesTeacher.length === 0 ? (
                <Typography variant="body2" color="text.secondary">
                  No hay clases asociadas.
                </Typography>
              ) : (
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems="flex-start">
                  <Box sx={{ flex: 1, minWidth: 0 }}>
                    <Typography variant="body2" fontWeight={700}>
                      Estudiante ({classSessions.filter((c) => c.prcRole === 'estudiante').length})
                    </Typography>
                    <List dense disablePadding>
                      {classesStudent.map((c) => (
                        <ListItemButton
                          key={`student-${c.prcClassSessionId}`}
                          onClick={() =>
                            go(
                              `/escuela/clases?studentId=${c.prcStudentId}&classSessionId=${c.prcClassSessionId}&at=${encodeURIComponent(
                                c.prcStartAt,
                              )}`,
                            )
                          }
                        >
                          <ListItemText
                            primary={c.prcSubjectName ?? `Materia #${c.prcSubjectId}`}
                            secondary={`${fmtDateTime(c.prcStartAt)} · ${c.prcStatus}`}
                          />
                        </ListItemButton>
                      ))}
                    </List>
                  </Box>
                  <Box sx={{ flex: 1, minWidth: 0 }}>
                    <Typography variant="body2" fontWeight={700}>
                      Profesor ({classSessions.filter((c) => c.prcRole === 'profesor').length})
                    </Typography>
                    <List dense disablePadding>
                      {classesTeacher.map((c) => (
                        <ListItemButton
                          key={`teacher-${c.prcClassSessionId}`}
                          onClick={() =>
                            go(
                              `/escuela/clases?teacherId=${c.prcTeacherId}&classSessionId=${c.prcClassSessionId}&at=${encodeURIComponent(
                                c.prcStartAt,
                              )}`,
                            )
                          }
                        >
                          <ListItemText
                            primary={`${c.prcSubjectName ?? `Materia #${c.prcSubjectId}`} · ${c.prcStudentName ?? `Alumno #${c.prcStudentId}`}`}
                            secondary={`${fmtDateTime(c.prcStartAt)} · ${c.prcStatus}`}
                          />
                        </ListItemButton>
                      ))}
                    </List>
                  </Box>
                </Stack>
              )}
            </Box>

            <Box>
              <Typography variant="overline" color="text.secondary">
                Tracks
              </Typography>
              {tracksSorted.length === 0 ? (
                <Typography variant="body2" color="text.secondary">
                  No hay tracks asociados.
                </Typography>
              ) : (
                <List dense disablePadding>
                  {tracksSorted.map((t) => (
                    <ListItemButton
                      key={t.prtId}
                      onClick={() =>
                        go(`/label/tracks?ownerId=${relatedQuery.data?.prPartyId ?? ''}&trackId=${encodeURIComponent(t.prtId)}`)
                      }
                    >
                      <ListItemText
                        primary={t.prtTitle}
                        secondary={`${t.prtStatus} · actualizado: ${fmtDateTime(t.prtUpdatedAt)}`}
                      />
                    </ListItemButton>
                  ))}
                </List>
              )}
            </Box>
          </Stack>
        )}
      </Stack>
    </Popover>
  );
}

