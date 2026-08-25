import { useEffect, useMemo, useState } from 'react';
import {
  Box,
  Button,
  Checkbox,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControlLabel,
  LinearProgress,
  Paper,
  Snackbar,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import PersonAddAltIcon from '@mui/icons-material/PersonAddAlt';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import { useForm } from 'react-hook-form';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Parties } from '../api/parties';
import type { PartyCreate, PartyDTO } from '../api/types';

const createTeacherSchema = z.object({
  displayName: z.string().min(2, 'Nombre requerido'),
  email: z.string().email('Correo inválido').optional().or(z.literal('')),
  phone: z.string().optional(),
});

type CreateTeacherForm = z.infer<typeof createTeacherSchema>;

function useLocalRoster() {
  const [roster, setRoster] = useState<number[]>(() => {
    if (typeof window === 'undefined') return [];
    const stored = window.localStorage.getItem('tdf-teacher-roster');
    if (!stored) return [];
    try {
      const parsed = JSON.parse(stored) as number[];
      return Array.isArray(parsed) ? parsed : [];
    } catch (_error) {
      return [];
    }
  });

  useEffect(() => {
    if (typeof window === 'undefined') return;
    window.localStorage.setItem('tdf-teacher-roster', JSON.stringify(roster));
  }, [roster]);

  return { roster, setRoster } as const;
}

export default function TeachersPage() {
  const qc = useQueryClient();
  const { roster, setRoster } = useLocalRoster();
  const [search, setSearch] = useState('');
  const [showRosterOnly, setShowRosterOnly] = useState(false);
  const [openCreate, setOpenCreate] = useState(false);
  const [snackbar, setSnackbar] = useState<string | null>(null);

  const partiesQuery = useQuery({ queryKey: ['parties'], queryFn: Parties.list });

  const addRoleMutation = useMutation({
    mutationFn: (partyId: number) => Parties.addRole(partyId, 'Teacher'),
    onSuccess: (_data, partyId) => {
      qc.invalidateQueries({ queryKey: ['parties'] });
      setRoster(prev => (prev.includes(partyId) ? prev : [...prev, partyId]));
      setSnackbar('Profesor agregado al roster');
    },
  });

  const {
    register,
    handleSubmit,
    reset,
    formState: { errors, isSubmitting },
  } = useForm<CreateTeacherForm>({
    resolver: zodResolver(createTeacherSchema),
    defaultValues: { displayName: '', email: '', phone: '' },
  });

  const createTeacher = useMutation({
    mutationFn: (body: PartyCreate) => Parties.create(body),
    onSuccess: (party) => {
      qc.invalidateQueries({ queryKey: ['parties'] });
      setRoster(prev => (prev.includes(party.partyId) ? prev : [...prev, party.partyId]));
      setSnackbar('Profesor creado con rol Teacher');
      reset();
      setOpenCreate(false);
    },
  });

  const parties = partiesQuery.data ?? [];
  const showFirstTeacherSetup = !partiesQuery.isLoading && !partiesQuery.isError && parties.length === 0;
  const singleTeacherContact =
    !partiesQuery.isLoading && !partiesQuery.isError && parties.length === 1
      ? (parties[0] ?? null)
      : null;
  const singleTeacherInRoster = singleTeacherContact ? roster.includes(singleTeacherContact.partyId) : false;

  const filtered = useMemo(() => {
    const term = search.trim().toLowerCase();
    return parties
      .filter(party => {
        const matchesSearch = !term || party.displayName.toLowerCase().includes(term) || (party.primaryEmail ?? '').toLowerCase().includes(term);
        const inRoster = roster.includes(party.partyId);
        return matchesSearch && (!showRosterOnly || inRoster);
      })
      .sort((a, b) => a.displayName.localeCompare(b.displayName, 'es'));
  }, [parties, roster, search, showRosterOnly]);

  const onSubmit = (values: CreateTeacherForm) => {
    const payload: PartyCreate = {
      cDisplayName: values.displayName.trim(),
      cIsOrg: false,
      cPrimaryEmail: values.email?.trim() || null,
      cPrimaryPhone: values.phone?.trim() || null,
      cRoles: ['Teacher'],
    };
    createTeacher.mutate(payload);
  };

  return (
    <Stack spacing={3}>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ xs: 'stretch', sm: 'center' }} justifyContent="space-between">
        <Box>
          <Typography variant="h5">Profesores</Typography>
          <Typography color="text.secondary" variant="body2">
            Administra el roster de profesores y vincula aquellos que imparten clases o trial lessons.
          </Typography>
          {showFirstTeacherSetup ? (
            <Typography color="text.secondary" variant="body2" sx={{ mt: 0.75 }}>
              Empieza con Nuevo profesor. La búsqueda, el filtro de roster y la tabla aparecerán cuando exista el primer contacto.
            </Typography>
          ) : singleTeacherContact ? (
            <Typography color="text.secondary" variant="body2" sx={{ mt: 0.75 }}>
              Revísalo aquí; cuando exista el segundo, volverán la búsqueda, el filtro de roster y la tabla para comparar docentes.
            </Typography>
          ) : null}
        </Box>
        <Stack direction="row" spacing={1} justifyContent="flex-end">
          {!showFirstTeacherSetup && !singleTeacherContact && (
            <>
              <TextField
                value={search}
                onChange={(event) => setSearch(event.target.value)}
                size="small"
                label="Buscar"
                placeholder="Nombre o correo"
              />
              <FormControlLabel
                control={(
                  <Checkbox
                    checked={showRosterOnly}
                    onChange={(_event, checked) => setShowRosterOnly(checked)}
                  />
                )}
                label="Solo roster"
              />
            </>
          )}
          <Button variant="contained" startIcon={<PersonAddAltIcon />} onClick={() => setOpenCreate(true)}>
            Nuevo profesor
          </Button>
        </Stack>
      </Stack>

      {showFirstTeacherSetup ? (
        <Paper
          variant="outlined"
          sx={{
            minHeight: 280,
            px: 3,
            py: 4,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            textAlign: 'center',
          }}
        >
          <Stack spacing={1.5} sx={{ maxWidth: 560 }}>
            <Typography variant="h6">Todavía no hay profesores registrados.</Typography>
            <Typography variant="body2" color="text.secondary">
              Usa Nuevo profesor para crear el primero. Cuando exista, aquí podrás confirmar si ya
              está en el roster y sumar más docentes sin revisar filtros vacíos.
            </Typography>
          </Stack>
        </Paper>
      ) : singleTeacherContact ? (
        <Paper
          variant="outlined"
          sx={{
            minHeight: 280,
            px: 3,
            py: 4,
            display: 'flex',
            alignItems: 'center',
          }}
        >
          <Stack spacing={2} sx={{ maxWidth: 640, width: '100%' }}>
            <Box>
              <Typography variant="h6">Primer contacto docente disponible.</Typography>
              <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                Confirma aquí su información principal y usa una sola acción para sumarlo al roster si hace falta.
              </Typography>
            </Box>
            <Stack
              spacing={0.75}
              sx={{
                border: '1px solid',
                borderColor: 'divider',
                borderRadius: 2,
                px: 2,
                py: 1.5,
              }}
            >
              <Typography variant="subtitle1">{singleTeacherContact.displayName}</Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Contacto:</Box>{' '}
                {[singleTeacherContact.primaryEmail, singleTeacherContact.primaryPhone].filter(Boolean).join(' · ') || '—'}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Notas:</Box> {singleTeacherContact.notes || '—'}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                <Box component="span" sx={{ fontWeight: 600 }}>Roster:</Box>{' '}
                {singleTeacherInRoster ? 'Ya en roster' : 'Pendiente de agregar'}
              </Typography>
            </Stack>
            {singleTeacherInRoster ? (
              <Tooltip title="Este contacto ya está en el roster de profesores">
                <Stack direction="row" spacing={0.5} alignItems="center" sx={{ alignSelf: 'flex-start' }}>
                  <CheckCircleIcon color="success" fontSize="small" />
                  <Typography variant="body2" sx={{ color: 'success.main' }}>
                    Ya en roster
                  </Typography>
                </Stack>
              </Tooltip>
            ) : (
              <Button
                size="small"
                color="primary"
                variant="outlined"
                disabled={addRoleMutation.isPending}
                onClick={() => addRoleMutation.mutate(singleTeacherContact.partyId)}
                startIcon={<PersonAddAltIcon fontSize="small" />}
                sx={{ alignSelf: 'flex-start', textTransform: 'none' }}
              >
                Agregar al roster
              </Button>
            )}
          </Stack>
        </Paper>
      ) : (
        <Paper variant="outlined">
          {partiesQuery.isLoading && <LinearProgress />}
          <TableContainer>
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Nombre</TableCell>
                  <TableCell>Contacto</TableCell>
                  <TableCell>Notas</TableCell>
                  <TableCell align="right">Acciones</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {filtered.map((party) => {
                  const inRoster = roster.includes(party.partyId);
                  const contact = [party.primaryEmail, party.primaryPhone].filter(Boolean).join(' · ');
                  return (
                    <TableRow key={party.partyId} hover>
                      <TableCell sx={{ minWidth: 200 }}>{party.displayName}</TableCell>
                      <TableCell>{contact || '—'}</TableCell>
                      <TableCell>{party.notes || '—'}</TableCell>
                      <TableCell align="right">
                        <Stack direction="row" spacing={1} justifyContent="flex-end" alignItems="center">
                          {inRoster ? (
                            <Tooltip title="Este contacto ya está en el roster de profesores">
                              <Stack direction="row" spacing={0.5} alignItems="center">
                                <CheckCircleIcon color="success" fontSize="small" />
                                <Typography variant="body2" sx={{ color: 'success.main' }}>
                                  Ya en roster
                                </Typography>
                              </Stack>
                            </Tooltip>
                          ) : (
                            <Button
                              size="small"
                              color="primary"
                              disabled={addRoleMutation.isPending}
                              onClick={() => addRoleMutation.mutate(party.partyId)}
                              startIcon={<PersonAddAltIcon fontSize="small" />}
                              sx={{ textTransform: 'none' }}
                            >
                              Agregar al roster
                            </Button>
                          )}
                        </Stack>
                      </TableCell>
                    </TableRow>
                  );
                })}
                {filtered.length === 0 && (
                  <TableRow>
                    <TableCell colSpan={4} align="center" sx={{ py: 4 }}>
                      {showRosterOnly ? 'No hay profesores en el roster que coincidan con la búsqueda.' : 'No hay contactos que coincidan con la búsqueda.'}
                    </TableCell>
                  </TableRow>
                )}
              </TableBody>
            </Table>
          </TableContainer>
        </Paper>
      )}

      <Dialog open={openCreate} onClose={() => setOpenCreate(false)} maxWidth="sm" fullWidth>
        <DialogTitle>Registrar profesor</DialogTitle>
        <DialogContent sx={{ display: 'flex', flexDirection: 'column', gap: 2, pt: 2 }}>
          <TextField
            label="Nombre"
            autoFocus
            {...register('displayName')}
            error={!!errors.displayName}
            helperText={errors.displayName?.message}
          />
          <TextField
            label="Correo"
            type="email"
            {...register('email')}
            error={!!errors.email}
            helperText={errors.email?.message}
          />
          <TextField
            label="Teléfono"
            {...register('phone')}
            error={!!errors.phone}
            helperText={errors.phone?.message}
          />
          <Typography variant="body2" color="text.secondary">
            La persona se creará como contacto y se le asignará automáticamente el rol <strong>Teacher</strong> para habilitar acceso al módulo de clases.
          </Typography>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setOpenCreate(false)} disabled={isSubmitting || createTeacher.isPending}>Cancelar</Button>
          <Button
            variant="contained"
            onClick={handleSubmit(onSubmit)}
            disabled={isSubmitting || createTeacher.isPending}
          >
            {createTeacher.isPending ? 'Guardando…' : 'Guardar'}
          </Button>
        </DialogActions>
      </Dialog>

      <Snackbar
        open={!!snackbar}
        autoHideDuration={4000}
        onClose={() => setSnackbar(null)}
        message={snackbar ?? ''}
      />
    </Stack>
  );
}
