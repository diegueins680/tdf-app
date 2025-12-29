import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  FormControl,
  InputLabel,
  LinearProgress,
  MenuItem,
  Select,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  TextField,
  Typography,
} from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Internships } from '../api/internships';
import type {
  InternProfileUpdate,
  InternProjectCreate,
  InternProjectUpdate,
  InternTaskCreate,
  InternTaskUpdate,
  InternTodoCreate,
  InternTodoUpdate,
  InternTimeEntryDTO,
  InternPermissionCreate,
  InternPermissionUpdate,
} from '../api/types';
import { useSession } from '../session/SessionContext';

const TASK_STATUS_OPTIONS = [
  { value: 'todo', label: 'Pendiente' },
  { value: 'doing', label: 'En progreso' },
  { value: 'blocked', label: 'Bloqueada' },
  { value: 'done', label: 'Lista' },
];

const PROJECT_STATUS_OPTIONS = [
  { value: 'active', label: 'Activo' },
  { value: 'paused', label: 'En pausa' },
  { value: 'completed', label: 'Completado' },
];

const PERMISSION_STATUS_LABELS: Record<string, string> = {
  pending: 'Pendiente',
  approved: 'Aprobado',
  rejected: 'Rechazado',
  cancelled: 'Cancelado',
};

const PLAYBOOK_OVERVIEW = [
  {
    title: 'Plan 8–12 semanas',
    detail: 'Objetivos claros por semana, con entregables y métricas visibles.',
  },
  {
    title: 'Rotaciones guiadas',
    detail: 'Estudio, A&R, marketing, data y operaciones para descubrir afinidad.',
  },
  {
    title: 'Proyecto estrella',
    detail: 'Un proyecto con impacto real, documentado y presentado al cierre.',
  },
];

const PLAYBOOK_RITUALS = [
  {
    title: 'Daily 10 min',
    detail: 'Prioridades del día y bloqueos.',
  },
  {
    title: 'Revisión semanal',
    detail: 'Avances, aprendizajes y siguiente meta.',
  },
  {
    title: 'Retro mensual',
    detail: 'Feedback accionable y ajuste de plan.',
  },
  {
    title: 'Demo day',
    detail: 'Presentación final con métricas e impacto.',
  },
];

const PLAYBOOK_PROJECT_DESCRIPTION =
  'Plan base de prácticas con rotaciones, proyecto estrella y rituales de seguimiento.';

const PLAYBOOK_TASK_TEMPLATES = [
  {
    title: 'Kickoff + objetivos SMART',
    description: 'Definir expectativas, entregables y métricas de éxito.',
  },
  {
    title: 'Mapa de competencias + áreas de interés',
    description: 'Priorizar habilidades y áreas donde aportar mayor valor.',
  },
  {
    title: 'Onboarding de herramientas y procesos',
    description: 'Accesos, workflow interno y normas de calidad.',
  },
  {
    title: 'Rotación 1: Estudio / producción',
    description: 'Apoyo en sesiones, documentación y preparación técnica.',
  },
  {
    title: 'Rotación 2: A&R y scouting',
    description: 'Investigación de artistas, briefs y oportunidades.',
  },
  {
    title: 'Rotación 3: Marketing y growth',
    description: 'Ejecución de contenido, campañas y análisis de resultados.',
  },
  {
    title: 'Rotación 4: Operaciones y data',
    description: 'Procesos internos, dashboards y optimizaciones.',
  },
  {
    title: 'Proyecto estrella: alcance + plan',
    description: 'Definir problema, alcance, entregables y cronograma.',
  },
  {
    title: 'Proyecto estrella: ejecución',
    description: 'Implementación y coordinación con stakeholders.',
  },
  {
    title: 'Proyecto estrella: entrega + métricas',
    description: 'Resultados, métricas y recomendaciones.',
  },
  {
    title: 'Portafolio interno + documentación',
    description: 'Registrar aprendizajes, procesos y evidencias.',
  },
  {
    title: 'Demo day + retro final',
    description: 'Presentación final y retroalimentación 360°.',
  },
];

const PERSONAL_CHECKLIST_TODOS = [
  '[Prácticas] Daily de 10 min: prioridades y bloqueos',
  '[Prácticas] Revisión semanal con mentor',
  '[Prácticas] Actualizar portafolio interno',
  '[Prácticas] Registrar aprendizaje clave',
  '[Prácticas] Preparar demo day',
];

const formatDate = (value?: string | null) => {
  if (!value) return '—';
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return value;
  return new Intl.DateTimeFormat('es-EC', { dateStyle: 'medium' }).format(date);
};

const formatDateTime = (value?: string | null) => {
  if (!value) return '—';
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return value;
  return new Intl.DateTimeFormat('es-EC', { dateStyle: 'medium', timeStyle: 'short' }).format(date);
};

const minutesToHours = (minutes: number) => (minutes / 60).toFixed(2);
const normalizeOptional = (value?: string | null) => {
  const trimmed = value?.trim() ?? '';
  return trimmed === '' ? null : trimmed;
};

const normalizeOptionalInt = (value?: string | null) => {
  const trimmed = value?.trim() ?? '';
  if (trimmed === '') return null;
  if (!/^-?\d+$/.test(trimmed)) return undefined;
  const parsed = Number.parseInt(trimmed, 10);
  return Number.isNaN(parsed) ? undefined : parsed;
};

export default function InternshipsPage() {
  const { session } = useSession();
  const qc = useQueryClient();
  const roles = session?.roles ?? [];
  const lowerRoles = useMemo(() => roles.map((r) => r.toLowerCase()), [roles]);
  const isAdmin = useMemo(
    () => lowerRoles.some((role) => ['admin', 'manager', 'studiomanager'].includes(role)),
    [lowerRoles],
  );
  const isIntern = useMemo(
    () => lowerRoles.some((role) => role.includes('intern') || role.includes('pasante') || role.includes('practicante')),
    [lowerRoles],
  );
  const canAccess = isAdmin || isIntern;

  const [selectedPartyId, setSelectedPartyId] = useState<number | null>(null);
  const [progressDraft, setProgressDraft] = useState<Record<string, number>>({});
  const [signupFeedback, setSignupFeedback] = useState<string | null>(null);
  const [playbookAssigneeId, setPlaybookAssigneeId] = useState<number | ''>('');
  const [playbookFeedback, setPlaybookFeedback] = useState<{ type: 'success' | 'error'; message: string } | null>(null);
  const [checklistFeedback, setChecklistFeedback] = useState<{ type: 'success' | 'error'; message: string } | null>(null);

  const [projectForm, setProjectForm] = useState<InternProjectCreate>({
    ipcTitle: '',
    ipcDescription: '',
    ipcStatus: 'active',
    ipcStartAt: '',
    ipcDueAt: '',
  });
  const [profileForm, setProfileForm] = useState({
    startAt: '',
    endAt: '',
    requiredHours: '',
    skills: '',
    areas: '',
  });
  const [taskForm, setTaskForm] = useState<InternTaskCreate>({
    itcProjectId: '',
    itcTitle: '',
    itcDescription: '',
    itcAssignedTo: null,
    itcDueAt: '',
  });
  const [todoForm, setTodoForm] = useState<InternTodoCreate>({ itdcText: '' });
  const [permissionForm, setPermissionForm] = useState<InternPermissionCreate>({
    ipcCategory: '',
    ipcReason: '',
    ipcStartAt: '',
    ipcEndAt: '',
  });

  const internsQuery = useQuery({
    queryKey: ['internships', 'interns'],
    queryFn: Internships.listInterns,
    enabled: isAdmin,
  });
  const profileQuery = useQuery({
    queryKey: ['internships', 'profile'],
    queryFn: Internships.getProfile,
    enabled: isIntern,
  });
  const projectsQuery = useQuery({
    queryKey: ['internships', 'projects'],
    queryFn: Internships.listProjects,
    enabled: canAccess,
  });
  const tasksQuery = useQuery({
    queryKey: ['internships', 'tasks'],
    queryFn: Internships.listTasks,
    enabled: canAccess,
  });
  const todosQuery = useQuery({
    queryKey: ['internships', 'todos'],
    queryFn: Internships.listTodos,
    enabled: canAccess,
  });
  const entriesQuery = useQuery({
    queryKey: ['internships', 'time-entries', selectedPartyId ?? 'self'],
    queryFn: () => Internships.listTimeEntries(isAdmin ? selectedPartyId : null),
    enabled: canAccess,
  });
  const permissionsQuery = useQuery({
    queryKey: ['internships', 'permissions'],
    queryFn: Internships.listPermissions,
    enabled: canAccess,
  });

  useEffect(() => {
    if (!profileQuery.data) return;
    setProfileForm({
      startAt: profileQuery.data.ipStartAt ?? '',
      endAt: profileQuery.data.ipEndAt ?? '',
      requiredHours: profileQuery.data.ipRequiredHours?.toString() ?? '',
      skills: profileQuery.data.ipSkills ?? '',
      areas: profileQuery.data.ipAreas ?? '',
    });
  }, [profileQuery.data]);

  useEffect(() => {
    if (!tasksQuery.data) return;
    const next: Record<string, number> = {};
    tasksQuery.data.forEach((task) => {
      next[task.itId] = task.itProgress;
    });
    setProgressDraft(next);
  }, [tasksQuery.data]);

  const createProjectMutation = useMutation({
    mutationFn: (payload: InternProjectCreate) => Internships.createProject(payload),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['internships', 'projects'] });
      setProjectForm({ ipcTitle: '', ipcDescription: '', ipcStatus: 'active', ipcStartAt: '', ipcDueAt: '' });
    },
  });
  const updateProfileMutation = useMutation({
    mutationFn: (payload: InternProfileUpdate) => Internships.updateProfile(payload),
    onSuccess: () => void qc.invalidateQueries({ queryKey: ['internships', 'profile'] }),
  });
  const updateProjectMutation = useMutation({
    mutationFn: ({ projectId, payload }: { projectId: string; payload: InternProjectUpdate }) =>
      Internships.updateProject(projectId, payload),
    onSuccess: () => void qc.invalidateQueries({ queryKey: ['internships', 'projects'] }),
  });
  const createTaskMutation = useMutation({
    mutationFn: (payload: InternTaskCreate) => Internships.createTask(payload),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['internships', 'tasks'] });
      setTaskForm({ itcProjectId: '', itcTitle: '', itcDescription: '', itcAssignedTo: null, itcDueAt: '' });
    },
  });
  const updateTaskMutation = useMutation({
    mutationFn: ({ taskId, payload }: { taskId: string; payload: InternTaskUpdate }) =>
      Internships.updateTask(taskId, payload),
    onSuccess: () => void qc.invalidateQueries({ queryKey: ['internships', 'tasks'] }),
  });
  const createTodoMutation = useMutation({
    mutationFn: (payload: InternTodoCreate) => Internships.createTodo(payload),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['internships', 'todos'] });
      setTodoForm({ itdcText: '' });
    },
  });
  const updateTodoMutation = useMutation({
    mutationFn: ({ todoId, payload }: { todoId: string; payload: InternTodoUpdate }) =>
      Internships.updateTodo(todoId, payload),
    onSuccess: () => void qc.invalidateQueries({ queryKey: ['internships', 'todos'] }),
  });
  const deleteTodoMutation = useMutation({
    mutationFn: (todoId: string) => Internships.deleteTodo(todoId),
    onSuccess: () => void qc.invalidateQueries({ queryKey: ['internships', 'todos'] }),
  });
  const clockInMutation = useMutation({
    mutationFn: () => Internships.clockIn({}),
    onSuccess: () => void qc.invalidateQueries({ queryKey: ['internships', 'time-entries'] }),
  });
  const clockOutMutation = useMutation({
    mutationFn: () => Internships.clockOut({}),
    onSuccess: () => void qc.invalidateQueries({ queryKey: ['internships', 'time-entries'] }),
  });
  const createPermissionMutation = useMutation({
    mutationFn: (payload: InternPermissionCreate) => Internships.createPermission(payload),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['internships', 'permissions'] });
      setPermissionForm({ ipcCategory: '', ipcReason: '', ipcStartAt: '', ipcEndAt: '' });
    },
  });
  const updatePermissionMutation = useMutation({
    mutationFn: ({ permissionId, payload }: { permissionId: string; payload: InternPermissionUpdate }) =>
      Internships.updatePermission(permissionId, payload),
    onSuccess: () => void qc.invalidateQueries({ queryKey: ['internships', 'permissions'] }),
  });
  const seedPlaybookMutation = useMutation({
    mutationFn: async ({ projectTitle, assigneeId }: { projectTitle: string; assigneeId: number | null }) => {
      const project = await Internships.createProject({
        ipcTitle: projectTitle,
        ipcDescription: PLAYBOOK_PROJECT_DESCRIPTION,
        ipcStatus: 'active',
        ipcStartAt: null,
        ipcDueAt: null,
      });
      for (const task of PLAYBOOK_TASK_TEMPLATES) {
        await Internships.createTask({
          itcProjectId: project.ipId,
          itcTitle: task.title,
          itcDescription: task.description,
          itcAssignedTo: assigneeId,
          itcDueAt: null,
        });
      }
      return project;
    },
    onMutate: () => setPlaybookFeedback(null),
    onSuccess: () => {
      setPlaybookFeedback({ type: 'success', message: 'Plan base creado. Ya puedes asignar avances en tareas.' });
      void qc.invalidateQueries({ queryKey: ['internships', 'projects'] });
      void qc.invalidateQueries({ queryKey: ['internships', 'tasks'] });
    },
    onError: () => setPlaybookFeedback({ type: 'error', message: 'No se pudo crear el plan base. Intenta de nuevo.' }),
  });
  const seedChecklistMutation = useMutation({
    mutationFn: async () => {
      for (const text of PERSONAL_CHECKLIST_TODOS) {
        await Internships.createTodo({ itdcText: text });
      }
      return true;
    },
    onMutate: () => setChecklistFeedback(null),
    onSuccess: () => {
      setChecklistFeedback({ type: 'success', message: 'Checklist personal creado en tus to-dos.' });
      void qc.invalidateQueries({ queryKey: ['internships', 'todos'] });
    },
    onError: () => setChecklistFeedback({ type: 'error', message: 'No se pudo crear el checklist.' }),
  });

  const projects = projectsQuery.data ?? [];
  const tasks = tasksQuery.data ?? [];
  const todos = todosQuery.data ?? [];
  const entries = entriesQuery.data ?? [];
  const permissions = permissionsQuery.data ?? [];
  const interns = internsQuery.data ?? [];
  const sessionPartyId = session?.partyId ?? null;
  const isSelfView = !isAdmin || (selectedPartyId != null && selectedPartyId === sessionPartyId);
  const selectedAssignee = interns.find((intern) => intern.isPartyId === playbookAssigneeId) ?? null;
  const playbookProjectTitle = selectedAssignee
    ? `Plan de prácticas - ${selectedAssignee.isName} (#${selectedAssignee.isPartyId})`
    : 'Plan de prácticas';
  const playbookProjectExists = projects.some(
    (project) => project.ipTitle.trim().toLowerCase() === playbookProjectTitle.toLowerCase(),
  );
  const hasChecklist = todos.some((todo) => todo.itdText.startsWith('[Prácticas]'));

  const openEntry = isSelfView ? (entries.find((entry) => !entry.iteClockOut) ?? null) : null;
  const totalMinutes = entries.reduce((sum, entry) => {
    if (entry.iteDurationMinutes != null) return sum + entry.iteDurationMinutes;
    if (entry.iteClockOut) {
      const start = new Date(entry.iteClockIn).getTime();
      const end = new Date(entry.iteClockOut).getTime();
      if (!Number.isNaN(start) && !Number.isNaN(end)) {
        return sum + Math.max(0, Math.floor((end - start) / 60000));
      }
    }
    return sum;
  }, 0);

  const signupPath = '/login?signup=1&roles=Intern&redirect=/practicas';
  const signupUrl = typeof window !== 'undefined' ? `${window.location.origin}${signupPath}` : signupPath;

  const handleCopySignup = async () => {
    if (!signupUrl) return;
    try {
      await navigator.clipboard.writeText(signupUrl);
      setSignupFeedback('Link copiado.');
    } catch {
      setSignupFeedback('No se pudo copiar. Usa el link manual.');
    }
  };

  if (!canAccess) {
    return (
      <Stack spacing={2}>
        <Typography variant="h5" fontWeight={700}>Prácticas pre-profesionales</Typography>
        <Alert severity="warning">Tu usuario no tiene acceso a este módulo.</Alert>
      </Stack>
    );
  }

  return (
    <Stack spacing={3}>
      <Stack spacing={0.5}>
        <Typography variant="overline" color="text.secondary">
          Talento / Prácticas
        </Typography>
        <Typography variant="h4" fontWeight={800}>
          Prácticas pre-profesionales
        </Typography>
        <Typography color="text.secondary">
          Gestiona proyectos, tareas, to-dos, control horario y permisos de pasantes/practicantes.
        </Typography>
      </Stack>

      {isAdmin && (
        <Card>
          <CardContent>
            <Stack spacing={1.5}>
              <Typography variant="h6" fontWeight={700}>Link de registro para pasantes</Typography>
              <TextField
                value={signupUrl}
                InputProps={{ readOnly: true }}
                fullWidth
              />
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems="center">
                <Button variant="contained" onClick={() => void handleCopySignup()}>
                  Copiar link
                </Button>
                {signupFeedback && <Typography variant="body2" color="text.secondary">{signupFeedback}</Typography>}
              </Stack>
            </Stack>
          </CardContent>
        </Card>
      )}

      <Card>
        <CardContent>
          <Stack spacing={2}>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} justifyContent="space-between">
              <Typography variant="h6" fontWeight={700}>Playbook de prácticas</Typography>
              <Chip label="Guía 8–12 semanas" variant="outlined" />
            </Stack>
            <Typography color="text.secondary">
              Usa este playbook como guía estándar para onboarding, rotaciones, proyecto estrella y seguimiento.
            </Typography>
            <Stack spacing={1}>
              {PLAYBOOK_OVERVIEW.map((item) => (
                <Box
                  key={item.title}
                  sx={{
                    borderRadius: 2,
                    border: '1px solid',
                    borderColor: 'divider',
                    px: 2,
                    py: 1.5,
                  }}
                >
                  <Typography fontWeight={600}>{item.title}</Typography>
                  <Typography variant="body2" color="text.secondary">
                    {item.detail}
                  </Typography>
                </Box>
              ))}
            </Stack>
            <Stack spacing={1}>
              <Typography variant="subtitle2" fontWeight={700}>Formato de seguimiento</Typography>
              <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
                {PLAYBOOK_RITUALS.map((ritual) => (
                  <Box
                    key={ritual.title}
                    sx={{
                      flex: 1,
                      borderRadius: 2,
                      border: '1px solid',
                      borderColor: 'divider',
                      px: 2,
                      py: 1.5,
                    }}
                  >
                    <Typography fontWeight={600}>{ritual.title}</Typography>
                    <Typography variant="body2" color="text.secondary">
                      {ritual.detail}
                    </Typography>
                  </Box>
                ))}
              </Stack>
            </Stack>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={1} alignItems={{ md: 'center' }}>
              {isAdmin && (
                <>
                  <FormControl size="small" sx={{ minWidth: 240 }}>
                    <InputLabel id="playbook-assignee-label">Asignar plan a</InputLabel>
                    <Select
                      labelId="playbook-assignee-label"
                      label="Asignar plan a"
                      value={playbookAssigneeId}
                      onChange={(event) =>
                        setPlaybookAssigneeId(event.target.value === '' ? '' : Number(event.target.value))
                      }
                    >
                      <MenuItem value="">Sin asignar</MenuItem>
                      {interns.map((intern) => (
                        <MenuItem key={intern.isPartyId} value={intern.isPartyId}>{intern.isName}</MenuItem>
                      ))}
                    </Select>
                  </FormControl>
                  <Button
                    variant="contained"
                    onClick={() =>
                      void seedPlaybookMutation.mutateAsync({
                        projectTitle: playbookProjectTitle,
                        assigneeId: playbookAssigneeId === '' ? null : playbookAssigneeId,
                      })
                    }
                    disabled={playbookProjectExists || seedPlaybookMutation.isPending}
                  >
                    {seedPlaybookMutation.isPending ? 'Creando…' : 'Generar plan base'}
                  </Button>
                </>
              )}
              {isIntern && (
                <Button
                  variant="outlined"
                  onClick={() => void seedChecklistMutation.mutateAsync()}
                  disabled={hasChecklist || seedChecklistMutation.isPending}
                >
                  {seedChecklistMutation.isPending ? 'Creando…' : 'Crear checklist personal'}
                </Button>
              )}
              {playbookProjectExists && isAdmin && (
                <Typography variant="body2" color="text.secondary">
                  Ya existe un plan con el mismo nombre.
                </Typography>
              )}
            </Stack>
            {playbookFeedback && <Alert severity={playbookFeedback.type}>{playbookFeedback.message}</Alert>}
            {checklistFeedback && <Alert severity={checklistFeedback.type}>{checklistFeedback.message}</Alert>}
          </Stack>
        </CardContent>
      </Card>

      {isIntern && (
        <Card>
          <CardContent>
            <Stack spacing={2}>
              <Typography variant="h6" fontWeight={700}>Perfil de prácticas</Typography>
              {profileQuery.isError && (
                <Alert severity="warning">No se pudo cargar tu perfil de prácticas.</Alert>
              )}
              <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                <TextField
                  label="Inicio de prácticas"
                  type="date"
                  value={profileForm.startAt}
                  onChange={(event) => setProfileForm((prev) => ({ ...prev, startAt: event.target.value }))}
                  InputLabelProps={{ shrink: true }}
                  fullWidth
                />
                <TextField
                  label="Fin de prácticas"
                  type="date"
                  value={profileForm.endAt}
                  onChange={(event) => setProfileForm((prev) => ({ ...prev, endAt: event.target.value }))}
                  InputLabelProps={{ shrink: true }}
                  fullWidth
                />
                <TextField
                  label="Horas requeridas"
                  type="number"
                  value={profileForm.requiredHours}
                  onChange={(event) => setProfileForm((prev) => ({ ...prev, requiredHours: event.target.value }))}
                  inputProps={{ min: 0, step: 1 }}
                  fullWidth
                />
              </Stack>
              <TextField
                label="Habilidades / skills"
                value={profileForm.skills}
                onChange={(event) => setProfileForm((prev) => ({ ...prev, skills: event.target.value }))}
                multiline
                minRows={2}
                fullWidth
              />
              <TextField
                label="Áreas de práctica de interés"
                value={profileForm.areas}
                onChange={(event) => setProfileForm((prev) => ({ ...prev, areas: event.target.value }))}
                multiline
                minRows={2}
                fullWidth
              />
              <Stack direction="row" spacing={1} alignItems="center">
                <Button
                  variant="contained"
                  onClick={() => {
                    const requiredHours = normalizeOptionalInt(profileForm.requiredHours);
                    const payload: InternProfileUpdate = {
                      ipuStartAt: normalizeOptional(profileForm.startAt),
                      ipuEndAt: normalizeOptional(profileForm.endAt),
                      ...(requiredHours !== undefined ? { ipuRequiredHours: requiredHours } : {}),
                      ipuSkills: normalizeOptional(profileForm.skills),
                      ipuAreas: normalizeOptional(profileForm.areas),
                    };
                    void updateProfileMutation.mutateAsync(payload);
                  }}
                  disabled={updateProfileMutation.isPending}
                >
                  {updateProfileMutation.isPending ? 'Guardando…' : 'Guardar perfil'}
                </Button>
                {updateProfileMutation.isSuccess && (
                  <Typography variant="body2" color="text.secondary">Perfil actualizado.</Typography>
                )}
              </Stack>
            </Stack>
          </CardContent>
        </Card>
      )}

      {(projectsQuery.isLoading || tasksQuery.isLoading || entriesQuery.isLoading || profileQuery.isLoading) && <LinearProgress />}

      <Card>
        <CardContent>
          <Stack spacing={2}>
            <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" alignItems={{ sm: 'center' }} spacing={1}>
              <Typography variant="h6" fontWeight={700}>Jornada y registro de horas</Typography>
              <Stack direction="row" spacing={1} alignItems="center">
                <Chip label={openEntry ? 'En jornada' : 'Fuera de jornada'} color={openEntry ? 'success' : 'default'} />
                <Chip label={`${minutesToHours(totalMinutes)} h registradas`} variant="outlined" />
              </Stack>
            </Stack>

            {isAdmin && (
              <FormControl size="small" sx={{ maxWidth: 320 }}>
                <InputLabel id="intern-filter-label">Filtrar por pasante</InputLabel>
                <Select
                  labelId="intern-filter-label"
                  label="Filtrar por pasante"
                  value={selectedPartyId ?? ''}
                  onChange={(event) => setSelectedPartyId(event.target.value === '' ? null : Number(event.target.value))}
                >
                  <MenuItem value="">Todos</MenuItem>
                  {interns.map((intern) => (
                    <MenuItem key={intern.isPartyId} value={intern.isPartyId}>{intern.isName}</MenuItem>
                  ))}
                </Select>
              </FormControl>
            )}

            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
              <Button
                variant="contained"
                onClick={() => void clockInMutation.mutateAsync()}
                disabled={!isSelfView || !!openEntry || clockInMutation.isPending}
              >
                {clockInMutation.isPending ? 'Marcando…' : 'Clock-in'}
              </Button>
              <Button
                variant="outlined"
                onClick={() => void clockOutMutation.mutateAsync()}
                disabled={!isSelfView || !openEntry || clockOutMutation.isPending}
              >
                {clockOutMutation.isPending ? 'Cerrando…' : 'Clock-out'}
              </Button>
            </Stack>
            {!isSelfView && (
              <Typography variant="caption" color="text.secondary">
                Vista administrativa: el clock-in/out solo aplica a tu propia cuenta.
              </Typography>
            )}

            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Pasante</TableCell>
                  <TableCell>Entrada</TableCell>
                  <TableCell>Salida</TableCell>
                  <TableCell>Horas</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {entries.length === 0 && (
                  <TableRow>
                    <TableCell colSpan={4}>
                      <Typography color="text.secondary">Sin registros todavía.</Typography>
                    </TableCell>
                  </TableRow>
                )}
                {entries.map((entry) => (
                  <TableRow key={entry.iteId}>
                    <TableCell>{entry.itePartyName}</TableCell>
                    <TableCell>{formatDateTime(entry.iteClockIn)}</TableCell>
                    <TableCell>{formatDateTime(entry.iteClockOut)}</TableCell>
                    <TableCell>
                      {entry.iteDurationMinutes != null
                        ? `${minutesToHours(entry.iteDurationMinutes)} h`
                        : entry.iteClockOut
                          ? '—'
                          : 'En curso'}
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </Stack>
        </CardContent>
      </Card>

      <Card>
        <CardContent>
          <Stack spacing={2}>
            <Stack direction="row" justifyContent="space-between" alignItems="center">
              <Typography variant="h6" fontWeight={700}>Proyectos</Typography>
              <Chip label={`${projects.length} activos`} variant="outlined" />
            </Stack>

            {isAdmin && (
              <Stack spacing={1}>
                <Typography fontWeight={600}>Crear proyecto</Typography>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
                  <TextField
                    label="Nombre"
                    value={projectForm.ipcTitle}
                    onChange={(event) => setProjectForm((prev) => ({ ...prev, ipcTitle: event.target.value }))}
                    fullWidth
                  />
                  <FormControl sx={{ minWidth: 180 }}>
                    <InputLabel id="project-status-label">Estado</InputLabel>
                    <Select
                      labelId="project-status-label"
                      label="Estado"
                      value={projectForm.ipcStatus ?? 'active'}
                      onChange={(event) => setProjectForm((prev) => ({ ...prev, ipcStatus: event.target.value }))}
                    >
                      {PROJECT_STATUS_OPTIONS.map((opt) => (
                        <MenuItem key={opt.value} value={opt.value}>{opt.label}</MenuItem>
                      ))}
                    </Select>
                  </FormControl>
                  <TextField
                    label="Inicio"
                    type="date"
                    value={projectForm.ipcStartAt ?? ''}
                    onChange={(event) => setProjectForm((prev) => ({ ...prev, ipcStartAt: event.target.value }))}
                    InputLabelProps={{ shrink: true }}
                  />
                  <TextField
                    label="Entrega"
                    type="date"
                    value={projectForm.ipcDueAt ?? ''}
                    onChange={(event) => setProjectForm((prev) => ({ ...prev, ipcDueAt: event.target.value }))}
                    InputLabelProps={{ shrink: true }}
                  />
                </Stack>
                <TextField
                  label="Descripción"
                  value={projectForm.ipcDescription ?? ''}
                  onChange={(event) => setProjectForm((prev) => ({ ...prev, ipcDescription: event.target.value }))}
                  fullWidth
                  multiline
                  minRows={2}
                />
                <Button
                  variant="contained"
                  onClick={() => {
                    const payload = {
                      ...projectForm,
                      ipcDescription: normalizeOptional(projectForm.ipcDescription),
                      ipcStartAt: normalizeOptional(projectForm.ipcStartAt),
                      ipcDueAt: normalizeOptional(projectForm.ipcDueAt),
                    };
                    void createProjectMutation.mutateAsync(payload);
                  }}
                  disabled={!projectForm.ipcTitle.trim() || createProjectMutation.isPending}
                >
                  {createProjectMutation.isPending ? 'Creando…' : 'Crear proyecto'}
                </Button>
              </Stack>
            )}

            {projects.length === 0 && (
              <Typography color="text.secondary">No hay proyectos todavía.</Typography>
            )}

            <Stack spacing={1}>
              {projects.map((project) => (
                <Box
                  key={project.ipId}
                  sx={{
                    border: '1px solid',
                    borderColor: 'divider',
                    borderRadius: 2,
                    p: 1.5,
                    display: 'flex',
                    flexDirection: 'column',
                    gap: 0.5,
                  }}
                >
                  <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1}>
                    <Box>
                      <Typography fontWeight={700}>{project.ipTitle}</Typography>
                      {project.ipDescription && (
                        <Typography variant="body2" color="text.secondary">{project.ipDescription}</Typography>
                      )}
                      <Typography variant="caption" color="text.secondary">
                        Inicio {formatDate(project.ipStartAt)} · Entrega {formatDate(project.ipDueAt)}
                      </Typography>
                    </Box>
                    <FormControl size="small" sx={{ minWidth: 160 }}>
                      <InputLabel id={`project-${project.ipId}`}>Estado</InputLabel>
                      <Select
                        labelId={`project-${project.ipId}`}
                        label="Estado"
                        value={project.ipStatus}
                        onChange={(event) =>
                          void updateProjectMutation.mutateAsync({
                            projectId: project.ipId,
                            payload: { ipuStatus: event.target.value },
                          })
                        }
                        disabled={!isAdmin}
                      >
                        {PROJECT_STATUS_OPTIONS.map((opt) => (
                          <MenuItem key={opt.value} value={opt.value}>{opt.label}</MenuItem>
                        ))}
                      </Select>
                    </FormControl>
                  </Stack>
                </Box>
              ))}
            </Stack>
          </Stack>
        </CardContent>
      </Card>

      <Card>
        <CardContent>
          <Stack spacing={2}>
            <Stack direction="row" justifyContent="space-between" alignItems="center">
              <Typography variant="h6" fontWeight={700}>Tareas</Typography>
              <Chip label={`${tasks.length} tareas`} variant="outlined" />
            </Stack>

            {isAdmin && (
              <Stack spacing={1}>
                <Typography fontWeight={600}>Asignar tarea</Typography>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
                  <FormControl fullWidth>
                    <InputLabel id="task-project-label">Proyecto</InputLabel>
                    <Select
                      labelId="task-project-label"
                      label="Proyecto"
                      value={taskForm.itcProjectId}
                      onChange={(event) => setTaskForm((prev) => ({ ...prev, itcProjectId: event.target.value }))}
                    >
                      {projects.map((project) => (
                        <MenuItem key={project.ipId} value={project.ipId}>{project.ipTitle}</MenuItem>
                      ))}
                    </Select>
                  </FormControl>
                  <TextField
                    label="Título"
                    value={taskForm.itcTitle}
                    onChange={(event) => setTaskForm((prev) => ({ ...prev, itcTitle: event.target.value }))}
                    fullWidth
                  />
                  <FormControl sx={{ minWidth: 200 }}>
                    <InputLabel id="task-intern-label">Asignado a</InputLabel>
                    <Select
                      labelId="task-intern-label"
                      label="Asignado a"
                      value={taskForm.itcAssignedTo ?? ''}
                      onChange={(event) => setTaskForm((prev) => ({
                        ...prev,
                        itcAssignedTo: event.target.value === '' ? null : Number(event.target.value),
                      }))}
                    >
                      <MenuItem value="">Sin asignar</MenuItem>
                      {interns.map((intern) => (
                        <MenuItem key={intern.isPartyId} value={intern.isPartyId}>{intern.isName}</MenuItem>
                      ))}
                    </Select>
                  </FormControl>
                  <TextField
                    label="Entrega"
                    type="date"
                    value={taskForm.itcDueAt ?? ''}
                    onChange={(event) => setTaskForm((prev) => ({ ...prev, itcDueAt: event.target.value }))}
                    InputLabelProps={{ shrink: true }}
                  />
                </Stack>
                <TextField
                  label="Descripción"
                  value={taskForm.itcDescription ?? ''}
                  onChange={(event) => setTaskForm((prev) => ({ ...prev, itcDescription: event.target.value }))}
                  fullWidth
                  multiline
                  minRows={2}
                />
                <Button
                  variant="contained"
                  onClick={() => {
                    const payload = {
                      ...taskForm,
                      itcDescription: normalizeOptional(taskForm.itcDescription),
                      itcDueAt: normalizeOptional(taskForm.itcDueAt),
                    };
                    void createTaskMutation.mutateAsync(payload);
                  }}
                  disabled={!taskForm.itcProjectId || !taskForm.itcTitle.trim() || createTaskMutation.isPending}
                >
                  {createTaskMutation.isPending ? 'Creando…' : 'Crear tarea'}
                </Button>
              </Stack>
            )}

            {tasks.length === 0 && (
              <Typography color="text.secondary">No hay tareas todavía.</Typography>
            )}

            <Stack spacing={1.5}>
              {tasks.map((task) => (
                <Box
                  key={task.itId}
                  sx={{
                    border: '1px solid',
                    borderColor: 'divider',
                    borderRadius: 2,
                    p: 1.5,
                  }}
                >
                  <Stack spacing={1}>
                    <Stack direction={{ xs: 'column', md: 'row' }} spacing={1} justifyContent="space-between">
                      <Box>
                        <Typography fontWeight={700}>{task.itTitle}</Typography>
                        <Typography variant="body2" color="text.secondary">
                          {task.itProjectName}
                          {task.itDescription ? ` · ${task.itDescription}` : ''}
                        </Typography>
                        <Typography variant="caption" color="text.secondary">
                          Entrega {formatDate(task.itDueAt)}
                        </Typography>
                      </Box>
                      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
                        <FormControl size="small" sx={{ minWidth: 160 }}>
                          <InputLabel id={`task-status-${task.itId}`}>Estado</InputLabel>
                          <Select
                            labelId={`task-status-${task.itId}`}
                            label="Estado"
                            value={task.itStatus}
                            onChange={(event) =>
                              void updateTaskMutation.mutateAsync({
                                taskId: task.itId,
                                payload: { ituStatus: event.target.value },
                              })
                            }
                          >
                            {TASK_STATUS_OPTIONS.map((opt) => (
                              <MenuItem key={opt.value} value={opt.value}>{opt.label}</MenuItem>
                            ))}
                          </Select>
                        </FormControl>
                        <TextField
                          label="Avance %"
                          type="number"
                          size="small"
                          value={progressDraft[task.itId] ?? task.itProgress}
                          onChange={(event) =>
                            setProgressDraft((prev) => ({
                              ...prev,
                              [task.itId]: Number(event.target.value),
                            }))
                          }
                          onBlur={(event) => {
                            const value = Number(event.target.value);
                            if (Number.isNaN(value)) return;
                            void updateTaskMutation.mutateAsync({ taskId: task.itId, payload: { ituProgress: value } });
                          }}
                          inputProps={{ min: 0, max: 100 }}
                        />
                        {isAdmin && (
                          <FormControl size="small" sx={{ minWidth: 160 }}>
                            <InputLabel id={`task-assignee-${task.itId}`}>Asignado</InputLabel>
                            <Select
                              labelId={`task-assignee-${task.itId}`}
                              label="Asignado"
                              value={task.itAssignedTo ?? ''}
                              onChange={(event) =>
                                void updateTaskMutation.mutateAsync({
                                  taskId: task.itId,
                                  payload: { ituAssignedTo: event.target.value === '' ? null : Number(event.target.value) },
                                })
                              }
                            >
                              <MenuItem value="">Sin asignar</MenuItem>
                              {interns.map((intern) => (
                                <MenuItem key={intern.isPartyId} value={intern.isPartyId}>{intern.isName}</MenuItem>
                              ))}
                            </Select>
                          </FormControl>
                        )}
                        {!isAdmin && (
                          <Chip label={task.itAssignedName ?? 'Sin asignar'} variant="outlined" />
                        )}
                      </Stack>
                    </Stack>
                  </Stack>
                </Box>
              ))}
            </Stack>
          </Stack>
        </CardContent>
      </Card>

      <Card>
        <CardContent>
          <Stack spacing={2}>
            <Stack direction="row" justifyContent="space-between" alignItems="center">
              <Typography variant="h6" fontWeight={700}>To-dos personales</Typography>
              <Chip label={`${todos.length} pendientes`} variant="outlined" />
            </Stack>

            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
              <TextField
                fullWidth
                label="Nuevo to-do"
                value={todoForm.itdcText}
                onChange={(event) => setTodoForm({ itdcText: event.target.value })}
              />
              <Button
                variant="contained"
                onClick={() => void createTodoMutation.mutateAsync(todoForm)}
                disabled={!todoForm.itdcText.trim() || createTodoMutation.isPending}
              >
                Agregar
              </Button>
            </Stack>

            {todos.length === 0 && (
              <Typography color="text.secondary">No hay to-dos aún.</Typography>
            )}

            <Stack spacing={1}>
              {todos.map((todo) => (
                <Stack
                  key={todo.itdId}
                  direction={{ xs: 'column', sm: 'row' }}
                  spacing={1}
                  alignItems={{ sm: 'center' }}
                  sx={{ border: '1px solid', borderColor: 'divider', borderRadius: 2, p: 1 }}
                >
                  <Button
                    size="small"
                    variant={todo.itdDone ? 'contained' : 'outlined'}
                    onClick={() => void updateTodoMutation.mutateAsync({ todoId: todo.itdId, payload: { itduDone: !todo.itdDone } })}
                  >
                    {todo.itdDone ? 'Hecho' : 'Marcar'}
                  </Button>
                  <Typography sx={{ flex: 1, textDecoration: todo.itdDone ? 'line-through' : 'none' }}>
                    {todo.itdText}
                  </Typography>
                  <Button
                    size="small"
                    color="error"
                    onClick={() => void deleteTodoMutation.mutateAsync(todo.itdId)}
                  >
                    Eliminar
                  </Button>
                </Stack>
              ))}
            </Stack>
          </Stack>
        </CardContent>
      </Card>

      <Card>
        <CardContent>
          <Stack spacing={2}>
            <Stack direction="row" justifyContent="space-between" alignItems="center">
              <Typography variant="h6" fontWeight={700}>Permisos</Typography>
              <Chip label={`${permissions.length} solicitudes`} variant="outlined" />
            </Stack>

            <Stack spacing={1}>
              <Typography fontWeight={600}>Solicitar permiso</Typography>
              <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
                <TextField
                  label="Tipo de permiso"
                  value={permissionForm.ipcCategory}
                  onChange={(event) => setPermissionForm((prev) => ({ ...prev, ipcCategory: event.target.value }))}
                  fullWidth
                />
                <TextField
                  label="Inicio"
                  type="date"
                  value={permissionForm.ipcStartAt}
                  onChange={(event) => setPermissionForm((prev) => ({ ...prev, ipcStartAt: event.target.value }))}
                  InputLabelProps={{ shrink: true }}
                />
                <TextField
                  label="Fin"
                  type="date"
                  value={permissionForm.ipcEndAt ?? ''}
                  onChange={(event) => setPermissionForm((prev) => ({ ...prev, ipcEndAt: event.target.value }))}
                  InputLabelProps={{ shrink: true }}
                />
              </Stack>
              <TextField
                label="Motivo"
                value={permissionForm.ipcReason ?? ''}
                onChange={(event) => setPermissionForm((prev) => ({ ...prev, ipcReason: event.target.value }))}
                fullWidth
                multiline
                minRows={2}
              />
              <Button
                variant="contained"
                onClick={() => {
                  const payload = {
                    ...permissionForm,
                    ipcReason: normalizeOptional(permissionForm.ipcReason),
                    ipcEndAt: normalizeOptional(permissionForm.ipcEndAt),
                  };
                  void createPermissionMutation.mutateAsync(payload);
                }}
                disabled={!permissionForm.ipcCategory.trim() || !permissionForm.ipcStartAt || createPermissionMutation.isPending}
              >
                Enviar solicitud
              </Button>
            </Stack>

            {permissions.length === 0 && (
              <Typography color="text.secondary">Sin solicitudes registradas.</Typography>
            )}

            <Stack spacing={1}>
              {permissions.map((perm) => (
                <Box
                  key={perm.iprId}
                  sx={{
                    border: '1px solid',
                    borderColor: 'divider',
                    borderRadius: 2,
                    p: 1.5,
                  }}
                >
                  <Stack spacing={0.5}>
                    <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1}>
                      <Box>
                        <Typography fontWeight={700}>{perm.iprCategory}</Typography>
                        <Typography variant="body2" color="text.secondary">
                          {perm.iprPartyName} · {formatDate(perm.iprStartAt)} → {formatDate(perm.iprEndAt)}
                        </Typography>
                        {perm.iprReason && (
                          <Typography variant="body2" color="text.secondary">{perm.iprReason}</Typography>
                        )}
                      </Box>
                      <Chip
                        label={PERMISSION_STATUS_LABELS[perm.iprStatus] ?? perm.iprStatus}
                        color={perm.iprStatus === 'approved' ? 'success' : perm.iprStatus === 'rejected' ? 'error' : 'default'}
                        variant="outlined"
                      />
                    </Stack>
                    {isAdmin && (
                      <Stack direction="row" spacing={1}>
                        <Button
                          size="small"
                          variant="contained"
                          onClick={() => void updatePermissionMutation.mutateAsync({ permissionId: perm.iprId, payload: { ipuStatus: 'approved' } })}
                        >
                          Aprobar
                        </Button>
                        <Button
                          size="small"
                          variant="outlined"
                          color="error"
                          onClick={() => void updatePermissionMutation.mutateAsync({ permissionId: perm.iprId, payload: { ipuStatus: 'rejected' } })}
                        >
                          Rechazar
                        </Button>
                      </Stack>
                    )}
                  </Stack>
                </Box>
              ))}
            </Stack>
          </Stack>
        </CardContent>
      </Card>
    </Stack>
  );
}
