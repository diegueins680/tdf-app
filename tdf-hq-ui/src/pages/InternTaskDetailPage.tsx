import ConfirmDialog from '../components/ConfirmDialog';
import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import ContentCopyOutlinedIcon from '@mui/icons-material/ContentCopyOutlined';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import EditOutlinedIcon from '@mui/icons-material/EditOutlined';
import RefreshIcon from '@mui/icons-material/Refresh';
import SaveOutlinedIcon from '@mui/icons-material/SaveOutlined';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Divider,
  FormControl,
  InputLabel,
  LinearProgress,
  Link,
  MenuItem,
  Select,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useMemo, useState, type FormEvent } from 'react';
import { useTranslation } from 'react-i18next';
import { Link as RouterLink, useNavigate, useParams } from 'react-router-dom';
import { Internships } from '../api/internships';
import { InternAudit } from '../api/internAudit';
import type { InternProjectDTO, InternTaskDTO, InternTaskUpdate } from '../api/types';
import PageShell, { EmptyState } from '../components/PageShell';
import { useSession } from '../session/SessionContext';
import { hasInternshipsAdminAccess } from '../utils/accessControl';
import { parseDateForDisplay } from '../utils/dateOnly';
import { formatDateForUser, formatDateTimeForUser } from '../utils/formatters';

const TASK_STATUS_OPTIONS = [
  { value: 'todo', label: 'Pendiente' },
  { value: 'doing', label: 'En progreso' },
  { value: 'blocked', label: 'Bloqueada' },
  { value: 'done', label: 'Lista' },
] as const;

const TASK_STATUS_LABELS: Record<string, string> = Object.fromEntries(
  TASK_STATUS_OPTIONS.map((option) => [option.value, option.label]),
);

interface TaskEditForm {
  projectId: string;
  title: string;
  description: string;
  status: string;
  progress: string;
  assignedTo: string;
  dueAt: string;
}

interface Feedback {
  severity: 'success' | 'error';
  message: string;
}

const buildTaskEditForm = (task: InternTaskDTO): TaskEditForm => ({
  projectId: task.itProjectId,
  title: task.itTitle,
  description: task.itDescription ?? '',
  status: task.itStatus,
  progress: String(task.itProgress),
  assignedTo: task.itAssignedTo == null ? '' : String(task.itAssignedTo),
  dueAt: task.itDueAt?.slice(0, 10) ?? '',
});

const formatDate = (value?: string | null) => {
  if (!value) return 'Sin fecha';
  const date = parseDateForDisplay(value);
  return date ? formatDateForUser(date) : value;
};

const formatDateTime = (value?: string | null) => {
  if (!value) return '—';
  return formatDateTimeForUser(value);
};

const errorMessage = (error: unknown, fallback: string) =>
  error instanceof Error && error.message.trim() !== '' ? error.message : fallback;

export default function InternTaskDetailPage() {
  const { t } = useTranslation();
  const { session } = useSession();
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const { taskId: rawTaskId = '' } = useParams();
  const taskId = rawTaskId.trim();
  const isAdmin = useMemo(
    () => hasInternshipsAdminAccess(session?.roles, session?.modules),
    [session?.modules, session?.roles],
  );
  const [editing, setEditing] = useState(false);
  const [taskForm, setTaskForm] = useState<TaskEditForm | null>(null);
  const [feedback, setFeedback] = useState<Feedback | null>(null);
  const [deleteConfirmOpen, setDeleteConfirmOpen] = useState(false);

  const tasksQuery = useQuery({
    queryKey: ['internships', 'tasks'],
    queryFn: Internships.listTasks,
    enabled: taskId !== '',
  });
  const task = tasksQuery.data?.find((candidate) => candidate.itId === taskId);
  const internsQuery = useQuery({
    queryKey: ['internships', 'interns'],
    queryFn: Internships.listInterns,
    enabled: isAdmin && editing,
  });
  const projectsQuery = useQuery({
    queryKey: ['internships', 'projects'],
    queryFn: Internships.listProjects,
    enabled: isAdmin && editing,
  });
  const auditPlansQuery = useQuery({
    queryKey: ['intern-audit', 'plans'],
    queryFn: InternAudit.listPlans,
    enabled: Boolean(task),
  });
  const auditPlan = auditPlansQuery.data?.find((candidate) => candidate.iapTaskId === taskId);

  useEffect(() => {
    if (!task || editing) return;
    setTaskForm(buildTaskEditForm(task));
  }, [editing, task]);

  const projectOptions = useMemo(() => {
    const projects = [...(projectsQuery.data ?? [])];
    if (task && !projects.some((project) => project.ipId === task.itProjectId)) {
      projects.unshift({
        ipId: task.itProjectId,
        ipTitle: task.itProjectName,
        ipStatus: 'active',
        ipCreatedAt: task.itCreatedAt,
        ipUpdatedAt: task.itUpdatedAt,
      } satisfies InternProjectDTO);
    }
    return projects;
  }, [projectsQuery.data, task]);

  const updateTaskMutation = useMutation({
    mutationFn: (payload: InternTaskUpdate) => Internships.updateTask(taskId, payload),
    onSuccess: (updatedTask) => {
      queryClient.setQueryData<InternTaskDTO[]>(['internships', 'tasks'], (current) =>
        current?.map((candidate) => candidate.itId === updatedTask.itId ? updatedTask : candidate),
      );
      setTaskForm(buildTaskEditForm(updatedTask));
      setEditing(false);
      setFeedback({ severity: 'success', message: 'La tarea se actualizó correctamente.' });
    },
    onError: (error) => {
      setFeedback({ severity: 'error', message: errorMessage(error, 'No se pudo actualizar la tarea.') });
    },
  });

  const deleteTaskMutation = useMutation({
    mutationFn: () => Internships.deleteTask(taskId),
    onSuccess: () => {
      queryClient.setQueryData<InternTaskDTO[]>(['internships', 'tasks'], (current) =>
        current?.filter((candidate) => candidate.itId !== taskId),
      );
      navigate('/practicas', { replace: true });
    },
    onError: (error) => {
      setFeedback({ severity: 'error', message: errorMessage(error, 'No se pudo eliminar la tarea.') });
    },
  });

  const beginEditing = () => {
    if (!task) return;
    setTaskForm(buildTaskEditForm(task));
    setFeedback(null);
    setEditing(true);
  };

  const cancelEditing = () => {
    if (task) setTaskForm(buildTaskEditForm(task));
    setFeedback(null);
    setEditing(false);
  };

  const submitTaskUpdate = (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    if (!taskForm) return;

    const progressText = taskForm.progress.trim();
    const progress = Number(progressText);
    if (!auditPlan && (!/^\d+$/.test(progressText) || !Number.isInteger(progress) || progress < 0 || progress > 100)) {
      setFeedback({ severity: 'error', message: 'El avance debe ser un número entero entre 0 y 100.' });
      return;
    }
    if (!TASK_STATUS_OPTIONS.some((option) => option.value === taskForm.status)) {
      setFeedback({ severity: 'error', message: 'Selecciona un estado válido.' });
      return;
    }

    let payload: InternTaskUpdate = {
      ituStatus: taskForm.status,
      ...(!auditPlan ? { ituProgress: progress } : {}),
    };

    if (isAdmin) {
      const title = taskForm.title.trim();
      if (taskForm.projectId.trim() === '') {
        setFeedback({ severity: 'error', message: 'Selecciona el proyecto de la tarea.' });
        return;
      }
      if (title === '') {
        setFeedback({ severity: 'error', message: 'El título de la tarea es obligatorio.' });
        return;
      }
      payload = {
        ...payload,
        ituProjectId: taskForm.projectId,
        ituTitle: title,
        ituDescription: taskForm.description.trim() || null,
        ituAssignedTo: taskForm.assignedTo === '' ? null : Number(taskForm.assignedTo),
        ituDueAt: taskForm.dueAt || null,
      };
    }

    setFeedback(null);
    updateTaskMutation.mutate(payload);
  };

  const copyTaskLink = async () => {
    try {
      await navigator.clipboard.writeText(window.location.href);
      setFeedback({ severity: 'success', message: 'Enlace de la tarea copiado.' });
    } catch (error) {
      setFeedback({ severity: 'error', message: errorMessage(error, 'No se pudo copiar el enlace.') });
    }
  };

  const confirmDeleteTask = () => {
    if (!task || !isAdmin) return;
    setDeleteConfirmOpen(true);
  };

  const handleDeleteConfirm = () => {
    deleteTaskMutation.mutate();
    setDeleteConfirmOpen(false);
  };

  return (
    <PageShell
      title={task?.itTitle ?? 'Detalle de tarea'}
      subtitle={task?.itProjectName}
      loading={tasksQuery.isLoading}
      maxWidth="md"
      actions={(
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
          {task && !editing && (
            <Button
              variant="contained"
              startIcon={<EditOutlinedIcon />}
              onClick={beginEditing}
              sx={{ minHeight: 44 }}
            >
              {isAdmin ? 'Editar tarea' : 'Actualizar avance'}
            </Button>
          )}
          {auditPlan && !editing && (
            <Button
              component={RouterLink}
              to={`/practicas/auditorias/${encodeURIComponent(auditPlan.iapId)}`}
              variant="outlined"
              sx={{ minHeight: 44 }}
            >
              Abrir plan de pruebas
            </Button>
          )}
          {task && !editing && (
            <Button component={RouterLink} to="/feedback/interno" variant="text" sx={{ minHeight: 44 }}>
              Ver reportes
            </Button>
          )}
          <Link
            component={RouterLink}
            to="/practicas"
            underline="hover"
            sx={{ display: 'inline-flex', alignItems: 'center', gap: 0.75, minHeight: 44 }}
          >
            <ArrowBackIcon fontSize="small" aria-hidden="true" />
            Volver a Prácticas
          </Link>
        </Stack>
      )}
    >
      <Stack spacing={2.5}>
        {tasksQuery.error && (
          <Alert severity="error">
            {errorMessage(tasksQuery.error, 'No se pudo cargar la tarea.')}
          </Alert>
        )}

        {feedback && <Alert severity={feedback.severity}>{feedback.message}</Alert>}

        {!tasksQuery.isLoading && !tasksQuery.error && !task && (
          <EmptyState
            title="Tarea no encontrada"
            description="La tarea no existe o no tienes permiso para verla."
            actionLabel="Ver mis tareas"
            actionHref="/practicas"
          />
        )}

        {task && editing && taskForm && (
          <Card variant="outlined">
            <CardContent component="form" onSubmit={submitTaskUpdate}>
              <Stack spacing={2.5}>
                <Box>
                  <Typography variant="h6">
                    {isAdmin ? 'Administrar tarea' : 'Actualizar avance'}
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    {isAdmin
                      ? 'Edita la información, organización y seguimiento de esta tarea.'
                      : 'Registra el estado y porcentaje de avance de tu tarea.'}
                  </Typography>
                </Box>

                {isAdmin && (
                  <>
                    <FormControl fullWidth required>
                      <InputLabel id="task-project-label">Proyecto</InputLabel>
                      <Select
                        labelId="task-project-label"
                        label="Proyecto"
                        value={taskForm.projectId}
                        onChange={(event) => setTaskForm((current) => current && ({
                          ...current,
                          projectId: event.target.value,
                        }))}
                      >
                        {projectOptions.map((project) => (
                          <MenuItem key={project.ipId} value={project.ipId}>{project.ipTitle}</MenuItem>
                        ))}
                      </Select>
                    </FormControl>

                    <TextField
                      label="Título"
                      required
                      fullWidth
                      value={taskForm.title}
                      onChange={(event) => setTaskForm((current) => current && ({
                        ...current,
                        title: event.target.value,
                      }))}
                      inputProps={{ maxLength: 160 }}
                    />

                    <TextField
                      label="Instrucciones"
                      fullWidth
                      multiline
                      minRows={8}
                      value={taskForm.description}
                      onChange={(event) => setTaskForm((current) => current && ({
                        ...current,
                        description: event.target.value,
                      }))}
                      helperText="Incluye el objetivo, pasos, criterios de aceptación y cualquier bloqueo conocido."
                    />
                  </>
                )}

                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
                  <FormControl fullWidth required>
                    <InputLabel id="task-status-label">Estado</InputLabel>
                    <Select
                      labelId="task-status-label"
                      label="Estado"
                      value={taskForm.status}
                      onChange={(event) => setTaskForm((current) => current && ({
                        ...current,
                        status: event.target.value,
                      }))}
                    >
                      {TASK_STATUS_OPTIONS.map((option) => (
                        <MenuItem key={option.value} value={option.value}>{option.label}</MenuItem>
                      ))}
                    </Select>
                  </FormControl>
                  <TextField
                    label="Avance %"
                    type="number"
                    required={!auditPlan}
                    fullWidth
                    value={taskForm.progress}
                    onChange={(event) => setTaskForm((current) => current && ({
                      ...current,
                      progress: event.target.value,
                    }))}
                    inputProps={{ min: 0, max: 100, step: 1 }}
                    disabled={Boolean(auditPlan)}
                    helperText={auditPlan ? 'Se calcula automáticamente con los casos ejecutados.' : undefined}
                  />
                </Stack>

                {isAdmin && (
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
                    <FormControl fullWidth>
                      <InputLabel id="task-assignee-label">Responsable</InputLabel>
                      <Select
                        labelId="task-assignee-label"
                        label="Responsable"
                        value={taskForm.assignedTo}
                        onChange={(event) => setTaskForm((current) => current && ({
                          ...current,
                          assignedTo: event.target.value,
                        }))}
                      >
                        <MenuItem value="">Sin asignar</MenuItem>
                        {internsQuery.data?.map((intern) => (
                          <MenuItem key={intern.isPartyId} value={String(intern.isPartyId)}>
                            {intern.isName}
                          </MenuItem>
                        ))}
                        {task.itAssignedTo != null
                          && !internsQuery.data?.some((intern) => intern.isPartyId === task.itAssignedTo) && (
                          <MenuItem value={String(task.itAssignedTo)}>{task.itAssignedName ?? 'Responsable actual'}</MenuItem>
                        )}
                      </Select>
                    </FormControl>
                    <TextField
                      label="Fecha de entrega"
                      type="date"
                      fullWidth
                      value={taskForm.dueAt}
                      onChange={(event) => setTaskForm((current) => current && ({
                        ...current,
                        dueAt: event.target.value,
                      }))}
                      InputLabelProps={{ shrink: true }}
                    />
                  </Stack>
                )}

                {Boolean(internsQuery.error ?? projectsQuery.error) && (
                  <Alert severity="warning">
                    No se pudieron cargar todos los proyectos o responsables. Puedes cancelar y volver a intentar.
                  </Alert>
                )}

                <Stack direction={{ xs: 'column-reverse', sm: 'row' }} spacing={1} justifyContent="flex-end">
                  <Button onClick={cancelEditing} disabled={updateTaskMutation.isPending}>
                    Cancelar
                  </Button>
                  <Button
                    type="submit"
                    variant="contained"
                    startIcon={<SaveOutlinedIcon />}
                    disabled={updateTaskMutation.isPending}
                  >
                    {updateTaskMutation.isPending ? 'Guardando…' : 'Guardar cambios'}
                  </Button>
                </Stack>
              </Stack>
            </CardContent>
          </Card>
        )}

        {task && !editing && (
          <Card variant="outlined">
            <CardContent>
              <Stack spacing={2}>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                  <Chip label={TASK_STATUS_LABELS[task.itStatus] ?? task.itStatus} color="primary" />
                  <Chip label={`Entrega: ${formatDate(task.itDueAt)}`} variant="outlined" />
                  <Chip label={task.itAssignedName ?? 'Sin asignar'} variant="outlined" />
                </Stack>

                <Box>
                  <Stack direction="row" justifyContent="space-between" spacing={2}>
                    <Typography variant="subtitle2" color="text.secondary">Avance</Typography>
                    <Typography variant="subtitle2">{task.itProgress}%</Typography>
                  </Stack>
                  <LinearProgress
                    variant="determinate"
                    value={Math.min(100, Math.max(0, task.itProgress))}
                    aria-label={`Avance de la tarea: ${task.itProgress}%`}
                    sx={{ mt: 0.75, height: 8, borderRadius: 999 }}
                  />
                </Box>

                <Divider />

                <Box>
                  <Typography variant="subtitle2" color="text.secondary" gutterBottom>
                    {t('internships.taskDetail.instructions')}
                  </Typography>
                  <Typography sx={{ whiteSpace: 'pre-wrap' }}>
                    {task.itDescription ?? 'Esta tarea no tiene instrucciones adicionales.'}
                  </Typography>
                </Box>
              </Stack>
            </CardContent>
          </Card>
        )}

        {task && (
          <Card variant="outlined">
            <CardContent>
              <Stack spacing={1.5}>
                <Typography variant="h6">Información y herramientas</Typography>
                <Divider />
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={3}>
                  <Box sx={{ flex: 1 }}>
                    <Typography variant="caption" color="text.secondary">Proyecto</Typography>
                    <Typography>{task.itProjectName}</Typography>
                  </Box>
                  <Box sx={{ flex: 1 }}>
                    <Typography variant="caption" color="text.secondary">Asignada a</Typography>
                    <Typography>{task.itAssignedName ?? 'Sin asignar'}</Typography>
                  </Box>
                </Stack>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={3}>
                  <Box sx={{ flex: 1 }}>
                    <Typography variant="caption" color="text.secondary">Creada</Typography>
                    <Typography>{formatDateTime(task.itCreatedAt)}</Typography>
                  </Box>
                  <Box sx={{ flex: 1 }}>
                    <Typography variant="caption" color="text.secondary">Actualizada</Typography>
                    <Typography>{formatDateTime(task.itUpdatedAt)}</Typography>
                  </Box>
                </Stack>
                <Box>
                  <Typography variant="caption" color="text.secondary">ID</Typography>
                  <Typography sx={{ fontFamily: 'monospace', overflowWrap: 'anywhere' }}>
                    {task.itId}
                  </Typography>
                </Box>
                <Divider />
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                  <Button
                    variant="outlined"
                    startIcon={<RefreshIcon />}
                    onClick={() => void tasksQuery.refetch()}
                    disabled={tasksQuery.isFetching}
                  >
                    {tasksQuery.isFetching ? 'Actualizando…' : 'Refrescar datos'}
                  </Button>
                  <Button variant="outlined" startIcon={<ContentCopyOutlinedIcon />} onClick={() => void copyTaskLink()}>
                    Copiar enlace
                  </Button>
                  {isAdmin && (
                    <Button
                      color="error"
                      variant="outlined"
                      startIcon={<DeleteOutlineIcon />}
                      onClick={confirmDeleteTask}
                      disabled={deleteTaskMutation.isPending}
                      sx={{ ml: { sm: 'auto' } }}
                    >
                      {deleteTaskMutation.isPending ? 'Eliminando…' : 'Eliminar tarea'}
                    </Button>
                  )}
                </Stack>
              </Stack>
            </CardContent>
          </Card>
        )}
      </Stack>
      <ConfirmDialog
        open={deleteConfirmOpen}
        onClose={() => setDeleteConfirmOpen(false)}
        onConfirm={handleDeleteConfirm}
        title="Eliminar tarea"
        description={`¿Eliminar definitivamente la tarea "${task?.itTitle ?? ''}"? Esta acción no se puede deshacer.`}
        severity="danger"
        confirming={deleteTaskMutation.isPending}
      />
    </PageShell>
  );
}
