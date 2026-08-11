import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import {
  Alert,
  Box,
  Card,
  CardContent,
  Chip,
  Divider,
  LinearProgress,
  Link,
  Stack,
  Typography,
} from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { Link as RouterLink, useParams } from 'react-router-dom';
import { Internships } from '../api/internships';
import PageShell, { EmptyState } from '../components/PageShell';
import { parseDateForDisplay } from '../utils/dateOnly';
import { formatDateForUser, formatDateTimeForUser } from '../utils/formatters';

const TASK_STATUS_LABELS: Record<string, string> = {
  todo: 'Pendiente',
  doing: 'En progreso',
  blocked: 'Bloqueada',
  done: 'Lista',
};

const formatDate = (value?: string | null) => {
  if (!value) return 'Sin fecha';
  const date = parseDateForDisplay(value);
  return date ? formatDateForUser(date) : value;
};

const formatDateTime = (value?: string | null) => {
  if (!value) return '—';
  return formatDateTimeForUser(value);
};

export default function InternTaskDetailPage() {
  const { t } = useTranslation();
  const { taskId: rawTaskId = '' } = useParams();
  const taskId = rawTaskId.trim();
  const tasksQuery = useQuery({
    queryKey: ['internships', 'tasks'],
    queryFn: Internships.listTasks,
    enabled: taskId !== '',
  });
  const task = tasksQuery.data?.find((candidate) => candidate.itId === taskId);

  return (
    <PageShell
      title={task?.itTitle ?? 'Detalle de tarea'}
      subtitle={task?.itProjectName}
      loading={tasksQuery.isLoading}
      maxWidth="md"
      actions={(
        <Link
          component={RouterLink}
          to="/practicas"
          underline="hover"
          sx={{ display: 'inline-flex', alignItems: 'center', gap: 0.75, minHeight: 44 }}
        >
          <ArrowBackIcon fontSize="small" aria-hidden="true" />
          Volver a Prácticas
        </Link>
      )}
    >
      <Stack spacing={2.5}>
        {tasksQuery.error && (
          <Alert severity="error">
            {tasksQuery.error instanceof Error
              ? tasksQuery.error.message
              : 'No se pudo cargar la tarea.'}
          </Alert>
        )}

        {!tasksQuery.isLoading && !tasksQuery.error && !task && (
          <EmptyState
            title="Tarea no encontrada"
            description="La tarea no existe o no tienes permiso para verla."
            actionLabel="Ver mis tareas"
            actionHref="/practicas"
          />
        )}

        {task && (
          <>
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

            <Card variant="outlined">
              <CardContent>
                <Stack spacing={1.25}>
                  <Typography variant="h6">Información de la tarea</Typography>
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
                </Stack>
              </CardContent>
            </Card>
          </>
        )}
      </Stack>
    </PageShell>
  );
}
