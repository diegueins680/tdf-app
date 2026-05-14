import { useMemo } from 'react';
import { useParams } from 'react-router-dom';
import { 
  useEnrollmentsQuery, 
  useLessonsQuery, 
  useStudentQuery,
  useLessonPackagesQuery,
  useTeachersQuery
} from '../../api/hq';
import {
  Alert,
  Box,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Grid,
  LinearProgress,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Typography,
} from '@mui/material';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import ScheduleIcon from '@mui/icons-material/Schedule';
import CancelIcon from '@mui/icons-material/Cancel';

function formatDate(isoString?: string | null): string {
  if (!isoString) return 'N/A';
  const date = new Date(isoString);
  return date.toLocaleDateString('es-EC', { year: 'numeric', month: 'short', day: 'numeric' });
}

function formatDateTime(isoString?: string | null): string {
  if (!isoString) return 'N/A';
  const date = new Date(isoString);
  return date.toLocaleDateString('es-EC', { 
    year: 'numeric', 
    month: 'short', 
    day: 'numeric',
    hour: '2-digit',
    minute: '2-digit'
  });
}

function formatRelativeTime(isoString?: string | null): string {
  if (!isoString) return '';
  const date = new Date(isoString);
  const now = new Date();
  const diffMs = now.getTime() - date.getTime();
  const diffDays = Math.floor(diffMs / (1000 * 60 * 60 * 24));
  
  if (diffDays === 0) return 'Hoy';
  if (diffDays === 1) return 'Ayer';
  if (diffDays === -1) return 'Mañana';
  if (diffDays > 0) return `Hace ${diffDays} días`;
  return `En ${Math.abs(diffDays)} días`;
}

function getStatusColor(status: string): 'success' | 'warning' | 'error' | 'default' {
  switch (status) {
    case 'active':
      return 'success';
    case 'completed':
      return 'default';
    case 'cancelled':
      return 'error';
    default:
      return 'warning';
  }
}

function getLessonStatusColor(status: string): 'success' | 'info' | 'error' | 'default' {
  switch (status) {
    case 'completed':
      return 'success';
    case 'scheduled':
      return 'info';
    case 'cancelled':
      return 'error';
    default:
      return 'default';
  }
}

function getLessonStatusIcon(status: string) {
  switch (status) {
    case 'completed':
      return <CheckCircleIcon fontSize="small" />;
    case 'scheduled':
      return <ScheduleIcon fontSize="small" />;
    case 'cancelled':
      return <CancelIcon fontSize="small" />;
    default:
      return null;
  }
}

export default function StudentDashboard() {
  const { studentId } = useParams<{ studentId: string }>();
  
  if (!studentId) {
    return (
      <Box p={3}>
        <Alert severity="error">ID de estudiante no proporcionado</Alert>
      </Box>
    );
  }
  
  const studentQuery = useStudentQuery(studentId);
  const enrollmentsQuery = useEnrollmentsQuery();
  const packagesQuery = useLessonPackagesQuery();
  const teachersQuery = useTeachersQuery();
  const lessonsQuery = useLessonsQuery({ student_id: studentId });

  const student = studentQuery.data;
  const enrollments = useMemo(
    () => (enrollmentsQuery.data ?? []).filter(e => e.student_id === studentId),
    [enrollmentsQuery.data, studentId]
  );
  const lessons = lessonsQuery.data ?? [];
  const packages = packagesQuery.data ?? [];
  const teachers = teachersQuery.data ?? [];

  const isLoading = studentQuery.isLoading || enrollmentsQuery.isLoading || lessonsQuery.isLoading || packagesQuery.isLoading || teachersQuery.isLoading;
  const isError = studentQuery.isError || enrollmentsQuery.isError || lessonsQuery.isError || packagesQuery.isError || teachersQuery.isError;
  const error = studentQuery.error || enrollmentsQuery.error || lessonsQuery.error || packagesQuery.error || teachersQuery.error;

  // Create a map of package_id to package for easy lookup
  const packageMap = useMemo(
    () => new Map(packages.map(pkg => [pkg.id, pkg])),
    [packages]
  );
  
  // Create a map of teacher_id to teacher for easy lookup
  const teacherMap = useMemo(
    () => new Map(teachers.map(teacher => [teacher.id, teacher])),
    [teachers]
  );

  // Calculate stats
  const activeEnrollments = enrollments.filter(e => e.status === 'active');
  const totalLessonsRemaining = activeEnrollments.reduce((sum, e) => sum + e.lessons_remaining, 0);
  const completedLessons = lessons.filter(l => l.status === 'completed').length;
  const scheduledLessons = lessons.filter(l => l.status === 'scheduled').length;

  if (isLoading) {
    return (
      <Box sx={{ display: 'flex', justifyContent: 'center', alignItems: 'center', minHeight: 400 }}>
        <CircularProgress />
      </Box>
    );
  }

  if (isError) {
    return (
      <Box p={3}>
        <Alert severity="error">
          Error cargando datos: {(error as Error).message}
        </Alert>
      </Box>
    );
  }

  if (!student) {
    return (
      <Box p={3}>
        <Alert severity="warning">Estudiante no encontrado</Alert>
      </Box>
    );
  }

  return (
    <Box p={3}>
      {/* Header */}
      <Stack direction="row" alignItems="center" justifyContent="space-between" mb={3}>
        <Box>
          <Typography variant="h4" gutterBottom>
            Panel del Estudiante
          </Typography>
          <Typography variant="h6" color="text.secondary">
            {student.name}
          </Typography>
          {student.email && (
            <Typography variant="body2" color="text.secondary">
              {student.email}
            </Typography>
          )}
        </Box>
      </Stack>

      {/* Stats Cards */}
      <Grid container spacing={3} mb={4}>
        <Grid item xs={12} sm={6} md={3}>
          <Card>
            <CardContent>
              <Typography color="text.secondary" gutterBottom variant="body2">
                Clases Disponibles
              </Typography>
              <Typography variant="h4" color="primary">
                {totalLessonsRemaining}
              </Typography>
              <Typography variant="caption" color="text.secondary">
                En paquetes activos
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={12} sm={6} md={3}>
          <Card>
            <CardContent>
              <Typography color="text.secondary" gutterBottom variant="body2">
                Paquetes Activos
              </Typography>
              <Typography variant="h4" color="success.main">
                {activeEnrollments.length}
              </Typography>
              <Typography variant="caption" color="text.secondary">
                De {enrollments.length} totales
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={12} sm={6} md={3}>
          <Card>
            <CardContent>
              <Typography color="text.secondary" gutterBottom variant="body2">
                Clases Completadas
              </Typography>
              <Typography variant="h4" color="info.main">
                {completedLessons}
              </Typography>
              <Typography variant="caption" color="text.secondary">
                Total histórico
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={12} sm={6} md={3}>
          <Card>
            <CardContent>
              <Typography color="text.secondary" gutterBottom variant="body2">
                Clases Programadas
              </Typography>
              <Typography variant="h4" color="warning.main">
                {scheduledLessons}
              </Typography>
              <Typography variant="caption" color="text.secondary">
                Próximas sesiones
              </Typography>
            </CardContent>
          </Card>
        </Grid>
      </Grid>

      {/* Enrollments/Packages */}
      <Paper sx={{ mb: 4 }}>
        <Box p={2}>
          <Typography variant="h6" gutterBottom>
            Mis Paquetes
          </Typography>
          {enrollments.length === 0 ? (
            <Alert severity="info">
              No tienes paquetes de clases aún. Contacta a recepción para adquirir un paquete.
            </Alert>
          ) : (
            <TableContainer>
              <Table size="small">
                <TableHead>
                  <TableRow>
                    <TableCell>Paquete</TableCell>
                    <TableCell>Fecha de Compra</TableCell>
                    <TableCell align="center">Clases Restantes</TableCell>
                    <TableCell align="center">Progreso</TableCell>
                    <TableCell>Estado</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {enrollments.map((enrollment) => {
                    // Get the package details to calculate progress
                    const pkg = packageMap.get(enrollment.package_id);
                    const totalLessons = pkg?.total_lessons ?? 0;
                    const usedLessons = totalLessons - enrollment.lessons_remaining;
                    const usagePercent = totalLessons > 0 
                      ? Math.round((usedLessons / totalLessons) * 100) 
                      : 0;
                    
                    return (
                      <TableRow key={enrollment.id}>
                        <TableCell>
                          <Typography variant="body2" fontWeight="medium">
                            {pkg?.name ?? `Paquete #${enrollment.package_id.substring(0, 8)}`}
                          </Typography>
                          {pkg?.description && (
                            <Typography variant="caption" color="text.secondary" display="block">
                              {pkg.description}
                            </Typography>
                          )}
                          {enrollment.notes && (
                            <Typography variant="caption" color="text.secondary" display="block">
                              {enrollment.notes}
                            </Typography>
                          )}
                        </TableCell>
                        <TableCell>
                          {formatDate(enrollment.purchase_date)}
                        </TableCell>
                        <TableCell align="center">
                          <Typography variant="h6" color="primary">
                            {enrollment.lessons_remaining}
                          </Typography>
                          {totalLessons > 0 && (
                            <Typography variant="caption" color="text.secondary">
                              de {totalLessons}
                            </Typography>
                          )}
                        </TableCell>
                        <TableCell>
                          <Box sx={{ width: '100%', minWidth: 100 }}>
                            <LinearProgress
                              variant="determinate"
                              value={usagePercent}
                              sx={{ height: 8, borderRadius: 1 }}
                            />
                            <Typography variant="caption" color="text.secondary">
                              {usagePercent}% usado
                            </Typography>
                          </Box>
                        </TableCell>
                        <TableCell>
                          <Chip
                            label={enrollment.status}
                            size="small"
                            color={getStatusColor(enrollment.status)}
                          />
                        </TableCell>
                      </TableRow>
                    );
                  })}
                </TableBody>
              </Table>
            </TableContainer>
          )}
        </Box>
      </Paper>

      {/* Recent Lessons */}
      <Paper>
        <Box p={2}>
          <Typography variant="h6" gutterBottom>
            Historial de Clases
          </Typography>
          {lessons.length === 0 ? (
            <Alert severity="info">
              No tienes clases registradas todavía.
            </Alert>
          ) : (
            <TableContainer>
              <Table size="small">
                <TableHead>
                  <TableRow>
                    <TableCell>Fecha y Hora</TableCell>
                    <TableCell>Profesor</TableCell>
                    <TableCell>Ubicación</TableCell>
                    <TableCell>Estado</TableCell>
                    <TableCell>Notas</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {lessons
                    .sort((a, b) => new Date(b.start_at).getTime() - new Date(a.start_at).getTime())
                    .slice(0, 10)
                    .map((lesson) => {
                      const teacher = teacherMap.get(lesson.teacher_id);
                      return (
                        <TableRow key={lesson.id}>
                          <TableCell>
                            <Stack direction="row" spacing={1} alignItems="center">
                              {getLessonStatusIcon(lesson.status)}
                              <Box>
                                <Typography variant="body2">
                                  {formatDateTime(lesson.start_at)}
                                </Typography>
                                <Typography variant="caption" color="text.secondary">
                                  {formatRelativeTime(lesson.start_at)}
                                </Typography>
                              </Box>
                            </Stack>
                          </TableCell>
                          <TableCell>
                            <Typography variant="body2">
                              {teacher?.name ?? `Profesor #${lesson.teacher_id.substring(0, 8)}`}
                            </Typography>
                          </TableCell>
                          <TableCell>
                            <Typography variant="body2" color="text.secondary">
                              {lesson.location || 'Por definir'}
                            </Typography>
                          </TableCell>
                          <TableCell>
                            <Chip
                              label={lesson.status}
                              size="small"
                              color={getLessonStatusColor(lesson.status)}
                            />
                          </TableCell>
                          <TableCell>
                            <Typography variant="caption" color="text.secondary">
                              {lesson.notes || '-'}
                            </Typography>
                          </TableCell>
                        </TableRow>
                      );
                    })}
                </TableBody>
              </Table>
            </TableContainer>
          )}
        </Box>
      </Paper>
    </Box>
  );
}
