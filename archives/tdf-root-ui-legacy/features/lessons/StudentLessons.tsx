import { useMemo } from 'react';
import { useParams } from 'react-router-dom';
import { useLessonsQuery } from '../../api/hq';
import {
  Box,
  Paper,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Typography,
} from '@mui/material';

export default function StudentLessons() {
  const { studentId } = useParams<{ studentId: string }>();
  const lessonsQuery = useLessonsQuery(studentId ? { student_id: studentId } : undefined);
  const lessons = useMemo(() => lessonsQuery.data ?? [], [lessonsQuery.data]);

  return (
    <Box p={2}>
      <Typography variant="h5" mb={2}>Clases del estudiante #{studentId}</Typography>
      <TableContainer component={Paper}>
        <Table size="small">
          <TableHead>
            <TableRow>
              <TableCell>ID</TableCell>
              <TableCell>Profesor</TableCell>
              <TableCell>Inicio</TableCell>
              <TableCell>Fin</TableCell>
              <TableCell align="right">Duración (min)</TableCell>
              <TableCell>Notas</TableCell>
              <TableCell>Estado</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {lessonsQuery.isLoading && (
              <TableRow><TableCell colSpan={7}>Cargando…</TableCell></TableRow>
            )}
            {lessonsQuery.isError && (
              <TableRow>
                <TableCell colSpan={7} sx={{ color: 'error.main' }}>
                  {(lessonsQuery.error as Error).message}
                </TableCell>
              </TableRow>
            )}
            {lessons.map(lesson => {
              const start = lesson.start_at ? new Date(lesson.start_at) : null;
              const end = lesson.end_at ? new Date(lesson.end_at) : null;
              const duration =
                start && end ? Math.max(0, Math.round((end.getTime() - start.getTime()) / 60000)) : null;
              return (
                <TableRow key={lesson.id}>
                  <TableCell>{lesson.id}</TableCell>
                  <TableCell>{lesson.teacher_id ?? '—'}</TableCell>
                  <TableCell>{start ? start.toLocaleString() : '—'}</TableCell>
                  <TableCell>{end ? end.toLocaleString() : '—'}</TableCell>
                  <TableCell align="right">{duration ?? '—'}</TableCell>
                  <TableCell>{lesson.notes ?? '—'}</TableCell>
                  <TableCell sx={{ textTransform: 'capitalize' }}>{lesson.status ?? 'scheduled'}</TableCell>
                </TableRow>
              );
            })}
            {!lessonsQuery.isLoading && !lessonsQuery.isError && lessons.length === 0 && (
              <TableRow>
                <TableCell colSpan={7} sx={{ color: 'text.secondary' }}>
                  No se registraron clases para este estudiante.
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </TableContainer>
    </Box>
  );
}
