import { useParams } from 'react-router-dom';
import { useMemo } from 'react';
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

export default function TeacherLessons() {
  const { teacherId } = useParams<{ teacherId: string }>();
  const lessonsQuery = useLessonsQuery(teacherId ? { teacher_id: teacherId } : undefined);

  const lessons = useMemo(() => lessonsQuery.data ?? [], [lessonsQuery.data]);

  return (
    <Box p={2}>
      <Typography variant="h5" mb={2}>Clases del profesor #{teacherId}</Typography>
      <TableContainer component={Paper}>
        <Table size="small">
          <TableHead>
            <TableRow>
              <TableCell>ID</TableCell>
              <TableCell>Estudiante</TableCell>
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
                  <TableCell>{lesson.student_id ?? '—'}</TableCell>
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
                  No se encontraron clases para este profesor.
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </TableContainer>
    </Box>
  );
}
