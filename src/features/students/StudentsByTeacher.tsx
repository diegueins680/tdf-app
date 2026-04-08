import { useMemo } from 'react';
import { useParams } from 'react-router-dom';
import { useLessonsQuery, useStudentsQuery } from '../../api/hq';
import {
  Box,
  List,
  ListItem,
  ListItemText,
  Paper,
  Typography,
} from '@mui/material';

type TeacherStudent = { id: string; name: string; email?: string };

export default function StudentsByTeacher() {
  const { teacherId } = useParams<{ teacherId: string }>();
  const lessonsQuery = useLessonsQuery(teacherId ? { teacher_id: teacherId } : undefined);
  const studentsQuery = useStudentsQuery();

  const students = useMemo(() => {
    if (!teacherId) return [];
    const lessons = lessonsQuery.data ?? [];
    const studentIds = Array.from(new Set(lessons.map(lesson => lesson.student_id).filter(Boolean))) as string[];
    if (studentIds.length === 0) {
      return [] as TeacherStudent[];
    }
    const studentDirectory = new Map(
      (studentsQuery.data ?? []).map(student => [student.id, student]),
    );
    return studentIds.map<TeacherStudent>((id) => {
      const found = studentDirectory.get(id);
      return {
        id,
        name: found?.name ?? `Estudiante #${id}`,
        email: found?.email ?? undefined,
      };
    });
  }, [teacherId, lessonsQuery.data, studentsQuery.data]);

  const isLoading = lessonsQuery.isLoading || studentsQuery.isLoading;
  const isError = lessonsQuery.isError || studentsQuery.isError;
  const error = (lessonsQuery.error ?? studentsQuery.error) as Error | undefined;

  return (
    <Box p={2}>
      <Typography variant="h5" mb={2}>Estudiantes asignados al profesor #{teacherId}</Typography>
      <Paper>
        {isLoading && (
          <Typography sx={{ p: 2 }}>Cargando…</Typography>
        )}
        {isError && (
          <Typography sx={{ p: 2 }} color="error">
            {error?.message ?? 'No se pudo cargar la información.'}
          </Typography>
        )}
        {!isLoading && !isError && students.length === 0 && (
          <Typography sx={{ p: 2 }} color="text.secondary">
            No hay estudiantes registrados para este profesor.
          </Typography>
        )}
        {students.length > 0 && (
          <List dense>
            {students.map((student: TeacherStudent) => (
              <ListItem key={student.id} divider>
                <ListItemText
                  primary={student.name || `Estudiante #${student.id}`}
                  secondary={student.email}
                />
              </ListItem>
            ))}
          </List>
        )}
      </Paper>
    </Box>
  );
}
