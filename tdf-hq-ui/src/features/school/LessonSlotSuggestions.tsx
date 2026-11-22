// SCHOOL-SLOT-SUGGESTION-001: asks the server for the best feasible slots for one
// new lesson. A suggestion is advice: nothing is reserved until the lesson is saved,
// and saving can still answer that the slot was taken.
import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Alert, Box, Button, LinearProgress, Stack, Typography } from '@mui/material';
import AutoAwesomeIcon from '@mui/icons-material/AutoAwesome';
import type { LessonSlotSuggestion } from '../../api/trials';
import { Trials } from '../../api/trials';
import { formatDateForUser } from '../../utils/formatters';
import { describeTeacherGap } from './slotSuggestionHelpers';

export interface LessonSlotSuggestionsProps {
  /** From the session feature flags; when false nothing renders and nothing is requested. */
  enabled: boolean;
  teacherId: number | null;
  studentId: number | null;
  subjectId: number | null;
  roomId: number | null;
  durationMinutes: number;
  onSelect: (suggestion: LessonSlotSuggestion) => void;
}

const formatSlot = (iso: string) =>
  formatDateForUser(iso, { dateStyle: 'medium', timeStyle: 'short' });

export function LessonSlotSuggestions({
  enabled,
  teacherId,
  studentId,
  subjectId,
  roomId,
  durationMinutes,
  onSelect,
}: LessonSlotSuggestionsProps) {
  const [asked, setAsked] = useState(false);
  const ready = teacherId !== null && studentId !== null && subjectId !== null;
  const query = useQuery({
    queryKey: ['lesson-slot-suggestions', teacherId, studentId, subjectId, roomId, durationMinutes],
    queryFn: () =>
      Trials.suggestLessonSlots({
        teacherId: teacherId ?? 0,
        studentId: studentId ?? 0,
        subjectId: subjectId ?? 0,
        ...(roomId !== null ? { roomId } : {}),
        durationMinutes,
      }),
    enabled: enabled && asked && ready,
    staleTime: 0,
    gcTime: 0,
    retry: false,
  });

  if (!enabled) return null;

  return (
    <Box data-testid="lesson-slot-suggestions">
      <Stack direction="row" spacing={1} alignItems="center">
        <Button
          size="small"
          variant="outlined"
          startIcon={<AutoAwesomeIcon fontSize="small" />}
          disabled={!ready || query.isFetching}
          onClick={() => {
            if (asked) void query.refetch();
            else setAsked(true);
          }}
        >
          Sugerir horarios
        </Button>
        {!ready && (
          <Typography variant="caption" color="text.secondary">
            Elige alumno, materia y profesor.
          </Typography>
        )}
      </Stack>
      {query.isFetching && <LinearProgress sx={{ mt: 1 }} />}
      {query.isError && (
        <Alert severity="warning" sx={{ mt: 1 }}>
          No se pudieron calcular sugerencias. Puedes elegir el horario manualmente.
        </Alert>
      )}
      {query.data && !query.isFetching && query.data.suggestions.length === 0 && (
        <Alert severity="info" sx={{ mt: 1 }}>
          No hay horarios disponibles en los próximos días: revisa la disponibilidad del profesor
          y que el alumno tenga un paquete vigente.
        </Alert>
      )}
      {query.data && !query.isFetching && query.data.suggestions.length > 0 && (
        <Stack spacing={1} sx={{ mt: 1 }}>
          <Typography variant="caption" color="text.secondary">
            Sugerencias, no reservas: el horario se confirma al guardar la clase.
          </Typography>
          {query.data.suggestions.map((suggestion) => (
            <Stack
              key={`${suggestion.startAt}-${suggestion.roomId}`}
              direction="row"
              spacing={1}
              alignItems="center"
              justifyContent="space-between"
            >
              <Box>
                <Typography variant="body2">
                  {formatSlot(suggestion.startAt)} · {suggestion.roomName}
                </Typography>
                <Typography variant="caption" color="text.secondary">
                  {describeTeacherGap(suggestion.teacherGapMinutes)}
                </Typography>
              </Box>
              <Button size="small" onClick={() => onSelect(suggestion)}>
                Usar
              </Button>
            </Stack>
          ))}
        </Stack>
      )}
    </Box>
  );
}

export default LessonSlotSuggestions;
