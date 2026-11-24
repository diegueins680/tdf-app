import AlarmAddIcon from '@mui/icons-material/AlarmAdd';
import EventIcon from '@mui/icons-material/Event';
import { Button, Chip, Stack } from '@mui/material';
import { buildGoogleCalendarUrl, downloadFollowUpIcs, type FollowUpCalendarEntry } from '../utils/followUpCalendar';

interface FollowUpCalendarActionsProps {
  entry: FollowUpCalendarEntry;
  // Shown when the dossier summary does not already show the next follow-up.
  dateLabel?: string;
  actionLabel: string;
  fileName: string;
}

// Reminder chip plus Google Calendar and .ics actions for one scheduled follow-up.
export default function FollowUpCalendarActions({ entry, dateLabel, actionLabel, fileName }: FollowUpCalendarActionsProps) {
  return (
    <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
      {dateLabel && (
        <Chip
          size="small"
          icon={<AlarmAddIcon />}
          color={entry.startAt.getTime() < Date.now() ? 'default' : 'warning'}
          variant="outlined"
          label={`Próximo: ${dateLabel}`}
        />
      )}
      <Button
        size="small"
        variant="text"
        startIcon={<EventIcon />}
        href={buildGoogleCalendarUrl(entry)}
        target="_blank"
        rel="noopener noreferrer"
        aria-label={`Añadir recordatorio de ${actionLabel} a Google Calendar`}
      >
        Google Calendar
      </Button>
      <Button
        size="small"
        variant="text"
        onClick={() => downloadFollowUpIcs(entry, fileName)}
        aria-label={`Descargar recordatorio de ${actionLabel} para Apple Calendar u Outlook`}
      >
        Apple / Outlook (.ics)
      </Button>
    </Stack>
  );
}
