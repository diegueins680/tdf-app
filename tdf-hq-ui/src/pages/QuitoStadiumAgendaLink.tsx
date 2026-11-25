import { Link, Typography } from '@mui/material';
import { atahualpaAgendaUrl } from './quitoStadiumSource';

export function QuitoStadiumAgendaLink({ city }: { city: string }) {
  if (city.trim().toLowerCase() !== 'quito') return null;
  return (
    <Typography sx={{ mb: 2 }}>
      <Link href={atahualpaAgendaUrl}>Conciertos del Olímpico Atahualpa en Live Nation</Link>
    </Typography>
  );
}
