import { Card, Chip, Stack, Typography } from '@mui/material';
import FavoriteIcon from '@mui/icons-material/Favorite';
import type { FanFollowDTO } from '../../api/types';

export function FollowedArtists({ follows }: { follows: FanFollowDTO[] }) {
  if (follows.length === 0) return null;

  return (
    <Card sx={{ p: 3 }}>
      <Typography variant="h6" gutterBottom>
        Artistas que sigues
      </Typography>
      <Stack direction="row" spacing={2} flexWrap="wrap">
        {follows.map((follow) => (
          <Chip
            key={follow.ffArtistId}
            label={follow.ffArtistName}
            color="primary"
            icon={<FavoriteIcon />}
          />
        ))}
      </Stack>
    </Card>
  );
}
