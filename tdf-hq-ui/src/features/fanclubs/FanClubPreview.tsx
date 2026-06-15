import { Card, CardContent, CardMedia, Grid, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import type { FanClubDTO } from '../../api/types';
import LazyPaginatedList from '../../components/LazyPaginatedList';

export function FanClubPreview({
  clubs,
  loading,
}: {
  clubs: FanClubDTO[];
  loading: boolean;
}) {
  if (clubs.length === 0) return null;

  return (
    <Stack spacing={2}>
      <Typography variant="h6">Tus clubes</Typography>
      <LazyPaginatedList
        items={clubs}
        loading={loading}
        pagination={{ itemLabel: 'clubes', initialRowsPerPage: 6 }}
        renderItems={(visibleClubs) => (
          <Grid container spacing={2}>
            {visibleClubs.map((club) => (
              <Grid item xs={12} md={4} key={club.fcId}>
                <Card
                  component={RouterLink}
                  to={`/fans/clubs/${club.fcArtistId}`}
                  sx={{
                    textDecoration: 'none',
                    color: 'inherit',
                    display: 'flex',
                    flexDirection: 'column',
                    height: '100%',
                    transition: 'transform 140ms ease, box-shadow 140ms ease',
                    '&:hover': { transform: 'translateY(-2px)', boxShadow: 3 },
                  }}
                >
                  {club.fcArtistImageUrl && (
                    <CardMedia component="img" height={160} image={club.fcArtistImageUrl} alt={club.fcName} />
                  )}
                  <CardContent sx={{ flex: 1 }}>
                    <Stack spacing={1}>
                      <Typography variant="h6" fontWeight={700}>
                        {club.fcName}
                      </Typography>
                      <Typography variant="body2" color="text.secondary">
                        {club.fcFollowerCount} seguidores
                      </Typography>
                      {club.fcDescription && (
                        <Typography variant="body2" color="text.secondary">
                          {club.fcDescription.length > 100
                            ? `${club.fcDescription.slice(0, 100)}…`
                            : club.fcDescription}
                        </Typography>
                      )}
                    </Stack>
                  </CardContent>
                </Card>
              </Grid>
            ))}
          </Grid>
        )}
      />
    </Stack>
  );
}
