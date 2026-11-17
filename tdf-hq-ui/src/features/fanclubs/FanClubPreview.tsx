import ArtistProfileLink from '../../components/ArtistProfileLink';
import ArtistProfileImage from '../../components/ArtistProfileImage';
import MarkdownText from '../../components/MarkdownText';
import { Card, CardActionArea, CardContent, Grid, Stack, Typography } from '@mui/material';
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
                  sx={{
                    display: 'flex',
                    flexDirection: 'column',
                    height: '100%',
                    transition: 'transform 140ms ease, box-shadow 140ms ease',
                    '&:hover': { transform: 'translateY(-2px)', boxShadow: 3 },
                  }}
                >
                  <ArtistProfileLink partyId={club.fcArtistId} sx={{ display: 'block' }}>
                    <ArtistProfileImage artistName={club.fcName.replace(/^Club de Fans de /i, '')}
                      imageUrl={club.fcArtistImageUrl} alt={club.fcName} width={640} height={160}
                      sx={{ width: '100%' }} />
                  </ArtistProfileLink>
                  <CardActionArea component={RouterLink} to={`/fans/clubs/${club.fcArtistId}`} sx={{ flex: 1, alignItems: 'stretch' }}>
                    <CardContent>
                      <Stack spacing={1}>
                        <Typography variant="h6" fontWeight={700}>
                          {club.fcName}
                        </Typography>
                        <Typography variant="body2" color="text.secondary">
                          {club.fcFollowerCount} seguidores
                        </Typography>
                        {club.fcDescription && (
                          <MarkdownText disableLinks inline variant="body2" color="text.secondary">
                            {club.fcDescription.length > 100
                              ? `${club.fcDescription.slice(0, 100)}…`
                              : club.fcDescription}
                          </MarkdownText>
                        )}
                      </Stack>
                    </CardContent>
                  </CardActionArea>
                </Card>
              </Grid>
            ))}
          </Grid>
        )}
      />
    </Stack>
  );
}
