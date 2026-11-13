import { Box, Button, Card, Stack, Typography } from '@mui/material';
import ArtistProfileImage from '../../components/ArtistProfileImage';

export interface StarterArtist {
  apArtistId: number;
  apDisplayName: string;
  apCity?: string | null;
  apHeroImageUrl?: string | null;
  apSlug?: string | null;
  apFollowerCount?: number | null;
}

const STARTER_LIMIT = 5;

/** The artists a new fan is most likely to recognise: the most followed first. */
export function pickStarterArtists<T extends StarterArtist>(artists: readonly T[], limit = STARTER_LIMIT): T[] {
  return [...artists]
    .sort((a, b) => (b.apFollowerCount ?? 0) - (a.apFollowerCount ?? 0)
      || a.apDisplayName.localeCompare(b.apDisplayName))
    .slice(0, limit);
}

interface Props {
  artists: readonly StarterArtist[];
  followedIds: ReadonlySet<number>;
  disabled: boolean;
  onToggle: (artistId: number, currentlyFollowing: boolean) => void;
  onSeeAll: () => void;
}

/**
 * The first thing a signed-in fan who follows nobody sees: following an artist
 * is the reason they came, and the full catalogue sits many screens below.
 */
export default function FirstFollowStarter({ artists, followedIds, disabled, onToggle, onSeeAll }: Props) {
  if (artists.length === 0) return null;
  const followedHere = artists.some((artist) => followedIds.has(artist.apArtistId));
  return (
    <Card component="section" aria-labelledby="first-follow-starter-title" sx={{ p: 2 }}>
      <Stack spacing={1.5}>
        <Box>
          <Typography id="first-follow-starter-title" component="h2" variant="h6">
            {followedHere ? 'Listo: ya sigues a tu primer artista' : 'Empieza siguiendo a un artista'}
          </Typography>
          <Typography variant="body2" color="text.secondary">
            Te avisaremos de sus lanzamientos y próximas fechas.
          </Typography>
        </Box>
        <Stack component="ul" spacing={1} sx={{ listStyle: 'none', m: 0, p: 0 }}>
          {artists.map((artist) => {
            const following = followedIds.has(artist.apArtistId);
            return (
              <Stack
                component="li"
                key={artist.apArtistId}
                direction="row"
                alignItems="center"
                justifyContent="space-between"
                spacing={1.5}
              >
                <Stack direction="row" spacing={1.5} alignItems="center" sx={{ minWidth: 0 }}>
                  <ArtistProfileImage
                    artistName={artist.apDisplayName}
                    imageUrl={artist.apHeroImageUrl}
                    slug={artist.apSlug}
                    width={40}
                    height={40}
                    sx={{ borderRadius: '50%', flexShrink: 0 }}
                  />
                  <Box sx={{ minWidth: 0 }}>
                    <Typography variant="subtitle1" fontWeight={700} noWrap>{artist.apDisplayName}</Typography>
                    {artist.apCity && (
                      <Typography variant="caption" color="text.secondary" noWrap component="p">{artist.apCity}</Typography>
                    )}
                  </Box>
                </Stack>
                <Button
                  size="small"
                  variant={following ? 'outlined' : 'contained'}
                  color={following ? 'inherit' : 'secondary'}
                  aria-pressed={following}
                  aria-label={`${following ? 'Siguiendo a' : 'Seguir a'} ${artist.apDisplayName}`}
                  disabled={disabled}
                  onClick={() => onToggle(artist.apArtistId, following)}
                  sx={{ flexShrink: 0 }}
                >
                  {following ? 'Siguiendo' : 'Seguir'}
                </Button>
              </Stack>
            );
          })}
        </Stack>
        <Box>
          <Button size="small" onClick={onSeeAll}>Ver todos los artistas</Button>
        </Box>
      </Stack>
    </Card>
  );
}
