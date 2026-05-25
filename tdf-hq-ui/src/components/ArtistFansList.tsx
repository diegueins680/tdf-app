import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import {
  Avatar,
  Box,
  CircularProgress,
  Pagination,
  Stack,
  Typography,
} from '@mui/material';
import PersonIcon from '@mui/icons-material/Person';
import { Fans } from '../api/fans';

interface ArtistFansListProps {
  artistId: number;
}

export default function ArtistFansList({ artistId }: ArtistFansListProps) {
  const [page, setPage] = useState(1);
  const pageSize = 5;

  const fansQuery = useQuery({
    queryKey: ['artist-fans', artistId, page, pageSize],
    queryFn: () => Fans.getArtistFans(artistId, page, pageSize),
    enabled: Boolean(artistId),
  });

  const fans = fansQuery.data?.items ?? [];
  const totalPages = fansQuery.data ? Math.ceil(fansQuery.data.total / pageSize) : 0;

  const handlePageChange = (_: unknown, value: number) => {
    setPage(value);
  };

  if (fansQuery.isLoading && page === 1) {
    return (
      <Box display="flex" alignItems="center" gap={1.5} py={2}>
        <CircularProgress size={18} />
        <Typography variant="body2" color="text.secondary">
          Cargando fans...
        </Typography>
      </Box>
    );
  }

  if (fansQuery.isError) {
    return (
      <Typography variant="body2" color="error" sx={{ py: 1 }}>
        No se pudieron cargar los fans.
      </Typography>
    );
  }

  if (fans.length === 0) {
    return (
      <Typography variant="body2" color="text.secondary">
        Este artista aún no tiene fans.
      </Typography>
    );
  }

  return (
    <Stack spacing={2}>
      <Stack spacing={1.5}>
        {fans.map((fan) => (
          <Stack
            key={fan.afFanId}
            direction="row"
            spacing={2}
            alignItems="center"
            sx={{
              p: 1.5,
              borderRadius: 2,
              '&:hover': {
                bgcolor: 'action.hover',
              },
            }}
          >
            <Avatar
              src={fan.afAvatarUrl ?? undefined}
              alt={fan.afDisplayName}
              sx={{
                width: 48,
                height: 48,
                bgcolor: 'primary.light',
              }}
            >
              {fan.afDisplayName?.[0]?.toUpperCase() ?? <PersonIcon />}
            </Avatar>
            <Box sx={{ flex: 1, minWidth: 0 }}>
              <Typography fontWeight={700} sx={{ lineHeight: 1.3 }}>
                {fan.afDisplayName}
              </Typography>
              <Typography variant="caption" color="text.secondary">
                Fan desde {new Date(fan.afFollowedAt).toLocaleDateString('es-EC', {
                  year: 'numeric',
                  month: 'short',
                  day: 'numeric',
                })}
              </Typography>
            </Box>
          </Stack>
        ))}
      </Stack>

      {totalPages > 1 && (
        <Box display="flex" justifyContent="center" pt={1}>
          <Pagination
            count={totalPages}
            page={page}
            onChange={handlePageChange}
            color="primary"
            size="small"
            disabled={fansQuery.isLoading}
          />
        </Box>
      )}
    </Stack>
  );
}
