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

type PositivePixelSize = number & { readonly __brand: 'PositivePixelSize' };
type NumericFontWeight = number & { readonly __brand: 'NumericFontWeight' };

const toPositivePixelSize = (value: number): PositivePixelSize => {
  if (!Number.isFinite(value) || value <= 0) {
    throw new Error('Pixel sizes must be positive.');
  }

  return value as PositivePixelSize;
};

const toNumericFontWeight = (value: number): NumericFontWeight => {
  if (!Number.isFinite(value) || value <= 0 || value > 1000) {
    throw new Error('Font weights must be finite CSS weights.');
  }

  return value as NumericFontWeight;
};

const DEFAULT_MUI_SPACING_UNIT_PX = toPositivePixelSize(8);
const LOADING_SPINNER_EXTRA_PX = toPositivePixelSize(2);
const CSS_FONT_WEIGHT_STEP = 100 as const;

const LOADING_SPINNER_SIZE_PX = toPositivePixelSize(
  DEFAULT_MUI_SPACING_UNIT_PX * 2 + LOADING_SPINNER_EXTRA_PX,
);
const FAN_AVATAR_SIZE_PX = toPositivePixelSize(DEFAULT_MUI_SPACING_UNIT_PX * 6);
const FAN_DISPLAY_NAME_FONT_WEIGHT = toNumericFontWeight(CSS_FONT_WEIGHT_STEP * 7);

type SquareFanAvatarSx = Readonly<{
  width: PositivePixelSize;
  height: PositivePixelSize;
  bgcolor: 'primary.light';
}>;

const FAN_AVATAR_SX: SquareFanAvatarSx = {
  width: FAN_AVATAR_SIZE_PX,
  height: FAN_AVATAR_SIZE_PX,
  bgcolor: 'primary.light',
};

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
        <CircularProgress size={LOADING_SPINNER_SIZE_PX} />
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
              sx={FAN_AVATAR_SX}
            >
              {fan.afDisplayName?.[0]?.toUpperCase() ?? <PersonIcon />}
            </Avatar>
            <Box sx={{ flex: 1, minWidth: 0 }}>
              <Typography fontWeight={FAN_DISPLAY_NAME_FONT_WEIGHT} sx={{ lineHeight: 1.3 }}>
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
