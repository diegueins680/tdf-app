import { Box, Skeleton, Stack } from '@mui/material';

export default function RouteLoadingFallback() {
  return (
    <Box
      role="status"
      aria-live="polite"
      aria-busy="true"
      aria-label="Cargando…"
      sx={{ p: 3, maxWidth: 1200, mx: 'auto' }}
    >
      {/* Header skeleton */}
      <Stack direction="row" justifyContent="space-between" alignItems="center" sx={{ mb: 3 }}>
        <Stack spacing={1}>
          <Skeleton variant="text" width={200} height={32} />
          <Skeleton variant="text" width={120} height={20} />
        </Stack>
        <Skeleton variant="rounded" width={100} height={36} />
      </Stack>

      {/* Content skeleton */}
      <Stack spacing={2}>
        <Skeleton variant="rounded" height={48} />
        <Skeleton variant="rounded" height={200} />
        <Skeleton variant="rounded" height={200} />
      </Stack>
    </Box>
  );
}
