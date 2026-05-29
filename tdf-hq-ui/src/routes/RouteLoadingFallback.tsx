import { Box, CircularProgress, Stack, Typography } from '@mui/material';

export default function RouteLoadingFallback() {
  return (
    <Box
      role="status"
      aria-live="polite"
      aria-busy="true"
      sx={{
        minHeight: '100vh',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        bgcolor: 'background.default',
      }}
    >
      <Stack spacing={2} alignItems="center">
        <CircularProgress />
        <Typography variant="body2" color="text.secondary">
          Cargando…
        </Typography>
      </Stack>
    </Box>
  );
}
