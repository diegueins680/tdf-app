import * as React from 'react';
import { Stack, Typography } from '@mui/material';
import { useBackendVersion } from '../hooks/useBackendVersion';

export default function AboutVersion() {
  const apiBase = (import.meta.env.VITE_API_BASE as string | undefined) || '';
  const { data, isLoading, error } = useBackendVersion(apiBase);
  if (isLoading) return <Typography variant="body2">Loading version…</Typography>;
  if (error || !data) return <Typography variant="body2">Version: unknown</Typography>;
  return (
    <Stack spacing={0.5}>
      <Typography variant="body2">Backend: {data.app || 'tdf-hq'}</Typography>
      <Typography variant="body2">Version: {data.version}</Typography>
      {data.builtAt ? (
        <Typography variant="body2">
          Built: {new Date(data.builtAt).toLocaleString()}
        </Typography>
      ) : null}
    </Stack>
  );
}

