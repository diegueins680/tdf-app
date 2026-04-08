import * as React from 'react';
import Chip from '@mui/material/Chip';
import { useQuery } from '@tanstack/react-query';

const API_BASE = import.meta.env.VITE_API_BASE as string | undefined;

async function checkHealth() {
  if (!API_BASE) throw new Error('API base not configured');
  const res = await fetch(`${API_BASE.replace(/\/$/, '')}/health`, {
    headers: { Accept: 'application/json' },
  });
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  // /health can be JSON or plain text. If not JSON, still treat as healthy.
  try {
    await res.json();
  } catch {
    /* ignore parse errors */
  }
  return true;
}

export function ApiStatusChip() {
  const enabled = Boolean(API_BASE);
  const { isLoading, isError } = useQuery({
    queryKey: ['api-health'],
    queryFn: checkHealth,
    refetchInterval: 30_000,
    enabled,
  });

  if (!enabled) {
    return <Chip label="API: not configured" size="small" />;
  }
  if (isLoading) {
    return <Chip label="API: checking..." size="small" />;
  }
  if (isError) {
    return <Chip label="API: offline" size="small" color="warning" />;
  }
  return <Chip label="API: online" size="small" color="success" />;
}

export default ApiStatusChip;
