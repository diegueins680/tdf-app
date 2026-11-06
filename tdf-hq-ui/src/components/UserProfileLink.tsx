import type { ReactNode } from 'react';
import { Box } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';

/** Link a known party identity to its profile; never guess an ID from a display name. */
export default function UserProfileLink({ partyId, children, label }: {
  partyId?: number | string | null;
  children: ReactNode;
  label?: string;
}) {
  const id = String(partyId ?? '');
  if (!/^[1-9]\d*$/.test(id) || !Number.isSafeInteger(Number(id))) return <>{children}</>;
  return <Box component={RouterLink} to={`/perfil/${id}`} aria-label={label}
    onClick={(event) => event.stopPropagation()}
    sx={{ display: 'inline-flex', alignItems: 'center', gap: 1, minWidth: 0, color: 'inherit', textDecoration: 'none', borderRadius: 1,
      '&:hover': { textDecoration: 'underline' }, '&:focus-visible': { outline: '2px solid', outlineColor: 'primary.main', outlineOffset: 3 } }}>
    {children}
  </Box>;
}
