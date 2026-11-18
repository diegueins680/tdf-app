import type { ReactNode } from 'react';
import { Link, type SxProps, type Theme } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { buildArtistPublicPath } from '../utils/artistIdentity';

export default function ArtistProfileLink({ partyId, slug, children, sx }: {
  partyId?: number | string | null;
  slug?: string | null;
  children: ReactNode;
  sx?: SxProps<Theme>;
}) {
  const path = buildArtistPublicPath(partyId, slug);
  if (!path) return <>{children}</>;

  return (
    <Link
      component={RouterLink}
      to={path}
      color="inherit"
      underline="hover"
      onClick={(event) => event.stopPropagation()}
      sx={sx}
    >
      {children}
    </Link>
  );
}
