import { Avatar, Box, Chip, Link as MuiLink, Stack, Typography } from '@mui/material';
import tdfWordmark from '../assets/tdf-logo-wordmark.svg';
import tdfIsotype from '../assets/tdf-logo-alt.svg';

interface PublicBrandBarProps {
  tagline?: string;
  compact?: boolean;
  href?: string;
  target?: string;
}

/**
 * Lightweight brand strip for public pages. Keeps the TDF Records logo prominent
 * and adds a short descriptor per page.
 */
export function PublicBrandBar({ tagline, compact, href = '/records', target }: PublicBrandBarProps) {
  const isoSize = compact ? 56 : 64;
  const wordmarkHeight = compact ? 32 : 38;

  return (
    <MuiLink href={href} target={target} underline="none" sx={{ display: 'inline-block' }}>
      <Box
        sx={{
          border: '1px solid rgba(255,255,255,0.08)',
          borderRadius: 999,
          px: { xs: 1.75, md: 2.5 },
          py: { xs: 1, md: 1.1 },
          display: 'inline-flex',
          alignItems: 'center',
          gap: { xs: 1.25, md: 1.75 },
          backdropFilter: 'blur(10px)',
          bgcolor: 'rgba(12,16,28,0.7)',
          boxShadow: '0 20px 50px rgba(0,0,0,0.35)',
          transition: 'transform 180ms ease, box-shadow 180ms ease',
          '&:hover': {
            transform: 'translateY(-2px)',
            boxShadow: '0 24px 60px rgba(0,0,0,0.45)',
          },
        }}
      >
        <Avatar
          src={tdfIsotype}
          alt="TDF Records logo"
          variant="rounded"
          sx={{
            width: isoSize,
            height: isoSize,
            bgcolor: '#0f1629',
            border: '1px solid rgba(255,255,255,0.12)',
            padding: 1,
          }}
        />
        <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
          <Box
            component="img"
            src={tdfWordmark}
            alt="TDF Records wordmark"
            sx={{
              height: wordmarkHeight,
              filter: 'drop-shadow(0px 4px 10px rgba(0,0,0,0.35))',
            }}
          />
          <Chip
            size="small"
            label="Records · Studio · Sessions"
            sx={{
              bgcolor: 'rgba(124,58,237,0.18)',
              color: '#e5e7eb',
              fontWeight: 700,
              border: '1px solid rgba(255,255,255,0.14)',
            }}
          />
        </Box>
        {tagline && (
          <Typography
            variant="body2"
            sx={{ color: 'rgba(226,232,240,0.9)', fontWeight: 600, letterSpacing: 0.2 }}
          >
            {tagline}
          </Typography>
        )}
      </Box>
    </MuiLink>
  );
}

export default PublicBrandBar;
