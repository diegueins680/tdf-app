import { Box, Chip, Link as MuiLink, Typography } from '@mui/material';
import tdfWordmark from '../assets/tdf-logo-wordmark.svg';

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
  const wordmarkHeight = compact ? 150 : 210;

  return (
    <MuiLink href={href} target={target} underline="none" sx={{ display: 'inline-block' }}>
      <Box
        sx={{
          border: '1px solid rgba(255,255,255,0.08)',
          borderRadius: 999,
          px: { xs: 1.2, md: 1.4 },
          py: { xs: 0.7, md: 0.85 },
          display: 'inline-flex',
          alignItems: 'center',
          gap: { xs: 0.7, md: 0.9 },
          flexWrap: 'wrap',
          justifyContent: 'center',
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
        <Box sx={{ display: 'flex', alignItems: 'center', gap: { xs: 0.6, md: 0.8 }, flexWrap: 'wrap' }}>
          <Box
            component="img"
            src={tdfWordmark}
            alt="TDF Records wordmark"
            sx={{
              height: wordmarkHeight,
              filter: 'brightness(0) invert(1) drop-shadow(0px 10px 26px rgba(0,0,0,0.65))',
              opacity: 0.95,
              display: 'block',
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
