import { Avatar, Box, Chip, Stack, Typography } from '@mui/material';
import tdfWordmark from '../assets/tdf-logo-wordmark.svg';
import tdfIsotype from '../assets/tdf-logo-alt.svg';

interface PublicBrandBarProps {
  tagline?: string;
  compact?: boolean;
}

/**
 * Lightweight brand strip for public pages. Keeps the TDF Records logo prominent
 * and adds a short descriptor per page.
 */
export function PublicBrandBar({ tagline, compact }: PublicBrandBarProps) {
  return (
    <Box
      sx={{
        border: '1px solid rgba(255,255,255,0.08)',
        borderRadius: 999,
        px: { xs: 1.5, md: 2.25 },
        py: { xs: 0.75, md: 1 },
        display: 'inline-flex',
        alignItems: 'center',
        gap: { xs: 1, md: 1.5 },
        backdropFilter: 'blur(10px)',
        bgcolor: 'rgba(12,16,28,0.6)',
        boxShadow: '0 20px 50px rgba(0,0,0,0.35)',
      }}
    >
      <Avatar
        src={tdfIsotype}
        alt="TDF Records logo"
        variant="rounded"
        sx={{
          width: compact ? 40 : 48,
          height: compact ? 40 : 48,
          bgcolor: '#0f1629',
          border: '1px solid rgba(255,255,255,0.12)',
          padding: 0.75,
        }}
      />
      <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
        <Box
          component="img"
          src={tdfWordmark}
          alt="TDF Records wordmark"
          sx={{
            height: compact ? 24 : 30,
            filter: 'drop-shadow(0px 4px 10px rgba(0,0,0,0.35))',
          }}
        />
        <Chip
          size="small"
          label="Records · Studio · Sessions"
          sx={{
            bgcolor: 'rgba(124,58,237,0.16)',
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
  );
}

export default PublicBrandBar;
