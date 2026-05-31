import type { ReactNode } from 'react';
import { Box, Stack, Typography, Button, Skeleton } from '@mui/material';
import InboxOutlinedIcon from '@mui/icons-material/InboxOutlined';
import { Link as RouterLink } from 'react-router-dom';

type PositivePixelDimension = number;

interface LoadingHeaderSkeletonDimensions {
  readonly titleWidthPx: PositivePixelDimension;
  readonly titleHeightPx: PositivePixelDimension;
  readonly subtitleWidthPx: PositivePixelDimension;
  readonly subtitleHeightPx: PositivePixelDimension;
}

// Invariant: every skeleton dimension is a positive pixel value that reserves stable loading layout space.
const LOADING_HEADER_SKELETON_DIMENSIONS = {
  titleWidthPx: 200,
  titleHeightPx: 36,
  subtitleWidthPx: 280,
  subtitleHeightPx: 20,
} as const satisfies LoadingHeaderSkeletonDimensions;

const SKELETON_CARD_HEIGHT_PX: PositivePixelDimension = 120;

export interface PageShellProps {
  title: string;
  subtitle?: string;
  actions?: ReactNode;
  children: ReactNode;
  loading?: boolean;
  maxWidth?: 'sm' | 'md' | 'lg' | 'xl' | false;
}

export default function PageShell({
  title,
  subtitle,
  actions,
  children,
  loading = false,
  maxWidth = 'xl',
}: PageShellProps) {
  return (
    <Stack spacing={4}>
      <Box
        sx={{
          position: 'sticky',
          top: 0,
          zIndex: 10,
          bgcolor: 'background.default',
          pt: { xs: 2, md: 3 },
          pb: 2,
          borderBottom: '1px solid',
          borderColor: 'divider',
          mb: -2,
        }}
      >
        <Stack
          direction={{ xs: 'column', sm: 'row' }}
          spacing={{ xs: 1.5, sm: 2 }}
          alignItems={{ xs: 'flex-start', sm: 'center' }}
          justifyContent="space-between"
        >
          <Stack spacing={0.5} sx={{ minWidth: 0 }}>
            {loading ? (
              <Box
                role="status"
                aria-busy="true"
                aria-live="polite"
                aria-label="Cargando…"
                sx={{ display: 'flex', flexDirection: 'column', gap: 0.5 }}
              >
                <Skeleton
                  variant="text"
                  width={LOADING_HEADER_SKELETON_DIMENSIONS.titleWidthPx}
                  height={LOADING_HEADER_SKELETON_DIMENSIONS.titleHeightPx}
                  aria-hidden="true"
                />
                <Skeleton
                  variant="text"
                  width={LOADING_HEADER_SKELETON_DIMENSIONS.subtitleWidthPx}
                  height={LOADING_HEADER_SKELETON_DIMENSIONS.subtitleHeightPx}
                  aria-hidden="true"
                />
              </Box>
            ) : (
              <>
                <Typography variant="h3" sx={{ fontSize: { xs: '1.35rem', md: '1.75rem' } }}>
                  {title}
                </Typography>
                {subtitle && (
                  <Typography variant="body1" color="text.secondary">
                    {subtitle}
                  </Typography>
                )}
              </>
            )}
          </Stack>
          {actions && <Stack direction="row" spacing={1}>{actions}</Stack>}
        </Stack>
      </Box>
      <Box sx={{ maxWidth: maxWidth === false ? undefined : `${maxWidthMap[maxWidth]}px`, width: '100%' }}>
        {children}
      </Box>
    </Stack>
  );
}

const maxWidthMap: Record<string, number> = {
  sm: 600,
  md: 900,
  lg: 1200,
  xl: 1536,
};

/* ------------------------------------------------------------------ */
// EmptyState

export interface EmptyStateProps {
  icon?: ReactNode;
  title: string;
  description?: string;
  actionLabel?: string;
  actionHref?: string;
  actionOnClick?: () => void;
  children?: ReactNode;
}

export function EmptyState({
  icon,
  title,
  description,
  actionLabel,
  actionHref,
  actionOnClick,
  children,
}: EmptyStateProps) {
  const actionButton =
    actionLabel && (actionHref || actionOnClick) ? (
      actionHref ? (
        <Button variant="contained" component={RouterLink} to={actionHref}>
          {actionLabel}
        </Button>
      ) : (
        <Button variant="contained" onClick={actionOnClick}>
          {actionLabel}
        </Button>
      )
    ) : null;

  // Fall back to a soft, neutral illustration so empty pages never feel stark.
  const resolvedIcon = icon ?? <InboxOutlinedIcon fontSize="inherit" />;

  return (
    <Box
      sx={{
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: 'center',
        textAlign: 'center',
        py: { xs: 6, md: 10 },
        px: 3,
        gap: 2,
      }}
    >
      <Box
        aria-hidden="true"
        sx={{
          width: 72,
          height: 72,
          borderRadius: 3,
          bgcolor: 'action.hover',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          color: 'text.secondary',
          fontSize: 32,
        }}
      >
        {resolvedIcon}
      </Box>
      <Stack spacing={0.5} alignItems="center">
        <Typography variant="h5" color="text.primary">
          {title}
        </Typography>
        {description && (
          <Typography variant="body2" color="text.secondary" sx={{ maxWidth: 360 }}>
            {description}
          </Typography>
        )}
      </Stack>
      {actionButton}
      {children}
    </Box>
  );
}

/* ------------------------------------------------------------------ */
// SkeletonCards — placeholder for loading dashboards/lists

export function SkeletonCards({ count = 3 }: { count?: number }) {
  return (
    <Stack
      spacing={3}
      role="status"
      aria-busy="true"
      aria-live="polite"
      aria-label="Cargando contenido…"
    >
      {Array.from({ length: count }).map((_, i) => (
        <Skeleton
          key={i}
          variant="rounded"
          height={SKELETON_CARD_HEIGHT_PX}
          sx={{ borderRadius: 3 }}
          aria-hidden="true"
        />
      ))}
    </Stack>
  );
}
