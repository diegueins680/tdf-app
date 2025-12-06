import { Box, Container, Stack } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import BrandLogo from './BrandLogo';
import type { ReactNode } from 'react';

export default function PublicBranding({ children }: { children: ReactNode }) {
  return (
    <Box sx={{ minHeight: '100vh', bgcolor: 'background.default' }}>
      <Box
        sx={{
          borderBottom: '1px solid',
          borderColor: 'divider',
          bgcolor: '#0f1118',
          py: 1.5,
        }}
      >
        <Container maxWidth="xl">
          <Stack direction="row" alignItems="center" spacing={2}>
            <Box
              component={RouterLink}
              to="/inicio"
              sx={{ display: 'inline-flex', alignItems: 'center' }}
              aria-label="Ir al inicio"
            >
              <BrandLogo
                variant="wordmark"
                size={48}
                sx={{ filter: 'brightness(0) invert(1)' }}
              />
            </Box>
          </Stack>
        </Container>
      </Box>
      <Container maxWidth="xl" sx={{ py: { xs: 2, md: 4 } }}>
        {children}
      </Container>
    </Box>
  );
}
