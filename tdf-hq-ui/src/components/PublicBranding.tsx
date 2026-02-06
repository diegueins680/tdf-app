import { Box, Button, Container, IconButton, Menu, MenuItem, Stack, Tooltip, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import BrandLogo from './BrandLogo';
import type { ReactNode } from 'react';
import { useState } from 'react';
import MoreVertIcon from '@mui/icons-material/MoreVert';

export default function PublicBranding({
  children,
  showHeader = true,
  showLoginButton = true,
}: {
  children: ReactNode;
  showHeader?: boolean;
  showLoginButton?: boolean;
}) {
  const [menuAnchor, setMenuAnchor] = useState<null | HTMLElement>(null);
  const open = Boolean(menuAnchor);

  return (
    <Box sx={{ minHeight: '100vh', bgcolor: 'background.default' }}>
      {showHeader && (
        <Box
          sx={{
            borderBottom: '1px solid',
            borderColor: 'divider',
            bgcolor: '#0f1118',
            py: 1.5,
          }}
        >
          <Container maxWidth="xl">
            <Stack direction="row" alignItems="center" justifyContent="space-between" spacing={2}>
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
              <Stack direction="row" alignItems="center" spacing={1.5}>
                {showLoginButton && (
                  <Button
                    variant="contained"
                    color="secondary"
                    component={RouterLink}
                    to="/login"
                    sx={{ textTransform: 'none' }}
                  >
                    Ingresar
                  </Button>
                )}
                <Tooltip title="Más opciones">
                  <IconButton
                    aria-label="Más opciones"
                    onClick={(e) => setMenuAnchor(e.currentTarget)}
                    sx={{ color: '#e2e8f0' }}
                  >
                    <MoreVertIcon />
                  </IconButton>
                </Tooltip>
                <Menu
                  anchorEl={menuAnchor}
                  open={open}
                  onClose={() => setMenuAnchor(null)}
                  anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
                  transformOrigin={{ vertical: 'top', horizontal: 'right' }}
                >
                  <MenuItem component={RouterLink} to="/reservar" onClick={() => setMenuAnchor(null)}>
                    Reservar
                  </MenuItem>
                  <MenuItem component={RouterLink} to="/donar" onClick={() => setMenuAnchor(null)}>
                    Donar
                  </MenuItem>
                  <MenuItem component={RouterLink} to="/feedback" onClick={() => setMenuAnchor(null)}>
                    Enviar feedback
                  </MenuItem>
                </Menu>
              </Stack>
            </Stack>
          </Container>
        </Box>
      )}
      <Container maxWidth="xl" sx={{ py: { xs: showHeader ? 2 : 3, md: showHeader ? 4 : 5 } }}>
        {children}
      </Container>
      <Box
        sx={{
          borderTop: '1px solid',
          borderColor: 'divider',
          bgcolor: '#0f1118',
          py: 2,
        }}
      >
        <Container maxWidth="xl">
          <Stack
            direction={{ xs: 'column', md: 'row' }}
            spacing={1.5}
            alignItems={{ xs: 'flex-start', md: 'center' }}
            justifyContent="space-between"
          >
            <Typography variant="body2" color="text.secondary">
              TDF Records
            </Typography>
            <Stack direction="row" spacing={2} flexWrap="wrap">
              <Button
                size="small"
                color="inherit"
                component={RouterLink}
                to="/whatsapp/consentimiento"
                sx={{ textTransform: 'none' }}
              >
                Consentimiento WhatsApp
              </Button>
              <Button
                size="small"
                color="inherit"
                component={RouterLink}
                to="/whatsapp/ok"
                sx={{ textTransform: 'none' }}
              >
                Confirmacion WhatsApp
              </Button>
            </Stack>
          </Stack>
        </Container>
      </Box>
    </Box>
  );
}
