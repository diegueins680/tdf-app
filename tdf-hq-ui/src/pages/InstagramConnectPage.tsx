import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  CardMedia,
  Chip,
  Divider,
  Grid,
  Stack,
  Typography,
} from '@mui/material';
import OpenInNewIcon from '@mui/icons-material/OpenInNew';
import RefreshIcon from '@mui/icons-material/Refresh';
import { useInstagramAuth } from '../hooks/useInstagramAuth';
import { getStoredInstagramResult } from '../services/instagramAuth';
import type { InstagramOAuthExchangeResponse } from '../api/instagramOAuth';

export default function InstagramConnectPage() {
  const { status, error, startAuth, resetAuth } = useInstagramAuth();
  const [result, setResult] = useState<InstagramOAuthExchangeResponse | null>(() => getStoredInstagramResult());

  useEffect(() => {
    if (status === 'ready') {
      setResult(getStoredInstagramResult());
    }
  }, [status]);

  const pages = result?.pages ?? [];
  const media = result?.media ?? [];
  const connectedHandle = result?.instagramUsername ?? null;

  const pageSummary = useMemo(() => {
    if (pages.length === 0) return 'Sin páginas vinculadas todavía.';
    return `${pages.length} página(s) con acceso.`;
  }, [pages.length]);

  return (
    <Box>
      <Stack spacing={2} sx={{ mb: 3 }}>
        <Typography variant="h4" fontWeight={800}>
          Conectar Instagram
        </Typography>
        <Typography variant="body1" color="text.secondary">
          Autoriza tu cuenta de Facebook para leer perfiles de Instagram profesionales y mostrar el media más reciente.
        </Typography>
        {error && <Alert severity="error">{error}</Alert>}
      </Stack>

      <Card sx={{ mb: 3 }}>
        <CardContent>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ xs: 'stretch', sm: 'center' }}>
            <Box sx={{ flex: 1 }}>
              <Typography variant="h6" fontWeight={700}>
                Estado de conexión
              </Typography>
              <Typography variant="body2" color="text.secondary">
                {result ? `Conectado a ${connectedHandle ?? 'Instagram'}` : 'Aún no hay una conexión activa.'}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                {pageSummary}
              </Typography>
            </Box>
            <Stack direction="row" spacing={1}>
              <Button
                variant="contained"
                onClick={() => startAuth('/social/instagram')}
                disabled={status === 'authenticating'}
              >
                {result ? 'Reautorizar' : 'Conectar'}
              </Button>
              {result && (
                <Button variant="outlined" startIcon={<RefreshIcon />} onClick={resetAuth}>
                  Desconectar
                </Button>
              )}
            </Stack>
          </Stack>
        </CardContent>
      </Card>

      {result && (
        <Stack spacing={3}>
          <Card>
            <CardContent>
              <Typography variant="h6" fontWeight={700} gutterBottom>
                Páginas con Instagram
              </Typography>
              <Stack direction="row" spacing={1} flexWrap="wrap">
                {pages.length === 0 && (
                  <Typography variant="body2" color="text.secondary">
                    No se detectaron páginas con cuenta de Instagram profesional vinculada.
                  </Typography>
                )}
                {pages.map((page) => (
                  <Chip
                    key={page.pageId}
                    label={`${page.pageName}${page.instagramUsername ? ` · @${page.instagramUsername}` : ''}`}
                    sx={{ mb: 1 }}
                  />
                ))}
              </Stack>
            </CardContent>
          </Card>

          <Card>
            <CardContent>
              <Stack direction="row" alignItems="center" justifyContent="space-between" sx={{ mb: 2 }}>
                <Typography variant="h6" fontWeight={700}>
                  Media reciente
                </Typography>
                {result.instagramUserId && (
                  <Button
                    size="small"
                    variant="text"
                    endIcon={<OpenInNewIcon />}
                    href={`https://www.instagram.com/${connectedHandle ?? ''}`}
                    target="_blank"
                    rel="noreferrer"
                  >
                    Ver perfil
                  </Button>
                )}
              </Stack>
              <Divider sx={{ mb: 2 }} />
              {media.length === 0 ? (
                <Typography variant="body2" color="text.secondary">
                  No se encontró media reciente para este perfil.
                </Typography>
              ) : (
                <Grid container spacing={2}>
                  {media.map((item) => (
                    <Grid item xs={12} sm={6} md={4} key={item.id}>
                      <Card variant="outlined">
                        {item.mediaUrl && (
                          <CardMedia component="img" image={item.mediaUrl} alt={item.caption ?? 'Instagram media'} />
                        )}
                        <CardContent>
                          <Typography variant="body2" color="text.secondary" gutterBottom>
                            {item.caption ? item.caption.slice(0, 120) : 'Sin caption'}
                          </Typography>
                          {item.permalink && (
                            <Button
                              size="small"
                              variant="text"
                              endIcon={<OpenInNewIcon />}
                              href={item.permalink}
                              target="_blank"
                              rel="noreferrer"
                            >
                              Abrir
                            </Button>
                          )}
                        </CardContent>
                      </Card>
                    </Grid>
                  ))}
                </Grid>
              )}
            </CardContent>
          </Card>
        </Stack>
      )}
    </Box>
  );
}
