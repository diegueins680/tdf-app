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
  MenuItem,
  Stack,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import OpenInNewIcon from '@mui/icons-material/OpenInNew';
import RefreshIcon from '@mui/icons-material/Refresh';
import { Link as RouterLink, useLocation } from 'react-router-dom';
import { useInstagramAuth } from '../hooks/useInstagramAuth';
import {
  getInstagramOAuthProvider,
  getInstagramRequestedScopes,
  getMetaReviewAssetSelection,
  getStoredInstagramResult,
  setMetaReviewAssetSelection,
} from '../services/instagramAuth';
import type { InstagramOAuthExchangeResponse } from '../api/instagramOAuth';

const META_REVIEW_REQUIRED_SCOPES_FACEBOOK = [
  'instagram_basic',
  'instagram_manage_messages',
] as const;

const META_REVIEW_REQUIRED_SCOPES_INSTAGRAM = [
  'instagram_business_basic',
  'instagram_business_manage_messages',
] as const;

const scopeLabel = (scope: string) => scope.replace(/_/g, ' ');

export default function InstagramConnectPage() {
  const { status, error, startAuth, resetAuth } = useInstagramAuth();
  const location = useLocation();
  const reviewMode = useMemo(() => new URLSearchParams(location.search).get('review') === '1', [location.search]);
  const [result, setResult] = useState<InstagramOAuthExchangeResponse | null>(() => getStoredInstagramResult());
  const [selectedPageId, setSelectedPageId] = useState<string>(() => getMetaReviewAssetSelection()?.pageId ?? '');

  useEffect(() => {
    if (status === 'ready') {
      setResult(getStoredInstagramResult());
    }
  }, [status]);

  const pages = result?.pages ?? [];
  const media = result?.media ?? [];
  const connectedHandle = result?.instagramUsername?.trim();
  const connectedHandleLabel = connectedHandle && connectedHandle.length > 0 ? connectedHandle : null;
  const oauthProvider = useMemo(() => getInstagramOAuthProvider(), []);
  const requiredReviewScopes = useMemo(
    () => (oauthProvider === 'instagram' ? META_REVIEW_REQUIRED_SCOPES_INSTAGRAM : META_REVIEW_REQUIRED_SCOPES_FACEBOOK),
    [oauthProvider],
  );
  const requestedScopes = useMemo(() => getInstagramRequestedScopes(), []);

  const missingReviewScopes = useMemo(
    () => requiredReviewScopes.filter((scope) => !requestedScopes.includes(scope)),
    [requiredReviewScopes, requestedScopes],
  );

  useEffect(() => {
    if (pages.length === 0) {
      setSelectedPageId('');
      setMetaReviewAssetSelection(null);
      return;
    }
    const existingSelection = pages.some((page) => page.pageId === selectedPageId);
    if (!existingSelection) {
      const persistedId = getMetaReviewAssetSelection()?.pageId;
      const persistedPage = persistedId ? pages.find((page) => page.pageId === persistedId) ?? null : null;
      const next = persistedPage ?? pages[0] ?? null;
      if (!next) return;
      setSelectedPageId(next.pageId);
      setMetaReviewAssetSelection(next);
    }
  }, [pages, selectedPageId]);

  const selectedPage = useMemo(() => {
    if (pages.length === 0) return null;
    return pages.find((page) => page.pageId === selectedPageId) ?? pages[0];
  }, [pages, selectedPageId]);

  useEffect(() => {
    if (!selectedPage) return;
    setMetaReviewAssetSelection(selectedPage);
  }, [selectedPage]);

  const pageSummary = useMemo(() => {
    if (pages.length === 0) return reviewMode ? 'No messaging assets linked yet.' : 'Sin páginas vinculadas todavía.';
    return reviewMode ? `${pages.length} messaging asset(s) available.` : `${pages.length} página(s) con acceso.`;
  }, [pages.length, reviewMode]);

  return (
    <Box>
      <Stack spacing={2} sx={{ mb: 3 }}>
        <Typography variant="h4" fontWeight={800}>
          {reviewMode ? 'Meta App Review: Instagram Setup' : 'Conectar Instagram'}
        </Typography>
        <Typography variant="body1" color="text.secondary">
          {reviewMode
            ? 'Use this screen in the screencast to show Meta login, permission grant, and visible asset selection before sending a message.'
            : 'Autoriza tu cuenta de Facebook para leer perfiles de Instagram profesionales y mostrar el media más reciente.'}
        </Typography>
        {error && <Alert severity="error">{error}</Alert>}
      </Stack>

      {reviewMode && (
        <Card sx={{ mb: 3 }}>
          <CardContent>
            <Stack spacing={1.5}>
              <Typography variant="h6" fontWeight={700}>
                Recording checklist (Step 1 of 3)
              </Typography>
              <Typography variant="body2" color="text.secondary">
                Start recording before clicking Connect, keep the permissions dialog visible, and return to this page after consent.
              </Typography>
              <Stack direction="row" spacing={1} flexWrap="wrap">
                {requiredReviewScopes.map((scope) => {
                  const enabled = requestedScopes.includes(scope);
                  return (
                    <Chip
                      key={scope}
                      label={`${scopeLabel(scope)}${enabled ? ' (requested)' : ' (missing)'}`}
                      color={enabled ? 'success' : 'warning'}
                      variant={enabled ? 'filled' : 'outlined'}
                      sx={{ mb: 1 }}
                    />
                  );
                })}
              </Stack>
              {missingReviewScopes.length > 0 && (
                <Alert severity="warning">
                  Missing scopes in current config ({oauthProvider}): {missingReviewScopes.join(', ')}
                </Alert>
              )}
            </Stack>
          </CardContent>
        </Card>
      )}

      <Card sx={{ mb: 3 }}>
        <CardContent>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ xs: 'stretch', sm: 'center' }}>
            <Box sx={{ flex: 1 }}>
              <Typography variant="h6" fontWeight={700}>
                {reviewMode ? 'Connection status' : 'Estado de conexión'}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                {result
                  ? reviewMode
                    ? `Connected to ${connectedHandleLabel ?? 'Instagram account'}`
                    : `Conectado a ${connectedHandleLabel ?? 'Instagram'}`
                  : reviewMode
                    ? 'No active connection yet.'
                    : 'Aún no hay una conexión activa.'}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                {pageSummary}
              </Typography>
            </Box>
            <Stack direction="row" spacing={1}>
              <Tooltip
                title={
                  reviewMode
                    ? 'Shows Meta login + permissions modal in the screencast.'
                    : 'Inicia el flujo de autorización en Meta.'
                }
              >
                <span>
                  <Button
                    variant="contained"
                    onClick={() => startAuth(reviewMode ? '/social/instagram?review=1' : '/social/instagram')}
                    disabled={status === 'authenticating'}
                  >
                    {result
                      ? reviewMode
                        ? 'Re-authorize'
                        : 'Reautorizar'
                      : reviewMode
                        ? 'Connect with Meta Login'
                        : 'Conectar'}
                  </Button>
                </span>
              </Tooltip>
              {result && (
                <Button variant="outlined" startIcon={<RefreshIcon />} onClick={resetAuth}>
                  {reviewMode ? 'Disconnect' : 'Desconectar'}
                </Button>
              )}
            </Stack>
          </Stack>
        </CardContent>
      </Card>

      {result && reviewMode && (
        <Card sx={{ mb: 3 }}>
          <CardContent>
            <Stack spacing={2}>
              <Typography variant="h6" fontWeight={700}>
                Asset selection (required by Meta reviewer)
              </Typography>
              <Typography variant="body2" color="text.secondary">
                Select the exact Page/account that will be used to send messages. Keep this selection visible in the screencast.
              </Typography>
              <TextField
                select
                label="Messaging asset"
                value={selectedPage?.pageId ?? ''}
                onChange={(event) => {
                  const pageId = event.target.value;
                  setSelectedPageId(pageId);
                  const next = pages.find((page) => page.pageId === pageId) ?? null;
                  setMetaReviewAssetSelection(next);
                }}
                fullWidth
                disabled={pages.length === 0}
              >
                {pages.map((page) => (
                  <MenuItem key={page.pageId} value={page.pageId}>
                    {`${page.pageName}${page.instagramUsername ? ` · @${page.instagramUsername}` : ''}`}
                  </MenuItem>
                ))}
              </TextField>
              {selectedPage && (
                <Stack spacing={0.5}>
                  <Typography variant="body2">Selected Page: {selectedPage.pageName}</Typography>
                  <Typography variant="body2">Page ID: {selectedPage.pageId}</Typography>
                  <Typography variant="body2">
                    Instagram account: {selectedPage.instagramUsername ? `@${selectedPage.instagramUsername}` : 'Not linked'}
                  </Typography>
                  <Typography variant="body2">Instagram User ID: {selectedPage.instagramUserId ?? '—'}</Typography>
                </Stack>
              )}
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                <Button
                  variant="contained"
                  component={RouterLink}
                  to="/social/inbox?review=1"
                >
                  Continue to message send flow
                </Button>
                <Button
                  variant="outlined"
                  startIcon={<OpenInNewIcon />}
                  component={RouterLink}
                  to="/social/inbox?review=1"
                >
                  Open inbox in review mode
                </Button>
              </Stack>
            </Stack>
          </CardContent>
        </Card>
      )}

      {result && (
        <Stack spacing={3}>
          <Card>
            <CardContent>
              <Typography variant="h6" fontWeight={700} gutterBottom>
                {reviewMode ? 'Connected Pages with Instagram' : 'Páginas con Instagram'}
              </Typography>
              <Stack direction="row" spacing={1} flexWrap="wrap">
                {pages.length === 0 && (
                  <Typography variant="body2" color="text.secondary">
                    {reviewMode
                      ? 'No Page with a linked professional Instagram account was found.'
                      : 'No se detectaron páginas con cuenta de Instagram profesional vinculada.'}
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
                  {reviewMode ? 'Recent media (use case evidence)' : 'Media reciente'}
                </Typography>
                {connectedHandleLabel && (
                  <Button
                    size="small"
                    variant="text"
                    endIcon={<OpenInNewIcon />}
                    href={`https://www.instagram.com/${connectedHandleLabel}`}
                    target="_blank"
                    rel="noreferrer"
                  >
                    {reviewMode ? 'Open profile' : 'Ver perfil'}
                  </Button>
                )}
              </Stack>
              <Divider sx={{ mb: 2 }} />
              {media.length === 0 ? (
                <Typography variant="body2" color="text.secondary">
                  {reviewMode
                    ? 'No recent media found for the connected profile.'
                    : 'No se encontró media reciente para este perfil.'}
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
                            {item.caption ? item.caption.slice(0, 120) : reviewMode ? 'No caption' : 'Sin caption'}
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
                              {reviewMode ? 'Open post' : 'Abrir'}
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
