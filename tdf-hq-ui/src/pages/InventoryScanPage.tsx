import { useCallback, useEffect, useState } from 'react';
import { Alert, Box, Button, Card, CardContent, Chip, Container, Stack, TextField, Typography } from '@mui/material';
import { useNavigate, useParams } from 'react-router-dom';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import type { AssetDTO } from '../api/types';
import { Inventory, type AssetCheckinRequest, type AssetCheckoutRequest } from '../api/inventory';

export default function InventoryScanPage() {
  const { token } = useParams();
  const qc = useQueryClient();
  const navigate = useNavigate();
  const [asset, setAsset] = useState<AssetDTO | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [feedback, setFeedback] = useState<string | null>(null);
  const [formError, setFormError] = useState<string | null>(null);
  const [checkoutForm, setCheckoutForm] = useState<AssetCheckoutRequest>({ coTargetKind: 'party' });
  const [checkinForm, setCheckinForm] = useState<AssetCheckinRequest>({});
  const [nextToken, setNextToken] = useState('');

  const loadAsset = useCallback(async () => {
    if (!token) return;
    try {
      const data = await Inventory.byQrToken(token);
      setAsset(data);
      setError(null);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'No pudimos cargar el equipo.');
    }
  }, [token]);

  useEffect(() => {
    void loadAsset();
  }, [loadAsset]);

  const checkoutMutation = useMutation({
    mutationFn: () =>
      asset ? Inventory.checkout(asset.assetId, checkoutForm) : Promise.reject(new Error('No asset')),
    onSuccess: async () => {
      void qc.invalidateQueries({ queryKey: ['assets'] });
      await loadAsset();
      setFeedback('Equipo marcado como check-out.');
      setFormError(null);
    },
    onError: (err) => setError(err instanceof Error ? err.message : 'No pudimos registrar el check-out.'),
  });

  const checkinMutation = useMutation({
    mutationFn: () =>
      asset ? Inventory.checkin(asset.assetId, checkinForm) : Promise.reject(new Error('No asset')),
    onSuccess: async () => {
      void qc.invalidateQueries({ queryKey: ['assets'] });
      await loadAsset();
      setFeedback('Equipo marcado como devuelto.');
      setFormError(null);
    },
    onError: (err) => setError(err instanceof Error ? err.message : 'No pudimos registrar el check-in.'),
  });

  const handleCheckoutSubmit = () => {
    const target = checkoutForm.coTargetParty?.trim() ?? '';
    if (!target) {
      setFormError('Agrega un destino para el check-out (persona, sala o referencia).');
      return;
    }
    setFormError(null);
    checkoutMutation.mutate();
  };

  const handleCheckinSubmit = () => {
    const condition = checkinForm.ciConditionIn?.trim() ?? '';
    if (!condition) {
      setFormError('Describe la condición al recibir el equipo.');
      return;
    }
    setFormError(null);
    checkinMutation.mutate();
  };

const normalizedStatus = asset?.status?.toLowerCase() ?? '';
const isCheckedOut = normalizedStatus.includes('book') || normalizedStatus.includes('out');
const isUnavailable = normalizedStatus.includes('maintenance') || normalizedStatus.includes('retired');

  if (!token) {
    return (
      <Container maxWidth="sm">
        <Stack spacing={2}>
          <Alert severity="error">Token QR no encontrado.</Alert>
          <Card>
            <CardContent>
              <Stack spacing={1.5}>
                <Typography variant="subtitle1">Pega el código para abrir un equipo</Typography>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                  <TextField
                    label="Código QR"
                    placeholder="Token del QR"
                    value={nextToken}
                    onChange={(e) => setNextToken(e.target.value)}
                    fullWidth
                  />
                  <Button
                    variant="contained"
                    onClick={() => {
                      const target = nextToken.trim();
                      if (!target) return;
                      navigate(`/inventario/scan/${target}`);
                    }}
                    disabled={!nextToken.trim()}
                  >
                    Abrir equipo
                  </Button>
                </Stack>
                <Typography variant="body2" color="text.secondary">
                  Escanea el QR físico para abrir el token o pide el enlace compartible.
                </Typography>
              </Stack>
            </CardContent>
          </Card>
        </Stack>
      </Container>
    );
  }

  return (
    <Box sx={{ minHeight: '100vh', background: '#0b1224', color: '#e2e8f0', py: 4 }}>
      <Container maxWidth="sm">
        <Stack spacing={2}>
          <Typography variant="h5" fontWeight={800}>Escaneo de equipo</Typography>
          {feedback && (
            <Alert severity="success" onClose={() => setFeedback(null)}>
              {feedback}
            </Alert>
          )}
          {formError && (
            <Alert severity="warning" onClose={() => setFormError(null)}>
              {formError}
            </Alert>
          )}
          {error && <Alert severity="error">{error}</Alert>}
          {asset ? (
            <Card sx={{ bgcolor: 'rgba(255,255,255,0.02)', border: '1px solid rgba(255,255,255,0.08)' }}>
              <CardContent>
                <Stack spacing={1}>
                  <Typography variant="h6">{asset.name}</Typography>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <Chip label={asset.category} size="small" />
                    <Chip label={`Estado: ${asset.status}`} size="small" />
                  </Stack>
                  <Typography variant="body2">Condición: {asset.condition ?? '—'}</Typography>
                  <Typography variant="body2">Ubicación: {asset.location ?? '—'}</Typography>
                  {isUnavailable ? (
                    <Stack spacing={1.5} sx={{ mt: 2 }}>
                      <Alert severity="warning">
                        Este equipo está marcado como {asset.status}. No se permite check-in/out mientras esté fuera de servicio.
                      </Alert>
                      <Button variant="outlined" onClick={() => void loadAsset()}>
                        Refrescar estado
                      </Button>
                    </Stack>
                  ) : isCheckedOut ? (
                    <>
                      <Typography variant="subtitle1" sx={{ mt: 2 }}>Registrar check-in</Typography>
                      <Stack direction="row" spacing={1} flexWrap="wrap">
                        {['Sin novedades', 'Limpio', 'Con desgaste menor'].map((preset) => (
                          <Button
                            key={preset}
                            size="small"
                            variant="outlined"
                            onClick={() =>
                              setCheckinForm((prev) => ({ ...prev, ciConditionIn: preset }))
                            }
                          >
                            {preset}
                          </Button>
                        ))}
                      </Stack>
                      <TextField
                        label="Condición al entrar"
                        value={checkinForm.ciConditionIn ?? ''}
                        onChange={(e) => setCheckinForm({ ...checkinForm, ciConditionIn: e.target.value })}
                        fullWidth
                        multiline
                        minRows={2}
                        sx={{ mt: 1 }}
                      />
                      <TextField
                        label="Notas"
                        value={checkinForm.ciNotes ?? ''}
                        onChange={(e) => setCheckinForm({ ...checkinForm, ciNotes: e.target.value })}
                        fullWidth
                        multiline
                        minRows={2}
                        sx={{ mt: 1 }}
                      />
                      <Button
                        variant="contained"
                        sx={{ mt: 2 }}
                        onClick={handleCheckinSubmit}
                        disabled={checkinMutation.isPending}
                      >
                        {checkinMutation.isPending ? 'Registrando…' : 'Check-in'}
                      </Button>
                      <Button variant="text" sx={{ mt: 1 }} onClick={() => void loadAsset()} disabled={checkinMutation.isPending}>
                        Refrescar estado
                      </Button>
                    </>
                  ) : (
                    <>
                      <Typography variant="subtitle1" sx={{ mt: 2 }}>Registrar check-out</Typography>
                      <Stack direction="row" spacing={1} flexWrap="wrap">
                        {['Cliente', 'Sala', 'Mantenimiento'].map((preset) => (
                          <Button
                            key={preset}
                            size="small"
                            variant="outlined"
                            onClick={() =>
                              setCheckoutForm((prev) => ({ ...prev, coTargetParty: preset }))
                            }
                          >
                            {preset}
                          </Button>
                        ))}
                      </Stack>
                      <TextField
                        label="Destino (party/ref)"
                        value={checkoutForm.coTargetParty ?? ''}
                        onChange={(e) => setCheckoutForm({ ...checkoutForm, coTargetParty: e.target.value })}
                        fullWidth
                        sx={{ mt: 1 }}
                      />
                      <TextField
                        label="Notas"
                        value={checkoutForm.coNotes ?? ''}
                        onChange={(e) => setCheckoutForm({ ...checkoutForm, coNotes: e.target.value })}
                        fullWidth
                        multiline
                        minRows={2}
                        sx={{ mt: 1 }}
                      />
                      <Button
                        variant="contained"
                        sx={{ mt: 2 }}
                        onClick={handleCheckoutSubmit}
                        disabled={checkoutMutation.isPending}
                      >
                        {checkoutMutation.isPending ? 'Registrando…' : 'Check-out'}
                      </Button>
                      <Button variant="text" sx={{ mt: 1 }} onClick={() => void loadAsset()} disabled={checkoutMutation.isPending}>
                        Refrescar estado
                      </Button>
                    </>
                  )}
                  <Stack spacing={1.5} sx={{ mt: 2 }}>
                    <Typography variant="subtitle1">¿Escanear otro equipo?</Typography>
                    <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                      <TextField
                        label="Código QR"
                        placeholder="Token del siguiente equipo"
                        value={nextToken}
                        onChange={(e) => setNextToken(e.target.value)}
                        fullWidth
                      />
                      <Button
                        variant="outlined"
                        onClick={() => {
                          const target = nextToken.trim();
                          if (!target) return;
                          navigate(`/inventario/scan/${target}`);
                        }}
                        disabled={!nextToken.trim()}
                      >
                        Abrir
                      </Button>
                    </Stack>
                  </Stack>
                </Stack>
              </CardContent>
            </Card>
          ) : (
            <Typography>Cargando equipo…</Typography>
          )}
        </Stack>
      </Container>
    </Box>
  );
}
