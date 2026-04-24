import { useCallback, useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Container,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useNavigate, useParams } from 'react-router-dom';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import type { AssetDTO } from '../api/types';
import {
  InventoryPublic,
  type AssetCheckinRequest,
  type AssetCheckoutRequest,
} from '../api/inventory';
import {
  CHECKOUT_DISPOSITION_OPTIONS,
  CHECKOUT_PAYMENT_TYPE_OPTIONS,
  checkoutSupportsPaymentDetails,
  checkoutSupportsReturnDate,
  formatCheckoutPaymentSummary,
  getCheckoutDispositionLabel,
} from '../utils/inventoryCheckout';

const REFRESH_STATE_HELP_TEXT =
  'Si otro operador ya movió este equipo desde otra pantalla, usa Refrescar estado para confirmar qué acción sigue aquí.';

function formatDate(value?: string | null) {
  if (!value) return '—';
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return value;
  return date.toLocaleString();
}

export default function InventoryScanPage() {
  const { token } = useParams();
  const qc = useQueryClient();
  const navigate = useNavigate();
  const [asset, setAsset] = useState<AssetDTO | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [feedback, setFeedback] = useState<string | null>(null);
  const [formError, setFormError] = useState<string | null>(null);
  const [nextToken, setNextToken] = useState('');
  const [checkoutForm, setCheckoutForm] = useState<AssetCheckoutRequest>({
    coTargetKind: 'party',
    coTargetParty: '',
    coDisposition: 'loan',
    coTermsAndConditions: '',
    coHolderEmail: '',
    coHolderPhone: '',
    coPaymentType: '',
    coPaymentInstallments: null,
    coPaymentReference: '',
    coConditionOut: '',
    coPhotoUrl: '',
    coNotes: '',
  });
  const [checkinForm, setCheckinForm] = useState<AssetCheckinRequest>({
    ciConditionIn: '',
    ciNotes: '',
    ciPhotoUrl: '',
  });
  const [photoUploadPending, setPhotoUploadPending] = useState<'checkout' | 'checkin' | null>(null);

  const loadAsset = useCallback(async () => {
    if (!token) return;
    try {
      const data = await InventoryPublic.byQrToken(token);
      setAsset(data);
      setError(null);
    } catch (err) {
      setAsset(null);
      setError(err instanceof Error ? err.message : 'No pudimos cargar el equipo.');
    }
  }, [token]);

  useEffect(() => {
    void loadAsset();
  }, [loadAsset]);

  const checkoutMutation = useMutation({
    mutationFn: () =>
      token ? InventoryPublic.checkout(token, checkoutForm) : Promise.reject(new Error('Missing token')),
    onSuccess: async () => {
      void qc.invalidateQueries({ queryKey: ['assets'] });
      await loadAsset();
      setFeedback('Salida registrada correctamente.');
      setFormError(null);
    },
    onError: (err) => setError(err instanceof Error ? err.message : 'No pudimos registrar la salida.'),
  });

  const checkinMutation = useMutation({
    mutationFn: () =>
      token ? InventoryPublic.checkin(token, checkinForm) : Promise.reject(new Error('Missing token')),
    onSuccess: async () => {
      void qc.invalidateQueries({ queryKey: ['assets'] });
      await loadAsset();
      setFeedback('Retorno registrado correctamente.');
      setFormError(null);
    },
    onError: (err) => setError(err instanceof Error ? err.message : 'No pudimos registrar el retorno.'),
  });

  const uploadPhoto = async (mode: 'checkout' | 'checkin', file: File) => {
    if (!token) return;
    setPhotoUploadPending(mode);
    try {
      const uploaded = await InventoryPublic.uploadPhoto(token, file, { name: file.name });
      if (mode === 'checkout') {
        setCheckoutForm((prev) => ({ ...prev, coPhotoUrl: uploaded.publicUrl || uploaded.webContentLink || uploaded.id }));
      } else {
        setCheckinForm((prev) => ({ ...prev, ciPhotoUrl: uploaded.publicUrl || uploaded.webContentLink || uploaded.id }));
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : 'No se pudo subir la foto.');
    } finally {
      setPhotoUploadPending(null);
    }
  };

  const normalizedStatus = asset?.status?.trim().toLowerCase() ?? '';
  const isCheckedOut = normalizedStatus === 'booked';
  const isRetired = normalizedStatus === 'retired';
  const isUnavailable = normalizedStatus.includes('maintenance');
  const isMovementPending = checkoutMutation.isPending || checkinMutation.isPending;
  const isSale = asset?.currentCheckoutDisposition?.trim().toLowerCase() === 'sale';

  const currentHolderSummary = useMemo(() => {
    if (!asset?.currentCheckoutTarget) return null;
    return asset.currentCheckoutTarget;
  }, [asset?.currentCheckoutTarget]);
  const supportsReturnDate = checkoutSupportsReturnDate(checkoutForm.coDisposition);
  const supportsPaymentDetails = checkoutSupportsPaymentDetails(checkoutForm.coDisposition);

  const handleCheckoutSubmit = () => {
    const holderName = checkoutForm.coTargetParty?.trim() ?? '';
    if (!holderName) {
      setFormError('Escribe el nombre de la persona o proyecto que se lleva el equipo.');
      return;
    }
    if (!(checkoutForm.coPhotoUrl?.trim())) {
      setFormError('Sube una foto del estado del equipo al salir.');
      return;
    }
    setFormError(null);
    checkoutMutation.mutate();
  };

  const handleCheckinSubmit = () => {
    const condition = checkinForm.ciConditionIn?.trim() ?? '';
    if (!condition) {
      setFormError('Describe la condición al recibir o devolver el equipo.');
      return;
    }
    setFormError(null);
    checkinMutation.mutate();
  };

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
                  Escanea el QR físico o pega el enlace que te compartió el equipo de TDF.
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
          <Typography variant="h5" fontWeight={800}>
            Salida y retorno de equipo
          </Typography>
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
                <Stack spacing={1.5}>
                  <Stack spacing={0.75}>
                    <Typography variant="h6">{asset.name}</Typography>
                    <Stack direction="row" spacing={1} alignItems="center" useFlexGap flexWrap="wrap">
                      <Chip label={asset.category} size="small" />
                      <Chip label={`Estado: ${asset.status}`} size="small" />
                      {asset.currentCheckoutDisposition && (
                        <Chip label={getCheckoutDispositionLabel(asset.currentCheckoutDisposition)} size="small" variant="outlined" />
                      )}
                    </Stack>
                    <Typography variant="body2">Condición actual: {asset.condition ?? '—'}</Typography>
                    <Typography variant="body2">Ubicación base: {asset.location ?? '—'}</Typography>
                  </Stack>

                  {asset.photoUrl && (
                    <img
                      src={asset.photoUrl}
                      alt={`Foto de ${asset.name}`}
                      style={{ width: '100%', maxHeight: 240, objectFit: 'cover', borderRadius: 12 }}
                    />
                  )}

                  {asset.currentCheckoutPhotoUrl && (
                    <Stack spacing={0.5}>
                      <Typography variant="subtitle2">Último estado registrado al salir</Typography>
                      <img
                        src={asset.currentCheckoutPhotoUrl}
                        alt={`Último estado registrado de ${asset.name}`}
                        style={{ width: '100%', maxHeight: 240, objectFit: 'cover', borderRadius: 12 }}
                      />
                    </Stack>
                  )}

                  <Stack
                    direction={{ xs: 'column', sm: 'row' }}
                    spacing={1}
                    alignItems={{ xs: 'flex-start', sm: 'center' }}
                  >
                    <Button variant="text" onClick={() => void loadAsset()} disabled={isMovementPending}>
                      Refrescar estado
                    </Button>
                    <Typography variant="caption" color="rgba(226,232,240,0.68)">
                      {REFRESH_STATE_HELP_TEXT}
                    </Typography>
                  </Stack>

                  {currentHolderSummary && (
                    <Alert severity="info">
                      Actualmente lo tiene: <strong>{currentHolderSummary}</strong>.{' '}
                      {asset.currentCheckoutAt ? `Salió: ${formatDate(asset.currentCheckoutAt)}.` : ''}
                      {' '}
                      {asset.currentCheckoutDueAt ? `Devuelve: ${formatDate(asset.currentCheckoutDueAt)}.` : 'Sin fecha de devolución.'}
                      {formatCheckoutPaymentSummary(
                        asset.currentCheckoutPaymentType,
                        asset.currentCheckoutPaymentInstallments,
                      )
                        ? ` Pago: ${formatCheckoutPaymentSummary(
                            asset.currentCheckoutPaymentType,
                            asset.currentCheckoutPaymentInstallments,
                          )}.`
                        : ''}
                    </Alert>
                  )}

                  {isUnavailable ? (
                    <Alert severity="warning">
                      Este equipo está marcado como {asset.status}. No se permiten movimientos mientras esté fuera de servicio.
                    </Alert>
                  ) : isRetired && isSale ? (
                    <Alert severity="info">
                      Este equipo se registró como vendido. El enlace queda solo como consulta histórica.
                    </Alert>
                  ) : isRetired ? (
                    <Alert severity="info">
                      Este equipo está retirado del inventario operativo. El enlace queda solo como consulta histórica.
                    </Alert>
                  ) : isCheckedOut ? (
                    <Stack spacing={1.5}>
                      <Typography variant="subtitle1" sx={{ mt: 1 }}>
                        Registrar retorno
                      </Typography>
                      <TextField
                        label="Condición al regresar"
                        value={checkinForm.ciConditionIn ?? ''}
                        onChange={(e) => setCheckinForm((prev) => ({ ...prev, ciConditionIn: e.target.value }))}
                        fullWidth
                        multiline
                        minRows={2}
                      />
                      <TextField
                        label="Notas"
                        value={checkinForm.ciNotes ?? ''}
                        onChange={(e) => setCheckinForm((prev) => ({ ...prev, ciNotes: e.target.value }))}
                        fullWidth
                        multiline
                        minRows={2}
                      />
                      <Button component="label" variant="outlined" disabled={photoUploadPending === 'checkin'}>
                        {photoUploadPending === 'checkin'
                          ? 'Subiendo foto…'
                          : checkinForm.ciPhotoUrl
                            ? 'Reemplazar foto de retorno'
                            : 'Subir foto de retorno'}
                        <input
                          hidden
                          type="file"
                          accept="image/*"
                          capture="environment"
                          onChange={(event) => {
                            const file = event.target.files?.[0];
                            if (file) void uploadPhoto('checkin', file);
                            event.currentTarget.value = '';
                          }}
                        />
                      </Button>
                      {checkinForm.ciPhotoUrl && (
                        <img
                          src={checkinForm.ciPhotoUrl}
                          alt="Estado al regresar"
                          style={{ width: '100%', maxHeight: 240, objectFit: 'cover', borderRadius: 12 }}
                        />
                      )}
                      <Button
                        variant="contained"
                        onClick={handleCheckinSubmit}
                        disabled={checkinMutation.isPending}
                      >
                        {checkinMutation.isPending ? 'Registrando…' : 'Registrar retorno'}
                      </Button>
                    </Stack>
                  ) : (
                    <Stack spacing={1.5}>
                      <Typography variant="subtitle1" sx={{ mt: 1 }}>
                        Registrar salida
                      </Typography>
                      <TextField
                        label="Nombre de quien se lleva el equipo"
                        value={checkoutForm.coTargetParty ?? ''}
                        onChange={(e) =>
                          setCheckoutForm((prev) => ({ ...prev, coTargetParty: e.target.value, coTargetKind: 'party' }))
                        }
                        fullWidth
                      />
                      <TextField
                        label="Tipo de movimiento"
                        select
                        value={checkoutForm.coDisposition ?? 'loan'}
                        onChange={(e) =>
                          setCheckoutForm((prev) => ({
                            ...prev,
                            coDisposition: e.target.value,
                            coDueAt: checkoutSupportsReturnDate(e.target.value) ? (prev.coDueAt ?? '') : '',
                            coPaymentType: checkoutSupportsPaymentDetails(e.target.value) ? (prev.coPaymentType ?? '') : '',
                            coPaymentInstallments: checkoutSupportsPaymentDetails(e.target.value)
                              ? (prev.coPaymentInstallments ?? null)
                              : null,
                            coPaymentReference: checkoutSupportsPaymentDetails(e.target.value)
                              ? (prev.coPaymentReference ?? '')
                              : '',
                          }))
                        }
                        fullWidth
                      >
                        {CHECKOUT_DISPOSITION_OPTIONS.map((option) => (
                          <MenuItem key={option.value} value={option.value}>
                            {option.label}
                          </MenuItem>
                        ))}
                      </TextField>
                      <TextField
                        label="Correo"
                        type="email"
                        value={checkoutForm.coHolderEmail ?? ''}
                        onChange={(e) => setCheckoutForm((prev) => ({ ...prev, coHolderEmail: e.target.value }))}
                        fullWidth
                      />
                      <TextField
                        label="Teléfono"
                        value={checkoutForm.coHolderPhone ?? ''}
                        onChange={(e) => setCheckoutForm((prev) => ({ ...prev, coHolderPhone: e.target.value }))}
                        fullWidth
                      />
                      {supportsReturnDate && (
                        <TextField
                          label="Fecha pactada de retorno"
                          type="datetime-local"
                          value={checkoutForm.coDueAt ?? ''}
                          onChange={(e) => setCheckoutForm((prev) => ({ ...prev, coDueAt: e.target.value }))}
                          fullWidth
                          InputLabelProps={{ shrink: true }}
                        />
                      )}
                      <TextField
                        label="Términos y condiciones"
                        value={checkoutForm.coTermsAndConditions ?? ''}
                        onChange={(e) => setCheckoutForm((prev) => ({ ...prev, coTermsAndConditions: e.target.value }))}
                        fullWidth
                        multiline
                        minRows={3}
                      />
                      {supportsPaymentDetails && (
                        <>
                          <TextField
                            label="Tipo de pago"
                            select
                            value={checkoutForm.coPaymentType ?? ''}
                            onChange={(e) => setCheckoutForm((prev) => ({ ...prev, coPaymentType: e.target.value }))}
                            fullWidth
                          >
                            <MenuItem value="">Sin registrar</MenuItem>
                            {CHECKOUT_PAYMENT_TYPE_OPTIONS.map((option) => (
                              <MenuItem key={option.value} value={option.value}>
                                {option.label}
                              </MenuItem>
                            ))}
                          </TextField>
                          <TextField
                            label="Número de cuotas"
                            type="number"
                            value={checkoutForm.coPaymentInstallments ?? ''}
                            onChange={(e) =>
                              setCheckoutForm((prev) => ({
                                ...prev,
                                coPaymentInstallments: e.target.value === '' ? null : Number(e.target.value),
                              }))
                            }
                            fullWidth
                            inputProps={{ min: 1, max: 60, step: 1 }}
                          />
                          <TextField
                            label="Referencia de pago"
                            value={checkoutForm.coPaymentReference ?? ''}
                            onChange={(e) => setCheckoutForm((prev) => ({ ...prev, coPaymentReference: e.target.value }))}
                            fullWidth
                          />
                        </>
                      )}
                      <TextField
                        label="Condición al salir"
                        value={checkoutForm.coConditionOut ?? ''}
                        onChange={(e) => setCheckoutForm((prev) => ({ ...prev, coConditionOut: e.target.value }))}
                        fullWidth
                        multiline
                        minRows={2}
                      />
                      <TextField
                        label="Notas"
                        value={checkoutForm.coNotes ?? ''}
                        onChange={(e) => setCheckoutForm((prev) => ({ ...prev, coNotes: e.target.value }))}
                        fullWidth
                        multiline
                        minRows={2}
                      />
                      <Button component="label" variant="outlined" disabled={photoUploadPending === 'checkout'}>
                        {photoUploadPending === 'checkout'
                          ? 'Subiendo foto…'
                          : checkoutForm.coPhotoUrl
                            ? 'Reemplazar foto del estado al salir'
                            : 'Subir foto del estado al salir'}
                        <input
                          hidden
                          type="file"
                          accept="image/*"
                          capture="environment"
                          onChange={(event) => {
                            const file = event.target.files?.[0];
                            if (file) void uploadPhoto('checkout', file);
                            event.currentTarget.value = '';
                          }}
                        />
                      </Button>
                      {checkoutForm.coPhotoUrl && (
                        <img
                          src={checkoutForm.coPhotoUrl}
                          alt="Estado al salir"
                          style={{ width: '100%', maxHeight: 240, objectFit: 'cover', borderRadius: 12 }}
                        />
                      )}
                      <Button
                        variant="contained"
                        onClick={handleCheckoutSubmit}
                        disabled={checkoutMutation.isPending}
                      >
                        {checkoutMutation.isPending ? 'Registrando…' : 'Registrar salida'}
                      </Button>
                    </Stack>
                  )}

                  <Stack spacing={1.5} sx={{ mt: 1 }}>
                    <Typography variant="subtitle1">¿Abrir otro equipo?</Typography>
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
