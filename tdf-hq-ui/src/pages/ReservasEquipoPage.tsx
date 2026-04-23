import { useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Grid,
  Stack,
  Typography,
} from '@mui/material';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import InventoryIcon from '@mui/icons-material/Inventory';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import type { AssetDTO, RoomDTO } from '../api/types';
import { Inventory } from '../api/inventory';
import { Rooms } from '../api/rooms';
import { buildInventoryScanUrl } from '../config/appConfig';
import {
  formatCheckoutTargetDisplay,
  getCheckoutDispositionLabel,
} from '../utils/inventoryCheckout';

function normalizeAssets(payload: { items: AssetDTO[] } | AssetDTO[]): AssetDTO[] {
  if (Array.isArray(payload)) return payload;
  return payload.items ?? [];
}

const statusChip = (status?: string | null) => {
  const lower = (status ?? '').toLowerCase();
  if (lower.includes('book')) return { color: 'warning' as const, icon: <EventAvailableIcon fontSize="small" />, label: 'Fuera' };
  if (lower.includes('maintenance') || lower.includes('mantenimiento')) {
    return { color: 'warning' as const, icon: <WarningAmberIcon fontSize="small" />, label: 'Mantenimiento' };
  }
  if (lower.includes('retired')) return { color: 'default' as const, icon: <InventoryIcon fontSize="small" />, label: 'Retirado' };
  return { color: 'success' as const, icon: <InventoryIcon fontSize="small" />, label: 'Disponible' };
};

function formatDate(value?: string | null) {
  if (!value) return '—';
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return value;
  return date.toLocaleString();
}

export default function ReservasEquipoPage() {
  const qc = useQueryClient();
  const assetsQuery = useQuery({
    queryKey: ['assets-reservas'],
    queryFn: () => Inventory.list({ pageSize: 200 }).then(normalizeAssets),
  });
  const roomsQuery = useQuery({
    queryKey: ['rooms', 'reservas'],
    queryFn: () => Rooms.list(),
    staleTime: 5 * 60 * 1000,
  });
  const [feedback, setFeedback] = useState<string | null>(null);

  const assets = useMemo(() => assetsQuery.data ?? [], [assetsQuery.data]);
  const roomOptions = useMemo<RoomDTO[]>(() => roomsQuery.data ?? [], [roomsQuery.data]);
  const roomMap = useMemo(() => new Map(roomOptions.map((room) => [room.roomId, room])), [roomOptions]);
  const checkedOutCount = assets.filter((asset) => Boolean(asset.currentCheckoutTarget)).length;
  const availableCount = assets.filter((asset) => !asset.currentCheckoutTarget && asset.status.toLowerCase() === 'active').length;
  const retiredCount = assets.filter((asset) => asset.status.toLowerCase() === 'retired').length;

  const ensureShareUrl = async (asset: AssetDTO) => {
    if (asset.qrToken) return buildInventoryScanUrl(asset.qrToken);
    const generated = await Inventory.generateQr(asset.assetId);
    void qc.invalidateQueries({ queryKey: ['assets-reservas'] });
    return generated.qrUrl;
  };

  const copyShareUrl = async (asset: AssetDTO) => {
    try {
      const url = await ensureShareUrl(asset);
      await navigator.clipboard.writeText(url);
      setFeedback(`Enlace público copiado para ${asset.name}.`);
    } catch (error) {
      setFeedback(error instanceof Error ? error.message : 'No se pudo copiar el enlace público.');
    }
  };

  const openShareUrl = async (asset: AssetDTO) => {
    try {
      const url = await ensureShareUrl(asset);
      window.open(url, '_blank', 'noopener,noreferrer');
    } catch (error) {
      setFeedback(error instanceof Error ? error.message : 'No se pudo abrir el enlace público.');
    }
  };

  return (
    <Box p={2}>
      <Stack spacing={2}>
        <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" alignItems={{ xs: 'flex-start', md: 'center' }} spacing={1}>
          <Stack direction="row" spacing={1} alignItems="center">
            <EventAvailableIcon color="primary" />
            <Box>
              <Typography variant="h4" fontWeight={800}>
                Reservas de equipo
              </Typography>
              <Typography variant="body2" color="text.secondary">
                Comparte el enlace público por activo para que el prestatario registre salida, responsable, fecha y foto.
              </Typography>
            </Box>
          </Stack>
          <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
            <Chip label={`Disponibles: ${availableCount}`} color="success" />
            <Chip label={`Fuera: ${checkedOutCount}`} color="warning" />
            <Chip label={`Retirados: ${retiredCount}`} />
          </Stack>
        </Stack>

        {feedback && (
          <Alert severity="info" onClose={() => setFeedback(null)}>
            {feedback}
          </Alert>
        )}
        {assetsQuery.isLoading && <Typography>Cargando equipo…</Typography>}
        {assetsQuery.error && <Alert severity="error">No se pudo cargar el control de reservas.</Alert>}

        <Card variant="outlined">
          <CardContent>
            <Stack spacing={1}>
              <Typography variant="h6" fontWeight={700}>
                Flujo recomendado
              </Typography>
              <Typography variant="body2" color="text.secondary">
                1. Copia el enlace del equipo.
              </Typography>
              <Typography variant="body2" color="text.secondary">
                2. Envíalo al usuario que se lo lleva.
              </Typography>
              <Typography variant="body2" color="text.secondary">
                3. El usuario registra nombre, contacto, tipo de movimiento y foto del estado del equipo al salir.
              </Typography>
            </Stack>
          </CardContent>
        </Card>

        <Grid container spacing={2}>
          {assets.map((asset) => {
            const chip = statusChip(asset.status);
            const currentTarget = formatCheckoutTargetDisplay(asset.currentCheckoutKind, asset.currentCheckoutTarget, roomMap);
            return (
              <Grid key={asset.assetId} item xs={12} md={6} lg={4}>
                <Card variant="outlined" sx={{ height: '100%' }}>
                  <CardContent>
                    <Stack spacing={1.25}>
                      <Stack spacing={0.5}>
                        <Typography variant="subtitle1" fontWeight={700}>
                          {asset.name}
                        </Typography>
                        <Typography variant="body2" color="text.secondary">
                          {asset.category}
                        </Typography>
                      </Stack>

                      {asset.currentCheckoutPhotoUrl && (
                        <img
                          src={asset.currentCheckoutPhotoUrl}
                          alt={`Estado actual de ${asset.name}`}
                          style={{ width: '100%', maxHeight: 180, objectFit: 'cover', borderRadius: 12 }}
                        />
                      )}

                      <Stack direction="row" spacing={1} alignItems="center" useFlexGap flexWrap="wrap">
                        <Chip size="small" label={chip.label} color={chip.color} icon={chip.icon} />
                        {asset.currentCheckoutDisposition && (
                          <Chip size="small" label={getCheckoutDispositionLabel(asset.currentCheckoutDisposition)} variant="outlined" />
                        )}
                        {asset.condition && <Chip size="small" label={asset.condition} variant="outlined" />}
                      </Stack>

                      {asset.currentCheckoutTarget ? (
                        <Stack spacing={0.5}>
                          <Typography variant="body2">
                            <strong>Tiene:</strong> {currentTarget}
                          </Typography>
                          <Typography variant="body2" color="text.secondary">
                            <strong>Salió:</strong> {formatDate(asset.currentCheckoutAt)}
                          </Typography>
                          <Typography variant="body2" color="text.secondary">
                            <strong>Devuelve:</strong> {formatDate(asset.currentCheckoutDueAt)}
                          </Typography>
                          {(asset.currentCheckoutHolderEmail || asset.currentCheckoutHolderPhone) && (
                            <Typography variant="body2" color="text.secondary">
                              {[asset.currentCheckoutHolderEmail, asset.currentCheckoutHolderPhone].filter(Boolean).join(' · ')}
                            </Typography>
                          )}
                        </Stack>
                      ) : (
                        <Typography variant="body2" color="text.secondary">
                          Sin salida activa. El enlace público está listo para el próximo préstamo o alquiler.
                        </Typography>
                      )}

                      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                        <Button variant="contained" size="small" onClick={() => void copyShareUrl(asset)}>
                          Copiar enlace
                        </Button>
                        <Button variant="outlined" size="small" onClick={() => void openShareUrl(asset)}>
                          Abrir formulario
                        </Button>
                      </Stack>
                    </Stack>
                  </CardContent>
                </Card>
              </Grid>
            );
          })}
        </Grid>
      </Stack>
    </Box>
  );
}
