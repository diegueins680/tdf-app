import { useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  IconButton,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  Tooltip,
  Typography,
} from '@mui/material';
import QrCodeIcon from '@mui/icons-material/QrCode';
import ExitToAppIcon from '@mui/icons-material/ExitToApp';
import HowToRegIcon from '@mui/icons-material/HowToReg';
import RefreshIcon from '@mui/icons-material/Refresh';
import HistoryIcon from '@mui/icons-material/History';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import type { AssetDTO, AssetCheckoutDTO, PartyDTO, RoomDTO } from '../api/types';
import { Inventory, type AssetCheckoutRequest, type AssetCheckinRequest, type AssetQrDTO } from '../api/inventory';
import { CheckoutDialog, CheckinDialog } from '../components/AssetDialogs';
import { Rooms } from '../api/rooms';
import { Parties } from '../api/parties';
import { buildInventoryScanUrl } from '../config/appConfig';

const toLocalDateTime = (date: Date) => {
  const pad = (val: number) => String(val).padStart(2, '0');
  return `${date.getFullYear()}-${pad(date.getMonth() + 1)}-${pad(date.getDate())}T${pad(date.getHours())}:${pad(
    date.getMinutes(),
  )}`;
};

function normalizeAssets(payload: { items: AssetDTO[] } | AssetDTO[]): AssetDTO[] {
  if (Array.isArray(payload)) return payload;
  return payload.items ?? [];
}

function getInventoryMovementState(status: string) {
  const normalizedStatus = status.trim().toLowerCase();

  return {
    canCheckout: normalizedStatus === 'active',
    canCheckin: normalizedStatus === 'booked',
  };
}

function getInventoryStatusLabel(status: string) {
  const normalizedStatus = status.trim().toLowerCase();

  if (normalizedStatus === 'active') return 'Disponible';
  if (normalizedStatus === 'booked') return 'Prestado';
  if (normalizedStatus === 'retired') return 'Retirado';
  return status.trim() || 'Estado desconocido';
}

function normalizeInventoryField(value?: string | null) {
  const trimmed = value?.trim();
  return trimmed ? trimmed : null;
}

export default function InventoryPage() {
  const qc = useQueryClient();
  const assetsQuery = useQuery({
    queryKey: ['assets'],
    queryFn: () => Inventory.list().then(normalizeAssets),
  });
  const roomsQuery = useQuery({
    queryKey: ['rooms'],
    queryFn: Rooms.list,
    staleTime: 5 * 60 * 1000,
  });
  const partiesQuery = useQuery({
    queryKey: ['parties', 'all'],
    queryFn: () => Parties.list(),
    staleTime: 5 * 60 * 1000,
  });
  const [selected, setSelected] = useState<AssetDTO | null>(null);
  const [qrDataUrl, setQrDataUrl] = useState<string | null>(null);
  const [history, setHistory] = useState<AssetCheckoutDTO[]>([]);
  const [historyViewMode, setHistoryViewMode] = useState<'panel' | 'embedded' | null>(null);
  const [currentCheckout, setCurrentCheckout] = useState<AssetCheckoutDTO | null>(null);
  const [dialogOpen, setDialogOpen] = useState<'checkout' | 'checkin' | 'qr' | null>(null);
  const [form, setForm] = useState<AssetCheckoutRequest>({
    coTargetKind: 'party',
    coTargetParty: '',
    coTargetRoom: '',
    coTargetSession: '',
    coConditionOut: '',
    coNotes: '',
  });
  const [checkinForm, setCheckinForm] = useState<AssetCheckinRequest>({
    ciConditionIn: '',
    ciNotes: '',
  });
  const [feedback, setFeedback] = useState<string | null>(null);

  const assetHistoryMutation = useMutation({
    mutationFn: (assetId: string) => Inventory.history(assetId),
    onSuccess: (data) => {
      setHistory(data);
      const active = data.find((entry) => !entry.returnedAt);
      setCurrentCheckout(active ?? null);
    },
  });

  const qrMutation = useMutation({
    mutationFn: (assetId: string) => Inventory.generateQr(assetId),
    onSuccess: (data: AssetQrDTO) => {
      const url = data.qrUrl;
      const qrImg = `https://api.qrserver.com/v1/create-qr-code/?size=320x320&data=${encodeURIComponent(url)}`;
      setQrDataUrl(qrImg);
      setDialogOpen('qr');
    },
  });

  const checkoutMutation = useMutation({
    mutationFn: ({ assetId, payload }: { assetId: string; payload: AssetCheckoutRequest }) =>
      Inventory.checkout(assetId, payload),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['assets'] });
      setFeedback('Equipo marcado como check-out.');
      setDialogOpen(null);
    },
    onError: (err) => setFeedback(err instanceof Error ? err.message : 'No se pudo registrar el check-out.'),
  });

  const checkinMutation = useMutation({
    mutationFn: ({ assetId, payload }: { assetId: string; payload: AssetCheckinRequest }) =>
      Inventory.checkin(assetId, payload),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['assets'] });
      setFeedback('Equipo marcado como devuelto.');
      setDialogOpen(null);
    },
    onError: (err) => setFeedback(err instanceof Error ? err.message : 'No se pudo registrar el check-in.'),
  });

  const openCheckout = (asset: AssetDTO) => {
    setHistoryViewMode('embedded');
    setCurrentCheckout(null);
    setHistory([]);
    assetHistoryMutation.mutate(asset.assetId);
    const defaultDue = new Date();
    defaultDue.setHours(defaultDue.getHours() + 48);
    setForm({
      coTargetKind: 'party',
      coTargetParty: '',
      coTargetRoom: '',
      coTargetSession: '',
      coConditionOut: '',
      coNotes: '',
      coDueAt: toLocalDateTime(defaultDue),
    });
    setSelected(asset);
    setDialogOpen('checkout');
  };

  const openCheckin = (asset: AssetDTO) => {
    setHistoryViewMode(null);
    setSelected(asset);
    setDialogOpen('checkin');
  };

  const openQr = (asset: AssetDTO) => {
    setHistoryViewMode(null);
    setSelected(asset);
    if (asset.qrToken) {
      const url = buildInventoryScanUrl(asset.qrToken);
      const qrImg = `https://api.qrserver.com/v1/create-qr-code/?size=320x320&data=${encodeURIComponent(url)}`;
      setQrDataUrl(qrImg);
      setDialogOpen('qr');
    } else if (asset.assetId) {
      qrMutation.mutate(asset.assetId);
    }
  };

  const openHistory = (asset: AssetDTO) => {
    setHistoryViewMode('panel');
    setHistory([]);
    setCurrentCheckout(null);
    setSelected(asset);
    assetHistoryMutation.mutate(asset.assetId);
  };

  const closeHistoryPanel = () => {
    setHistoryViewMode(null);
    setHistory([]);
    setCurrentCheckout(null);
  };

  const assets = useMemo(() => assetsQuery.data ?? [], [assetsQuery.data]);
  const grouped = useMemo(() => assets, [assets]);
  const roomOptions = useMemo<RoomDTO[]>(() => roomsQuery.data ?? [], [roomsQuery.data]);
  const partyOptions = useMemo<PartyDTO[]>(() => partiesQuery.data ?? [], [partiesQuery.data]);
  const singleAsset = grouped.length === 1 ? (grouped[0] ?? null) : null;
  const showFirstAssetEmptyState = !assetsQuery.isLoading && !assetsQuery.error && grouped.length === 0;
  const showSingleAssetSummary = !assetsQuery.isLoading && !assetsQuery.error && singleAsset != null;
  const singleAssetLocation = singleAsset ? normalizeInventoryField(singleAsset.location) : null;
  const singleAssetCondition = singleAsset ? normalizeInventoryField(singleAsset.condition) : null;
  const showLocationColumn = grouped.some((asset) => normalizeInventoryField(asset.location) != null);
  const showLocationSetupGuidance = grouped.length > 1 && !showLocationColumn;

  return (
    <Box sx={{ color: '#e2e8f0' }}>
      <Stack direction="row" justifyContent="space-between" alignItems="center" mb={2}>
        <Box>
          <Typography variant="h5" fontWeight={800}>
            Inventario de equipo
          </Typography>
          <Typography variant="body2" color="rgba(226,232,240,0.75)">
            Administra equipos, genera QR y registra check-out / check-in.
          </Typography>
        </Box>
        <Stack direction="row" spacing={1}>
          <Button variant="outlined" startIcon={<RefreshIcon />} onClick={() => void qc.invalidateQueries({ queryKey: ['assets'] })}>
            Actualizar
          </Button>
        </Stack>
      </Stack>

      {feedback && <Alert severity="info" sx={{ mb: 2 }} onClose={() => setFeedback(null)}>{feedback}</Alert>}
      {assetsQuery.isLoading && <Typography>Cargando inventario…</Typography>}
      {assetsQuery.error && <Alert severity="error">No se pudo cargar inventario.</Alert>}

      {showFirstAssetEmptyState ? (
        <Card sx={{ bgcolor: 'rgba(255,255,255,0.02)', border: '1px solid rgba(255,255,255,0.08)' }}>
          <CardContent>
            <Stack spacing={1}>
              <Typography variant="h6" fontWeight={700}>
                Primeros pasos
              </Typography>
              <Typography variant="body2" color="rgba(226,232,240,0.78)">
                Todavía no hay equipos registrados. Cuando exista el primero, aquí verás estado, ubicación, QR e historial
                para operar check-out y check-in desde una sola fila.
              </Typography>
              <Typography variant="body2" color="rgba(226,232,240,0.68)">
                Si estás esperando la carga inicial del inventario, usa Actualizar para volver a consultar sin revisar una tabla vacía.
              </Typography>
            </Stack>
          </CardContent>
        </Card>
      ) : showSingleAssetSummary && singleAsset ? (
        <Card sx={{ bgcolor: 'rgba(255,255,255,0.02)', border: '1px solid rgba(255,255,255,0.08)' }}>
          <CardContent>
            <Stack spacing={2}>
              <Stack spacing={0.75}>
                <Typography variant="h6" fontWeight={700}>
                  Primer equipo registrado
                </Typography>
                <Typography variant="body2" color="rgba(226,232,240,0.78)">
                  Revisa estado, ubicación y el siguiente movimiento desde este resumen. Cuando exista el segundo
                  equipo, volverá la tabla operativa.
                </Typography>
              </Stack>
              <Stack spacing={0.5}>
                <Typography variant="body1" fontWeight={700}>
                  {singleAsset.name}
                </Typography>
                <Typography variant="body2" color="rgba(226,232,240,0.78)">
                  Categoría: {singleAsset.category}
                </Typography>
                <Typography variant="body2" color="rgba(226,232,240,0.78)">
                  Estado: {getInventoryStatusLabel(singleAsset.status)}
                </Typography>
                {singleAssetLocation && (
                  <Typography variant="body2" color="rgba(226,232,240,0.78)">
                    Ubicación: {singleAssetLocation}
                  </Typography>
                )}
                {singleAssetCondition && (
                  <Typography variant="body2" color="rgba(226,232,240,0.78)">
                    Condición: {singleAssetCondition}
                  </Typography>
                )}
              </Stack>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                <Button
                  size="small"
                  variant="outlined"
                  startIcon={<QrCodeIcon />}
                  onClick={() => void openQr(singleAsset)}
                  aria-label={`Abrir QR de ${singleAsset.name}`}
                >
                  Ver QR
                </Button>
                {getInventoryMovementState(singleAsset.status).canCheckout && (
                  <Button
                    size="small"
                    variant="contained"
                    startIcon={<ExitToAppIcon />}
                    onClick={() => openCheckout(singleAsset)}
                    aria-label={`Abrir check-out de ${singleAsset.name}`}
                  >
                    Registrar check-out
                  </Button>
                )}
                {getInventoryMovementState(singleAsset.status).canCheckin && (
                  <Button
                    size="small"
                    variant="contained"
                    startIcon={<HowToRegIcon />}
                    onClick={() => openCheckin(singleAsset)}
                    aria-label={`Abrir check-in de ${singleAsset.name}`}
                  >
                    Registrar check-in
                  </Button>
                )}
                <Button size="small" variant="text" onClick={() => openHistory(singleAsset)}>
                  Historial
                </Button>
              </Stack>
            </Stack>
          </CardContent>
        </Card>
      ) : (
        <Card sx={{ bgcolor: 'rgba(255,255,255,0.02)', border: '1px solid rgba(255,255,255,0.08)' }}>
          <CardContent>
            <Stack spacing={1.5}>
              {showLocationSetupGuidance && (
                <Typography variant="body2" color="rgba(226,232,240,0.68)">
                  La ubicación aparecerá en la tabla cuando al menos un equipo tenga una ubicación registrada.
                </Typography>
              )}
              <Table size="small">
                <TableHead>
                  <TableRow>
                    <TableCell>Equipo</TableCell>
                    <TableCell>Estado</TableCell>
                    {showLocationColumn && <TableCell>Ubicación</TableCell>}
                    <TableCell align="right">Acciones</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {grouped.map((asset) => {
                    const movementState = getInventoryMovementState(asset.status);

                    return (
                      <TableRow key={asset.assetId} hover>
                        <TableCell>
                          <Stack spacing={0.25}>
                            <Typography variant="body2" fontWeight={700}>
                              {asset.name}
                            </Typography>
                            <Typography variant="caption" color="text.secondary">
                              {asset.category}
                            </Typography>
                            <Typography variant="caption" color="text.secondary">
                              Condición: {asset.condition ?? '—'}
                            </Typography>
                          </Stack>
                        </TableCell>
                        <TableCell>{getInventoryStatusLabel(asset.status)}</TableCell>
                        {showLocationColumn && <TableCell>{normalizeInventoryField(asset.location) ?? '—'}</TableCell>}
                        <TableCell align="right">
                          <IconButton size="small" onClick={() => void openQr(asset)} title="QR" aria-label={`Abrir QR de ${asset.name}`}>
                            <QrCodeIcon fontSize="small" />
                          </IconButton>
                          {movementState.canCheckout && (
                            <IconButton
                              size="small"
                              onClick={() => openCheckout(asset)}
                              title="Check-out"
                              aria-label={`Abrir check-out de ${asset.name}`}
                            >
                              <ExitToAppIcon fontSize="small" />
                            </IconButton>
                          )}
                          {movementState.canCheckin && (
                            <IconButton
                              size="small"
                              onClick={() => openCheckin(asset)}
                              title="Check-in"
                              aria-label={`Abrir check-in de ${asset.name}`}
                            >
                              <HowToRegIcon fontSize="small" />
                            </IconButton>
                          )}
                          <Tooltip title="Historial">
                            <IconButton
                              size="small"
                              onClick={() => openHistory(asset)}
                              aria-label={`Abrir historial de ${asset.name}`}
                            >
                              <HistoryIcon fontSize="small" />
                            </IconButton>
                          </Tooltip>
                        </TableCell>
                      </TableRow>
                    );
                  })}
                </TableBody>
              </Table>
            </Stack>
          </CardContent>
        </Card>
      )}

      <CheckoutDialog
        open={dialogOpen === 'checkout'}
        onClose={() => setDialogOpen(null)}
        asset={selected}
        form={form}
        onFormChange={setForm}
        onSubmit={() => selected && checkoutMutation.mutate({ assetId: selected.assetId, payload: form })}
        loading={checkoutMutation.isPending}
        roomOptions={roomOptions}
        partyOptions={partyOptions}
        loadingRooms={roomsQuery.isLoading}
        loadingParties={partiesQuery.isLoading}
        currentCheckout={currentCheckout}
        recentHistory={history}
      />

      <CheckinDialog
        open={dialogOpen === 'checkin'}
        onClose={() => setDialogOpen(null)}
        asset={selected}
        form={checkinForm}
        onFormChange={setCheckinForm}
        onSubmit={() => selected && checkinMutation.mutate({ assetId: selected.assetId, payload: checkinForm })}
        loading={checkinMutation.isPending}
      />

      <Dialog open={dialogOpen === 'qr'} onClose={() => setDialogOpen(null)} maxWidth="sm" fullWidth>
        <DialogTitle>QR de equipo</DialogTitle>
        <DialogContent sx={{ textAlign: 'center' }}>
          {qrDataUrl ? (
            <img src={qrDataUrl} alt="QR" style={{ width: '100%', maxWidth: 320 }} />
          ) : (
            <Typography variant="body2">Generando QR…</Typography>
          )}
          {selected?.qrToken && (
            <Typography variant="body2" sx={{ mt: 1 }}>
              Token: {selected.qrToken}
            </Typography>
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setDialogOpen(null)}>Cerrar</Button>
        </DialogActions>
      </Dialog>

      {historyViewMode === 'panel' && selected && (
        <Card sx={{ mt: 3, bgcolor: 'rgba(255,255,255,0.02)', border: '1px solid rgba(255,255,255,0.08)' }}>
          <CardContent>
            <Stack
              direction={{ xs: 'column', sm: 'row' }}
              justifyContent="space-between"
              alignItems={{ xs: 'flex-start', sm: 'center' }}
              spacing={1}
              sx={{ mb: 1.5 }}
            >
              <Typography variant="h6">
                Historial · {selected.name}
              </Typography>
              <Button size="small" variant="text" onClick={closeHistoryPanel}>
                Ocultar historial
              </Button>
            </Stack>
            {assetHistoryMutation.isPending ? (
              <Typography variant="body2" color="text.secondary">
                Cargando historial…
              </Typography>
            ) : history.length > 0 ? (
              <Table size="small">
                <TableHead>
                  <TableRow>
                    <TableCell>Salida</TableCell>
                    <TableCell>Devuelto</TableCell>
                    <TableCell>Destino</TableCell>
                    <TableCell>Notas</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {history.map((h) => (
                    <TableRow key={h.checkoutId}>
                      <TableCell>{formatDate(h.checkedOutAt)}</TableCell>
                      <TableCell>{h.returnedAt ? formatDate(h.returnedAt) : '—'}</TableCell>
                      <TableCell>{h.targetKind}</TableCell>
                      <TableCell>{h.notes ?? '—'}</TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            ) : (
              <Alert severity="info">
                Todavía no hay movimientos registrados para este equipo. Cuando ocurra el primero, aquí verás salida,
                devolución, destino y notas.
              </Alert>
            )}
          </CardContent>
        </Card>
      )}
    </Box>
  );
}

function formatDate(value: string) {
  const d = new Date(value);
  if (Number.isNaN(d.getTime())) return value;
  return d.toLocaleString();
}
