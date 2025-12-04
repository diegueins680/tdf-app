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
  Typography,
} from '@mui/material';
import QrCodeIcon from '@mui/icons-material/QrCode';
import ExitToAppIcon from '@mui/icons-material/ExitToApp';
import HowToRegIcon from '@mui/icons-material/HowToReg';
import RefreshIcon from '@mui/icons-material/Refresh';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import QRCode from 'qrcode';
import type { AssetDTO, AssetCheckoutDTO } from '../api/types';
import { Inventory, type AssetCheckoutRequest, type AssetCheckinRequest, type AssetQrDTO } from '../api/inventory';
import { CheckoutDialog, CheckinDialog } from '../components/AssetDialogs';

function normalizeAssets(payload: { items: AssetDTO[] } | AssetDTO[]): AssetDTO[] {
  if (Array.isArray(payload)) return payload;
  return payload.items ?? [];
}

export default function InventoryPage() {
  const qc = useQueryClient();
  const assetsQuery = useQuery({
    queryKey: ['assets'],
    queryFn: () => Inventory.list().then(normalizeAssets),
  });
  const [selected, setSelected] = useState<AssetDTO | null>(null);
  const [qrDataUrl, setQrDataUrl] = useState<string | null>(null);
  const [history, setHistory] = useState<AssetCheckoutDTO[]>([]);
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
    onSuccess: (data) => setHistory(data),
  });

  const qrMutation = useMutation({
    mutationFn: (assetId: string) => Inventory.generateQr(assetId),
    onSuccess: async (data: AssetQrDTO) => {
      const url = data.qrUrl;
      const qr = (await QRCode.toDataURL(url, { width: 320 })) as string;
      setQrDataUrl(qr);
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
    setSelected(asset);
    setDialogOpen('checkout');
  };

  const openCheckin = (asset: AssetDTO) => {
    setSelected(asset);
    setDialogOpen('checkin');
  };

  const openQr = async (asset: AssetDTO) => {
    setSelected(asset);
    if (asset.qrToken) {
      const url = `https://tdf-app.pages.dev/inventario/scan/${asset.qrToken}`;
      const qr = (await QRCode.toDataURL(url, { width: 320 })) as string;
      setQrDataUrl(qr);
      setDialogOpen('qr');
    } else if (asset.assetId) {
      qrMutation.mutate(asset.assetId);
    }
  };

  const openHistory = (asset: AssetDTO) => {
    setSelected(asset);
    assetHistoryMutation.mutate(asset.assetId);
  };

  const assets = useMemo(() => assetsQuery.data ?? [], [assetsQuery.data]);
  const grouped = useMemo(() => assets, [assets]);

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

      <Card sx={{ bgcolor: 'rgba(255,255,255,0.02)', border: '1px solid rgba(255,255,255,0.08)' }}>
        <CardContent>
          <Table size="small">
            <TableHead>
              <TableRow>
                <TableCell>Equipo</TableCell>
                <TableCell>Categoría</TableCell>
                <TableCell>Estado</TableCell>
                <TableCell>Condición</TableCell>
                <TableCell>Ubicación</TableCell>
                <TableCell align="right">Acciones</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {grouped.map((asset) => (
                <TableRow key={asset.assetId} hover>
                  <TableCell>{asset.name}</TableCell>
                  <TableCell>{asset.category}</TableCell>
                  <TableCell>{asset.status}</TableCell>
                  <TableCell>{asset.condition ?? '—'}</TableCell>
                  <TableCell>{asset.location ?? '—'}</TableCell>
                  <TableCell align="right">
                    <IconButton size="small" onClick={() => void openQr(asset)} title="QR">
                      <QrCodeIcon fontSize="small" />
                    </IconButton>
                    <IconButton size="small" onClick={() => openCheckout(asset)} title="Check-out" disabled={asset.status.toLowerCase() === 'booked'}>
                      <ExitToAppIcon fontSize="small" />
                    </IconButton>
                    <IconButton size="small" onClick={() => openCheckin(asset)} title="Check-in" disabled={asset.status.toLowerCase() !== 'booked'}>
                      <HowToRegIcon fontSize="small" />
                    </IconButton>
                    <Button size="small" onClick={() => openHistory(asset)}>
                      Historial
                    </Button>
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </CardContent>
      </Card>

      <CheckoutDialog
        open={dialogOpen === 'checkout'}
        onClose={() => setDialogOpen(null)}
        asset={selected}
        form={form}
        onFormChange={setForm}
        onSubmit={() => selected && checkoutMutation.mutate({ assetId: selected.assetId, payload: form })}
        loading={checkoutMutation.isPending}
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

      {selected && history.length > 0 && (
        <Card sx={{ mt: 3, bgcolor: 'rgba(255,255,255,0.02)', border: '1px solid rgba(255,255,255,0.08)' }}>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              Historial · {selected.name}
            </Typography>
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
