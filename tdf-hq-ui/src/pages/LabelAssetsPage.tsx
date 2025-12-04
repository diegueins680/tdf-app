import { useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  IconButton,
  InputAdornment,
  MenuItem,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';
import ExitToAppIcon from '@mui/icons-material/ExitToApp';
import HowToRegIcon from '@mui/icons-material/HowToReg';
import QrCodeIcon from '@mui/icons-material/QrCode';
import RefreshIcon from '@mui/icons-material/Refresh';
import SearchIcon from '@mui/icons-material/Search';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import QRCode from 'qrcode';
import type { AssetCheckoutDTO, AssetDTO, AssetUpdate, PageResponse, RoomDTO } from '../api/types';
import { Inventory, type AssetCheckinRequest, type AssetCheckoutRequest, type AssetQrDTO } from '../api/inventory';
import { Rooms } from '../api/rooms';
import { CheckoutDialog, CheckinDialog } from '../components/AssetDialogs';
import GoogleDriveUploadWidget from '../components/GoogleDriveUploadWidget';

interface AssetFormState {
  name: string;
  category: string;
  status: string;
  locationId: string;
  photoUrl: string;
  notes: string;
}

type AssetsPayload = PageResponse<AssetDTO> | { items: AssetDTO[] } | AssetDTO[];

function normalizeAssets(payload: AssetsPayload): AssetDTO[] {
  if (Array.isArray(payload)) return payload;
  return payload.items ?? [];
}

function buildEmptyForm(): AssetFormState {
  return {
    name: '',
    category: '',
    status: 'Active',
    locationId: '',
    photoUrl: '',
    notes: '',
  };
}

function formFromAsset(asset: AssetDTO): AssetFormState {
  return {
    name: asset.name,
    category: asset.category,
    status: asset.status ?? 'Active',
    locationId: asset.location ?? '',
    photoUrl: '',
    notes: '',
  };
}

const STATUS_OPTIONS = [
  { value: 'all', label: 'Todos' },
  { value: 'Active', label: 'Activos' },
  { value: 'Booked', label: 'Prestados' },
  { value: 'OutForMaintenance', label: 'Mantenimiento' },
  { value: 'Retired', label: 'Retirados' },
];

export default function LabelAssetsPage() {
  const qc = useQueryClient();
  const assetsQuery = useQuery({
    queryKey: ['assets'],
    queryFn: () => Inventory.list({ pageSize: 200 }).then(normalizeAssets),
  });
  const roomsQuery = useQuery({
    queryKey: ['rooms'],
    queryFn: () => Rooms.list(),
  });
  const [search, setSearch] = useState('');
  const [statusFilter, setStatusFilter] = useState<string>('all');
  const [categoryFilter, setCategoryFilter] = useState<string>('all');
  const [selected, setSelected] = useState<AssetDTO | null>(null);
  const [editingAsset, setEditingAsset] = useState<AssetDTO | null>(null);
  const [assetForm, setAssetForm] = useState<AssetFormState>(buildEmptyForm);
  const [assetFormError, setAssetFormError] = useState<string | null>(null);
  const [qrDataUrl, setQrDataUrl] = useState<string | null>(null);
  const [history, setHistory] = useState<AssetCheckoutDTO[]>([]);
  const [dialogOpen, setDialogOpen] = useState<'checkout' | 'checkin' | 'qr' | 'form' | null>(null);
  const [checkoutForm, setCheckoutForm] = useState<AssetCheckoutRequest>({
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

  const createMutation = useMutation({
    mutationFn: async (draft: AssetFormState) => {
      const created = await Inventory.create({
        cName: draft.name.trim(),
        cCategory: draft.category.trim(),
      });
      const updates: AssetUpdate = {};
      if (draft.status && draft.status !== created.status) updates.uStatus = draft.status;
      if (draft.locationId) updates.uLocationId = draft.locationId;
      if (draft.notes) updates.uNotes = draft.notes;
      if (Object.keys(updates).length > 0) {
        return Inventory.update(created.assetId, updates);
      }
      return created;
    },
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['assets'] });
      setDialogOpen(null);
      setAssetForm(buildEmptyForm());
      setEditingAsset(null);
      setFeedback('Asset creado.');
    },
    onError: (err) => setAssetFormError(err instanceof Error ? err.message : 'No se pudo crear el asset.'),
  });

  const updateMutation = useMutation({
    mutationFn: ({ assetId, draft }: { assetId: string; draft: AssetFormState }) =>
      Inventory.update(assetId, {
        uName: draft.name.trim(),
        uCategory: draft.category.trim(),
        uStatus: draft.status,
        uLocationId: draft.locationId ? draft.locationId : null,
        uNotes: draft.notes.trim() ? draft.notes.trim() : null,
      }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['assets'] });
      setDialogOpen(null);
      setAssetForm(buildEmptyForm());
      setEditingAsset(null);
      setFeedback('Asset actualizado.');
    },
    onError: (err) => setAssetFormError(err instanceof Error ? err.message : 'No se pudo actualizar el asset.'),
  });

  const deleteMutation = useMutation({
    mutationFn: (assetId: string) => Inventory.remove(assetId),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['assets'] });
      setFeedback('Asset eliminado.');
    },
    onError: (err) => setFeedback(err instanceof Error ? err.message : 'No se pudo eliminar el asset.'),
  });

  const openCheckout = (asset: AssetDTO) => {
    setSelected(asset);
    setCheckoutForm({
      coTargetKind: 'party',
      coTargetParty: '',
      coTargetRoom: '',
      coTargetSession: '',
      coConditionOut: '',
      coNotes: '',
    });
    setDialogOpen('checkout');
  };

  const openCheckin = (asset: AssetDTO) => {
    setSelected(asset);
    setCheckinForm({
      ciConditionIn: '',
      ciNotes: '',
    });
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
    setHistory([]);
    assetHistoryMutation.mutate(asset.assetId);
  };

  const assets = useMemo(() => assetsQuery.data ?? [], [assetsQuery.data]);
  const rooms = useMemo(() => roomsQuery.data ?? [], [roomsQuery.data]);
  const roomMap = useMemo(() => new Map<string, RoomDTO>(rooms.map((r) => [r.roomId, r])), [rooms]);
  const categories = useMemo(
    () => Array.from(new Set(assets.map((a) => a.category))).sort((a, b) => a.localeCompare(b)),
    [assets],
  );

  const statusCounts = useMemo(() => {
    return assets.reduce<Record<string, number>>((acc, asset) => {
      const key = asset.status ?? 'Unknown';
      acc[key] = (acc[key] ?? 0) + 1;
      return acc;
    }, {});
  }, [assets]);

  const filteredAssets = useMemo(() => {
    const term = search.trim().toLowerCase();
    return assets.filter((asset) => {
      const matchesSearch =
        !term ||
        `${asset.name} ${asset.category} ${asset.brand ?? ''} ${asset.model ?? ''} ${asset.status}`
          .toLowerCase()
          .includes(term);
      const matchesStatus = statusFilter === 'all' || asset.status === statusFilter;
      const matchesCategory = categoryFilter === 'all' || asset.category === categoryFilter;
      return matchesSearch && matchesStatus && matchesCategory;
    });
  }, [assets, search, statusFilter, categoryFilter]);

  const handleOpenNew = () => {
    setEditingAsset(null);
    setAssetForm(buildEmptyForm());
    setAssetFormError(null);
    setDialogOpen('form');
  };

  const handleEdit = (asset: AssetDTO) => {
    setEditingAsset(asset);
    setAssetForm(formFromAsset(asset));
    setAssetFormError(null);
    setDialogOpen('form');
  };

  const handleDelete = (asset: AssetDTO) => {
    const confirm = window.confirm(`¿Eliminar ${asset.name}? Esta acción no se puede deshacer.`);
    if (!confirm) return;
    deleteMutation.mutate(asset.assetId);
  };

  const handleSaveAsset = () => {
    const trimmedName = assetForm.name.trim();
    const trimmedCategory = assetForm.category.trim();
    if (!trimmedName) {
      setAssetFormError('Agrega un nombre para el asset.');
      return;
    }
    if (!trimmedCategory) {
      setAssetFormError('Agrega una categoría.');
      return;
    }
    const draft: AssetFormState = {
      name: trimmedName,
      category: trimmedCategory,
      status: assetForm.status,
      locationId: assetForm.locationId,
      photoUrl: assetForm.photoUrl,
      notes: assetForm.notes,
    };
    if (editingAsset) {
      updateMutation.mutate({ assetId: editingAsset.assetId, draft });
    } else {
      createMutation.mutate(draft);
    }
  };

  const displayLocation = (asset: AssetDTO) => {
    if (!asset.location) return '—';
    const room = roomMap.get(asset.location);
    return room?.rName ?? asset.location;
  };

  const renderStatusChip = (status: string) => {
    const key = status.toLowerCase();
    const color: 'default' | 'success' | 'warning' | 'info' =
      key === 'active' ? 'success' : key === 'booked' ? 'warning' : key === 'outformaintenance' ? 'info' : 'default';
    return <Chip size="small" label={status} color={color} variant="outlined" />;
  };

  return (
    <Box sx={{ color: '#0f172a' }}>
      <Stack direction="row" justifyContent="space-between" alignItems="center" mb={2}>
        <Box>
          <Typography variant="overline" color="text.secondary">
            Label / Assets
          </Typography>
          <Typography variant="h4" fontWeight={800}>
            Catálogo de assets y equipo del label
          </Typography>
          <Typography variant="body1" color="text.secondary">
            Reutiliza el inventario para que marketing, estudio y label tengan una sola lista de equipo, préstamos y QR.
          </Typography>
        </Box>
        <Stack direction="row" spacing={1}>
          <Button variant="outlined" startIcon={<RefreshIcon />} onClick={() => void qc.invalidateQueries({ queryKey: ['assets'] })}>
            Actualizar
          </Button>
          <Button variant="contained" startIcon={<AddIcon />} onClick={handleOpenNew}>
            Agregar asset
          </Button>
        </Stack>
      </Stack>

      {feedback && (
        <Alert severity="info" sx={{ mb: 2 }} onClose={() => setFeedback(null)}>
          {feedback}
        </Alert>
      )}
      {assetsQuery.isError && <Alert severity="error" sx={{ mb: 2 }}>No se pudo cargar el inventario.</Alert>}

      <Card sx={{ mb: 2 }}>
        <CardContent>
          <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
            <TextField
              fullWidth
              placeholder="Buscar por nombre, categoría o modelo"
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              InputProps={{
                startAdornment: (
                  <InputAdornment position="start">
                    <SearchIcon fontSize="small" />
                  </InputAdornment>
                ),
              }}
            />
            <TextField
              select
              label="Estado"
              value={statusFilter}
              onChange={(e) => setStatusFilter(e.target.value)}
              sx={{ minWidth: 180 }}
            >
              {STATUS_OPTIONS.map((opt) => (
                <MenuItem key={opt.value} value={opt.value}>
                  {opt.label}
                </MenuItem>
              ))}
            </TextField>
            <TextField
              select
              label="Categoría"
              value={categoryFilter}
              onChange={(e) => setCategoryFilter(e.target.value)}
              sx={{ minWidth: 180 }}
            >
              <MenuItem value="all">Todas</MenuItem>
              {categories.map((cat) => (
                <MenuItem key={cat} value={cat}>
                  {cat}
                </MenuItem>
              ))}
            </TextField>
          </Stack>
          <Stack direction="row" spacing={1} mt={2} flexWrap="wrap">
            {STATUS_OPTIONS.filter((s) => s.value !== 'all').map((opt) => (
              <Chip
                key={opt.value}
                label={`${opt.label} ${statusCounts[opt.value] ? `(${statusCounts[opt.value]})` : ''}`}
                onClick={() => setStatusFilter(opt.value)}
                variant={statusFilter === opt.value ? 'filled' : 'outlined'}
                color={statusFilter === opt.value ? 'primary' : 'default'}
                sx={{ textTransform: 'capitalize' }}
              />
            ))}
          </Stack>
        </CardContent>
      </Card>

      <Card sx={{ bgcolor: 'background.paper', border: '1px solid', borderColor: 'divider' }}>
        <CardContent>
          {assetsQuery.isLoading ? (
            <Typography variant="body2" color="text.secondary">
              Cargando inventario…
            </Typography>
          ) : (
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Asset</TableCell>
                  <TableCell>Categoría</TableCell>
                  <TableCell>Estado</TableCell>
                  <TableCell>Ubicación</TableCell>
                  <TableCell align="right">Acciones</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {filteredAssets.map((asset) => (
                  <TableRow key={asset.assetId} hover>
                    <TableCell>
                      <Stack spacing={0.25}>
                        <Typography fontWeight={600}>{asset.name}</Typography>
                        {(asset.brand ?? asset.model) && (
                          <Typography variant="caption" color="text.secondary">
                            {[asset.brand, asset.model].filter(Boolean).join(' · ')}
                          </Typography>
                        )}
                        {asset.qrToken && (
                          <Typography variant="caption" color="text.secondary">
                            QR activo
                          </Typography>
                        )}
                      </Stack>
                    </TableCell>
                    <TableCell>{asset.category}</TableCell>
                    <TableCell>{renderStatusChip(asset.status)}</TableCell>
                    <TableCell>{displayLocation(asset)}</TableCell>
                    <TableCell align="right">
                      <Tooltip title="Editar">
                        <IconButton size="small" onClick={() => handleEdit(asset)}>
                          <EditIcon fontSize="small" />
                        </IconButton>
                      </Tooltip>
                      <Tooltip title="QR">
                        <IconButton size="small" onClick={() => void openQr(asset)}>
                          <QrCodeIcon fontSize="small" />
                        </IconButton>
                      </Tooltip>
                      <Tooltip title="Check-out">
                        <span>
                          <IconButton
                            size="small"
                            onClick={() => openCheckout(asset)}
                            disabled={asset.status.toLowerCase() === 'booked'}
                          >
                            <ExitToAppIcon fontSize="small" />
                          </IconButton>
                        </span>
                      </Tooltip>
                      <Tooltip title="Check-in">
                        <span>
                          <IconButton
                            size="small"
                            onClick={() => openCheckin(asset)}
                            disabled={asset.status.toLowerCase() !== 'booked'}
                          >
                            <HowToRegIcon fontSize="small" />
                          </IconButton>
                        </span>
                      </Tooltip>
                      <Button size="small" onClick={() => openHistory(asset)}>
                        Historial
                      </Button>
                      <Tooltip title="Eliminar">
                        <span>
                          <IconButton
                            size="small"
                            onClick={() => handleDelete(asset)}
                            disabled={deleteMutation.isPending}
                          >
                            <DeleteIcon fontSize="small" />
                          </IconButton>
                        </span>
                      </Tooltip>
                    </TableCell>
                  </TableRow>
                ))}
                {filteredAssets.length === 0 && !assetsQuery.isLoading && (
                  <TableRow>
                    <TableCell colSpan={5}>
                      <Typography variant="body2" color="text.secondary">
                        No hay assets con los filtros actuales.
                      </Typography>
                    </TableCell>
                  </TableRow>
                )}
              </TableBody>
            </Table>
          )}
        </CardContent>
      </Card>

      <CheckoutDialog
        open={dialogOpen === 'checkout'}
        onClose={() => setDialogOpen(null)}
        asset={selected}
        form={checkoutForm}
        onFormChange={setCheckoutForm}
        onSubmit={() => selected && checkoutMutation.mutate({ assetId: selected.assetId, payload: checkoutForm })}
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
        <DialogTitle>QR de asset</DialogTitle>
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

      <Dialog open={dialogOpen === 'form'} onClose={() => setDialogOpen(null)} maxWidth="sm" fullWidth>
        <DialogTitle>{editingAsset ? 'Editar asset' : 'Nuevo asset'}</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={2} sx={{ mt: 1 }}>
            <TextField
              label="Nombre"
              value={assetForm.name}
              onChange={(e) => setAssetForm((prev) => ({ ...prev, name: e.target.value }))}
              required
              fullWidth
            />
            <TextField
              label="Categoría"
              value={assetForm.category}
              onChange={(e) => setAssetForm((prev) => ({ ...prev, category: e.target.value }))}
              required
              fullWidth
              select={categories.length > 0}
              helperText={categories.length ? 'Selecciona o escribe una categoría' : undefined}
            >
              {categories.map((cat) => (
                <MenuItem key={cat} value={cat}>
                  {cat}
                </MenuItem>
              ))}
            </TextField>
            <GoogleDriveUploadWidget
              label="Subir foto a Drive"
              helperText="Adjunta una foto y guardaremos el link en notas."
              onComplete={(files) => {
                const link = files[0]?.webViewLink ?? files[0]?.webContentLink;
                if (link) {
                  setAssetForm((prev) => ({ ...prev, notes: link, photoUrl: link }));
                }
              }}
              accept="image/*"
              dense
            />
            <TextField
              label="Notas / link de Drive"
              value={assetForm.notes}
              onChange={(e) => setAssetForm((prev) => ({ ...prev, notes: e.target.value }))}
              fullWidth
              placeholder="Link de Drive u otras notas"
            />
            <TextField
              select
              label="Estado"
              value={assetForm.status}
              onChange={(e) => setAssetForm((prev) => ({ ...prev, status: e.target.value }))}
            >
              {STATUS_OPTIONS.filter((opt) => opt.value !== 'all').map((opt) => (
                <MenuItem key={opt.value} value={opt.value}>
                  {opt.label}
                </MenuItem>
              ))}
            </TextField>
            <TextField
              select
              label="Ubicación (sala)"
              value={assetForm.locationId}
              onChange={(e) => setAssetForm((prev) => ({ ...prev, locationId: e.target.value }))}
              helperText="Opcional. Usa las salas del estudio para ubicar el asset."
            >
              <MenuItem value="">Sin sala</MenuItem>
              {rooms.map((room) => (
                <MenuItem key={room.roomId} value={room.roomId}>
                  {room.rName}
                </MenuItem>
              ))}
            </TextField>
            {assetFormError && <Alert severity="error">{assetFormError}</Alert>}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setDialogOpen(null)}>Cancelar</Button>
          <Button
            variant="contained"
            onClick={handleSaveAsset}
            disabled={createMutation.isPending || updateMutation.isPending}
          >
            {createMutation.isPending || updateMutation.isPending ? 'Guardando…' : 'Guardar'}
          </Button>
        </DialogActions>
      </Dialog>

      {selected && history.length > 0 && (
        <Card sx={{ mt: 3, bgcolor: 'rgba(255,255,255,0.5)', border: '1px solid', borderColor: 'divider' }}>
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
