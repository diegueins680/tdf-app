import { useMemo, useState, type MouseEvent } from 'react';
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
  Menu,
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
import QrCodeIcon from '@mui/icons-material/QrCode';
import ExitToAppIcon from '@mui/icons-material/ExitToApp';
import HowToRegIcon from '@mui/icons-material/HowToReg';
import RefreshIcon from '@mui/icons-material/Refresh';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import type { AssetDTO, AssetCheckoutDTO, PartyDTO, RoomDTO } from '../api/types';
import { Inventory, type AssetCheckoutRequest, type AssetCheckinRequest, type AssetQrDTO } from '../api/inventory';
import { CheckoutDialog, CheckinDialog } from '../components/AssetDialogs';
import { Rooms } from '../api/rooms';
import { Parties } from '../api/parties';
import { buildInventoryScanUrl } from '../config/appConfig';
import {
  formatCheckoutPaymentSummary,
  formatCheckoutTargetDisplay,
  getCheckoutDispositionLabel,
} from '../utils/inventoryCheckout';

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

function getSharedInventoryStatusSummary(assets: readonly AssetDTO[]) {
  if (assets.length < 2) return '';

  const normalizedStatuses = assets
    .map((asset) => asset.status.trim())
    .filter((status) => status !== '');

  if (normalizedStatuses.length !== assets.length) return '';

  const firstStatus = normalizedStatuses[0]!;
  const firstComparableStatus = firstStatus.toLocaleLowerCase('es');

  return normalizedStatuses.every((status) => status.toLocaleLowerCase('es') === firstComparableStatus)
    ? getInventoryStatusLabel(firstStatus)
    : '';
}

function normalizeInventoryField(value?: string | null) {
  const trimmed = value?.trim();
  return trimmed ? trimmed : null;
}

const normalizeInventoryComparisonValue = (value?: string | null) =>
  normalizeInventoryField(value)?.toLocaleLowerCase('es') ?? '';

function getSharedInventoryLocationSummary(assets: readonly AssetDTO[]) {
  if (assets.length < 2) return '';

  const normalizedLocations = assets
    .map((asset) => normalizeInventoryField(asset.location))
    .filter((location): location is string => location != null);

  if (normalizedLocations.length !== assets.length) return '';

  const [firstLocation] = normalizedLocations;
  const firstComparableLocation = normalizeInventoryComparisonValue(firstLocation);

  return normalizedLocations.every(
    (location) => normalizeInventoryComparisonValue(location) === firstComparableLocation,
  )
    ? (firstLocation ?? '')
    : '';
}

function getSharedInventoryCategorySummary(assets: readonly AssetDTO[]) {
  if (assets.length < 2) return '';

  const normalizedCategories = assets
    .map((asset) => normalizeInventoryField(asset.category))
    .filter((category): category is string => category != null);

  if (normalizedCategories.length !== assets.length) return '';

  const [firstCategory] = normalizedCategories;
  const firstComparableCategory = normalizeInventoryComparisonValue(firstCategory);

  return normalizedCategories.every(
    (category) => normalizeInventoryComparisonValue(category) === firstComparableCategory,
  )
    ? (firstCategory ?? '')
    : '';
}

function joinInventorySummaryParts(parts: readonly string[]) {
  if (parts.length <= 1) return parts[0] ?? '';
  if (parts.length === 2) return `${parts[0]} y ${parts[1]}`;
  return `${parts.slice(0, -1).join(', ')} y ${parts[parts.length - 1]}`;
}

function getSharedInventoryColumnSummary({
  status,
  category,
  location,
}: {
  status: string;
  category: string;
  location: string;
}) {
  const sharedParts = [
    status ? `estado ${status}` : '',
    category ? `categoría ${category}` : '',
    location ? `ubicación ${location}` : '',
  ].filter(Boolean);

  if (sharedParts.length < 2) return '';

  return `Se ocultaron columnas porque toda esta vista coincide en ${joinInventorySummaryParts(sharedParts)}. Volverán cuando esta vista mezcle valores distintos.`;
}

function getCurrentTargetSummary(asset: AssetDTO, roomMap: Map<string, RoomDTO>) {
  return formatCheckoutTargetDisplay(asset.currentCheckoutKind, asset.currentCheckoutTarget, roomMap);
}

function buildCurrentCheckoutContextSummary({
  disposition,
  checkedOutAt,
  dueAt,
  paymentSummary,
}: {
  disposition?: string | null;
  checkedOutAt?: string | null;
  dueAt?: string | null;
  paymentSummary: string;
}) {
  return [
    normalizeInventoryField(disposition) ? getCheckoutDispositionLabel(disposition) : '',
    checkedOutAt ? `Salida: ${formatDate(checkedOutAt)}` : '',
    dueAt ? `Retorno pactado: ${formatDate(dueAt)}` : '',
    paymentSummary ? `Pago: ${paymentSummary}` : '',
  ]
    .filter(Boolean)
    .join(' · ');
}

const INVENTORY_LOCATION_SETUP_GUIDANCE =
  'La ubicación aparecerá en la tabla cuando al menos un equipo tenga una ubicación registrada.';
const INVENTORY_CHECKOUT_CONTEXT_GUIDANCE =
  'Quién lo tiene y desde cuándo aparecerán en la tabla cuando al menos un equipo tenga un check-out activo.';
const INVENTORY_MOVEMENT_GUIDANCE =
  'Usa check-out o check-in cuando esté disponible para registrar el siguiente movimiento.';
const INVENTORY_CHECKOUT_ONLY_GUIDANCE =
  'Usa check-out para registrar la siguiente salida.';
const INVENTORY_CHECKIN_ONLY_GUIDANCE =
  'Usa check-in para registrar el siguiente retorno.';
const INVENTORY_NO_MOVEMENT_GUIDANCE =
  'En esta vista no hay movimientos disponibles por ahora.';
const INVENTORY_SINGLE_ASSET_NO_MOVEMENT_GUIDANCE =
  'En este estado no hay check-out ni check-in disponibles. Usa QR e historial si necesitas revisar el registro.';
const INVENTORY_ROW_SECONDARY_ACTIONS_LABEL = 'QR e historial';

function getInventoryMovementGuidance({
  canCheckout,
  canCheckin,
}: {
  canCheckout: boolean;
  canCheckin: boolean;
}) {
  if (canCheckout && canCheckin) return INVENTORY_MOVEMENT_GUIDANCE;
  if (canCheckout) return INVENTORY_CHECKOUT_ONLY_GUIDANCE;
  if (canCheckin) return INVENTORY_CHECKIN_ONLY_GUIDANCE;
  return INVENTORY_NO_MOVEMENT_GUIDANCE;
}

export default function InventoryPage() {
  const qc = useQueryClient();
  const handleRefreshAssets = () => {
    void qc.invalidateQueries({ queryKey: ['assets'] });
  };
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
  const [qrShareUrl, setQrShareUrl] = useState<string>('');
  const [history, setHistory] = useState<AssetCheckoutDTO[]>([]);
  const [historyViewMode, setHistoryViewMode] = useState<'panel' | 'embedded' | null>(null);
  const [actionsMenuTarget, setActionsMenuTarget] = useState<{ anchorEl: HTMLButtonElement; asset: AssetDTO } | null>(null);
  const [currentCheckout, setCurrentCheckout] = useState<AssetCheckoutDTO | null>(null);
  const [dialogOpen, setDialogOpen] = useState<'checkout' | 'checkin' | 'qr' | null>(null);
  const [form, setForm] = useState<AssetCheckoutRequest>({
    coTargetKind: 'party',
    coTargetParty: '',
    coTargetRoom: '',
    coTargetSession: '',
    coDisposition: 'loan',
    coTermsAndConditions: '',
    coHolderEmail: '',
    coHolderPhone: '',
    coPaymentType: '',
    coPaymentInstallments: null,
    coPaymentReference: '',
    coPaymentAmount: '',
    coPaymentCurrency: '',
    coPaymentOutstanding: '',
    coPhotoUrl: '',
    coConditionOut: '',
    coNotes: '',
  });
  const [checkinForm, setCheckinForm] = useState<AssetCheckinRequest>({
    ciConditionIn: '',
    ciNotes: '',
    ciPhotoUrl: '',
  });
  const [feedback, setFeedback] = useState<string | null>(null);
  const [checkoutPhotoUploading, setCheckoutPhotoUploading] = useState(false);
  const [checkinPhotoUploading, setCheckinPhotoUploading] = useState(false);

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
      void qc.invalidateQueries({ queryKey: ['assets'] });
      const url = data.qrUrl;
      const qrImg = `https://api.qrserver.com/v1/create-qr-code/?size=320x320&data=${encodeURIComponent(url)}`;
      setQrDataUrl(qrImg);
      setQrShareUrl(url);
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
      coDisposition: 'loan',
      coTermsAndConditions: '',
      coHolderEmail: '',
      coHolderPhone: '',
      coPaymentType: '',
      coPaymentInstallments: null,
      coPaymentReference: '',
      coPaymentAmount: '',
      coPaymentCurrency: '',
      coPaymentOutstanding: '',
      coPhotoUrl: '',
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
    setCheckinForm({
      ciConditionIn: '',
      ciNotes: '',
      ciPhotoUrl: '',
    });
    setDialogOpen('checkin');
  };

  const openQr = (asset: AssetDTO) => {
    setHistoryViewMode(null);
    setSelected(asset);
    if (asset.qrToken) {
      const url = buildInventoryScanUrl(asset.qrToken);
      const qrImg = `https://api.qrserver.com/v1/create-qr-code/?size=320x320&data=${encodeURIComponent(url)}`;
      setQrDataUrl(qrImg);
      setQrShareUrl(url);
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

  const openActionsMenu = (event: MouseEvent<HTMLButtonElement>, asset: AssetDTO) => {
    setActionsMenuTarget({ anchorEl: event.currentTarget, asset });
  };

  const closeActionsMenu = () => {
    setActionsMenuTarget(null);
  };

  const runAssetMenuAction = (action: (asset: AssetDTO) => void) => {
    const asset = actionsMenuTarget?.asset;
    closeActionsMenu();
    if (asset) action(asset);
  };

  const copyShareUrl = async (asset: AssetDTO) => {
    let url = asset.qrToken ? buildInventoryScanUrl(asset.qrToken) : '';
    if (!url && asset.assetId) {
      try {
        const generated = await Inventory.generateQr(asset.assetId);
        url = generated.qrUrl;
        void qc.invalidateQueries({ queryKey: ['assets'] });
      } catch (error) {
        setFeedback(error instanceof Error ? error.message : 'No se pudo generar el enlace público.');
        return;
      }
    }
    try {
      await navigator.clipboard.writeText(url);
      setFeedback('Enlace público copiado.');
    } catch {
      setFeedback(url);
    }
  };

  const uploadCheckoutPhoto = async (file: File) => {
    setCheckoutPhotoUploading(true);
    try {
      const uploaded = await Inventory.uploadPhoto(file, { name: file.name });
      setForm((prev) => ({ ...prev, coPhotoUrl: uploaded.publicUrl || uploaded.webContentLink || uploaded.id }));
    } catch (error) {
      setFeedback(error instanceof Error ? error.message : 'No se pudo subir la foto de salida.');
    } finally {
      setCheckoutPhotoUploading(false);
    }
  };

  const uploadCheckinPhoto = async (file: File) => {
    setCheckinPhotoUploading(true);
    try {
      const uploaded = await Inventory.uploadPhoto(file, { name: file.name });
      setCheckinForm((prev) => ({ ...prev, ciPhotoUrl: uploaded.publicUrl || uploaded.webContentLink || uploaded.id }));
    } catch (error) {
      setFeedback(error instanceof Error ? error.message : 'No se pudo subir la foto de retorno.');
    } finally {
      setCheckinPhotoUploading(false);
    }
  };

  const assets = useMemo(() => assetsQuery.data ?? [], [assetsQuery.data]);
  const grouped = useMemo(() => assets, [assets]);
  const roomOptions = useMemo<RoomDTO[]>(() => roomsQuery.data ?? [], [roomsQuery.data]);
  const roomMap = useMemo(() => new Map(roomOptions.map((room) => [room.roomId, room])), [roomOptions]);
  const partyOptions = useMemo<PartyDTO[]>(() => partiesQuery.data ?? [], [partiesQuery.data]);
  const singleAsset = grouped.length === 1 ? (grouped[0] ?? null) : null;
  const showFirstAssetEmptyState = !assetsQuery.isLoading && !assetsQuery.error && grouped.length === 0;
  const showSingleAssetSummary = !assetsQuery.isLoading && !assetsQuery.error && singleAsset != null;
  const singleAssetLocation = singleAsset ? normalizeInventoryField(singleAsset.location) : null;
  const singleAssetCondition = singleAsset ? normalizeInventoryField(singleAsset.condition) : null;
  const singleAssetMovementState = singleAsset ? getInventoryMovementState(singleAsset.status) : null;
  const sharedStatusSummary = useMemo(() => getSharedInventoryStatusSummary(grouped), [grouped]);
  const sharedCategorySummary = useMemo(() => getSharedInventoryCategorySummary(grouped), [grouped]);
  const sharedLocationSummary = useMemo(() => getSharedInventoryLocationSummary(grouped), [grouped]);
  const sharedColumnSummary = useMemo(
    () => getSharedInventoryColumnSummary({
      status: sharedStatusSummary,
      category: sharedCategorySummary,
      location: sharedLocationSummary,
    }),
    [sharedCategorySummary, sharedLocationSummary, sharedStatusSummary],
  );
  const showSingleAssetNoMovementGuidance = Boolean(
    showSingleAssetSummary
    && singleAssetMovementState
    && !singleAssetMovementState.canCheckout
    && !singleAssetMovementState.canCheckin,
  );
  const showStatusColumn = sharedStatusSummary === '';
  const showLocationColumn = sharedLocationSummary === '' && grouped.some((asset) => normalizeInventoryField(asset.location) != null);
  const showCurrentCheckoutColumns = grouped.some((asset) => {
    const movementState = getInventoryMovementState(asset.status);
    return movementState.canCheckin
      || normalizeInventoryField(asset.currentCheckoutTarget) != null
      || Boolean(asset.currentCheckoutAt);
  });
  const showLocationSetupGuidance = grouped.length > 1 && sharedLocationSummary === '' && !showLocationColumn;
  const showCheckoutContextGuidance = grouped.length > 1 && !showCurrentCheckoutColumns;
  const showMovementGuidance = grouped.length > 1;
  const visibleMovementActions = grouped.reduce(
    (summary, asset) => {
      const movementState = getInventoryMovementState(asset.status);
      return {
        canCheckout: summary.canCheckout || movementState.canCheckout,
        canCheckin: summary.canCheckin || movementState.canCheckin,
      };
    },
    { canCheckout: false, canCheckin: false },
  );
  const movementGuidance = showMovementGuidance ? getInventoryMovementGuidance(visibleMovementActions) : '';
  const tableGuidance = [
    showLocationSetupGuidance ? INVENTORY_LOCATION_SETUP_GUIDANCE : '',
    showCheckoutContextGuidance ? INVENTORY_CHECKOUT_CONTEXT_GUIDANCE : '',
    movementGuidance,
  ]
    .filter(Boolean)
    .join(' ');
  const showHeaderRefreshAction = !showFirstAssetEmptyState;

  return (
    <Box sx={{ color: '#e2e8f0' }}>
      <Stack direction="row" justifyContent="space-between" alignItems="center" mb={2}>
        <Box>
          <Typography variant="h5" fontWeight={800}>
            Inventario de equipo
          </Typography>
          <Typography variant="body2" color="rgba(226,232,240,0.75)">
            Administra equipos, ve quién los tiene, comparte enlaces públicos y registra check-out / check-in con evidencia.
          </Typography>
        </Box>
        {showHeaderRefreshAction && (
          <Stack direction="row" spacing={1}>
            <Button variant="outlined" startIcon={<RefreshIcon />} onClick={handleRefreshAssets}>
              Actualizar
            </Button>
          </Stack>
        )}
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
                Si estás esperando la carga inicial del inventario, vuelve a consultar desde aquí sin revisar una tabla vacía.
              </Typography>
              <Button
                size="small"
                variant="outlined"
                startIcon={<RefreshIcon />}
                onClick={handleRefreshAssets}
                sx={{ alignSelf: 'flex-start' }}
              >
                Volver a consultar inventario
              </Button>
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
                {singleAsset.currentCheckoutTarget && (
                  <>
                    <Typography variant="body2" color="rgba(226,232,240,0.78)">
                      Tiene: {getCurrentTargetSummary(singleAsset, roomMap)}
                    </Typography>
                    <Typography variant="body2" color="rgba(226,232,240,0.78)">
                      Movimiento: {getCheckoutDispositionLabel(singleAsset.currentCheckoutDisposition)}
                    </Typography>
                    {singleAsset.currentCheckoutAt && (
                      <Typography variant="body2" color="rgba(226,232,240,0.78)">
                        Salió: {formatDate(singleAsset.currentCheckoutAt)}
                      </Typography>
                    )}
                    {singleAsset.currentCheckoutDueAt && (
                      <Typography variant="body2" color="rgba(226,232,240,0.78)">
                        Retorno pactado: {formatDate(singleAsset.currentCheckoutDueAt)}
                      </Typography>
                    )}
                    {formatCheckoutPaymentSummary(
                      singleAsset.currentCheckoutPaymentType,
                      singleAsset.currentCheckoutPaymentInstallments,
                      singleAsset.currentCheckoutPaymentAmountCents,
                      singleAsset.currentCheckoutPaymentCurrency,
                      singleAsset.currentCheckoutPaymentOutstandingCents,
                    ) && (
                      <Typography variant="body2" color="rgba(226,232,240,0.78)">
                        Pago:{' '}
                        {formatCheckoutPaymentSummary(
                          singleAsset.currentCheckoutPaymentType,
                          singleAsset.currentCheckoutPaymentInstallments,
                          singleAsset.currentCheckoutPaymentAmountCents,
                          singleAsset.currentCheckoutPaymentCurrency,
                          singleAsset.currentCheckoutPaymentOutstandingCents,
                        )}
                      </Typography>
                    )}
                  </>
                )}
              </Stack>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                {singleAssetMovementState?.canCheckout && (
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
                {singleAssetMovementState?.canCheckin && (
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
                {showSingleAssetNoMovementGuidance && (
                  <Typography variant="body2" color="rgba(226,232,240,0.68)">
                    {INVENTORY_SINGLE_ASSET_NO_MOVEMENT_GUIDANCE}
                  </Typography>
                )}
                <Button
                  size="small"
                  variant="outlined"
                  startIcon={<QrCodeIcon />}
                  onClick={(event) => openActionsMenu(event, singleAsset)}
                  aria-label={`Abrir QR, enlace e historial de ${singleAsset.name}`}
                  sx={{ textTransform: 'none' }}
                >
                  {INVENTORY_ROW_SECONDARY_ACTIONS_LABEL}
                </Button>
              </Stack>
            </Stack>
          </CardContent>
        </Card>
      ) : (
        <Card sx={{ bgcolor: 'rgba(255,255,255,0.02)', border: '1px solid rgba(255,255,255,0.08)' }}>
          <CardContent>
            <Stack spacing={1.5}>
              {sharedColumnSummary ? (
                <Typography
                  variant="caption"
                  color="rgba(226,232,240,0.68)"
                  data-testid="inventory-shared-columns-summary"
                >
                  {sharedColumnSummary}
                </Typography>
              ) : (
                <>
                  {sharedStatusSummary && (
                    <Typography variant="caption" color="rgba(226,232,240,0.68)">
                      {`Mostrando un solo estado: ${sharedStatusSummary}. La columna volverá cuando esta vista mezcle estados distintos.`}
                    </Typography>
                  )}
                  {sharedCategorySummary && (
                    <Typography variant="caption" color="rgba(226,232,240,0.68)">
                      {`Mostrando una sola categoría: ${sharedCategorySummary}. La categoría volverá cuando esta vista mezcle categorías distintas.`}
                    </Typography>
                  )}
                  {sharedLocationSummary && (
                    <Typography variant="caption" color="rgba(226,232,240,0.68)">
                      {`Mostrando una sola ubicación: ${sharedLocationSummary}. La columna volverá cuando esta vista mezcle ubicaciones distintas.`}
                    </Typography>
                  )}
                </>
              )}
              {tableGuidance && (
                <Typography variant="body2" color="rgba(226,232,240,0.68)">
                  {tableGuidance}
                </Typography>
              )}
              <Table size="small">
                <TableHead>
                  <TableRow>
                    <TableCell>Equipo</TableCell>
                    {showStatusColumn && <TableCell>Estado</TableCell>}
                    {showCurrentCheckoutColumns && <TableCell>Tenencia actual</TableCell>}
                    {showLocationColumn && <TableCell>Ubicación</TableCell>}
                    <TableCell align="right">Acciones</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {grouped.map((asset) => {
                    const movementState = getInventoryMovementState(asset.status);
                    const assetCondition = normalizeInventoryField(asset.condition);
                    const paymentSummary = formatCheckoutPaymentSummary(
                      asset.currentCheckoutPaymentType,
                      asset.currentCheckoutPaymentInstallments,
                      asset.currentCheckoutPaymentAmountCents,
                      asset.currentCheckoutPaymentCurrency,
                      asset.currentCheckoutPaymentOutstandingCents,
                    );
                    const hasCurrentCheckoutContext = Boolean(
                      normalizeInventoryField(asset.currentCheckoutTarget)
                      || asset.currentCheckoutAt
                      || normalizeInventoryField(asset.currentCheckoutHolderEmail)
                      || normalizeInventoryField(asset.currentCheckoutHolderPhone)
                      || asset.currentCheckoutDueAt
                      || paymentSummary
                      || normalizeInventoryField(asset.currentCheckoutDisposition),
                    );
                    const checkoutContextSummary = buildCurrentCheckoutContextSummary({
                      disposition: asset.currentCheckoutDisposition,
                      checkedOutAt: asset.currentCheckoutAt,
                      dueAt: asset.currentCheckoutDueAt,
                      paymentSummary,
                    });

                    return (
                      <TableRow key={asset.assetId} hover>
                        <TableCell>
                          <Stack spacing={0.25}>
                            <Typography variant="body2" fontWeight={700}>
                              {asset.name}
                            </Typography>
                            {!sharedCategorySummary && (
                              <Typography variant="caption" color="text.secondary">
                                {asset.category}
                              </Typography>
                            )}
                            {assetCondition && (
                              <Typography variant="caption" color="text.secondary">
                                Condición: {assetCondition}
                              </Typography>
                            )}
                          </Stack>
                        </TableCell>
                        {showStatusColumn && <TableCell>{getInventoryStatusLabel(asset.status)}</TableCell>}
                        {showCurrentCheckoutColumns && (
                          <TableCell>
                            {hasCurrentCheckoutContext ? (
                              <Stack spacing={0.25}>
                                {normalizeInventoryField(asset.currentCheckoutTarget) && (
                                  <Typography variant="body2">
                                    {getCurrentTargetSummary(asset, roomMap)}
                                  </Typography>
                                )}
                                {checkoutContextSummary && (
                                  <Typography variant="caption" color="text.secondary">
                                    {checkoutContextSummary}
                                  </Typography>
                                )}
                                {(asset.currentCheckoutHolderEmail || asset.currentCheckoutHolderPhone) && (
                                  <Typography variant="caption" color="text.secondary">
                                    {[asset.currentCheckoutHolderEmail, asset.currentCheckoutHolderPhone].filter(Boolean).join(' · ')}
                                  </Typography>
                                )}
                              </Stack>
                            ) : (
                              '—'
                            )}
                          </TableCell>
                        )}
                        {showLocationColumn && <TableCell>{normalizeInventoryField(asset.location) ?? '—'}</TableCell>}
                        <TableCell align="right">
                          <Stack direction="row" spacing={0.5} justifyContent="flex-end" alignItems="center">
                            {movementState.canCheckout && (
                              <Tooltip title="Registrar check-out">
                                <IconButton
                                  size="small"
                                  onClick={() => openCheckout(asset)}
                                  aria-label={`Abrir check-out de ${asset.name}`}
                                >
                                  <ExitToAppIcon fontSize="small" />
                                </IconButton>
                              </Tooltip>
                            )}
                            {movementState.canCheckin && (
                              <Tooltip title="Registrar check-in">
                                <IconButton
                                  size="small"
                                  onClick={() => openCheckin(asset)}
                                  aria-label={`Abrir check-in de ${asset.name}`}
                                >
                                  <HowToRegIcon fontSize="small" />
                                </IconButton>
                              </Tooltip>
                            )}
                            <Button
                              size="small"
                              variant="text"
                              onClick={(event) => openActionsMenu(event, asset)}
                              aria-label={`Abrir QR, enlace e historial de ${asset.name}`}
                              sx={{ textTransform: 'none' }}
                            >
                              {INVENTORY_ROW_SECONDARY_ACTIONS_LABEL}
                            </Button>
                          </Stack>
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
        onCheckoutPhotoSelect={(file) => void uploadCheckoutPhoto(file)}
        checkoutPhotoUploading={checkoutPhotoUploading}
      />

      <CheckinDialog
        open={dialogOpen === 'checkin'}
        onClose={() => setDialogOpen(null)}
        asset={selected}
        form={checkinForm}
        onFormChange={setCheckinForm}
        onSubmit={() => selected && checkinMutation.mutate({ assetId: selected.assetId, payload: checkinForm })}
        loading={checkinMutation.isPending}
        onCheckinPhotoSelect={(file) => void uploadCheckinPhoto(file)}
        checkinPhotoUploading={checkinPhotoUploading}
      />

      <Dialog open={dialogOpen === 'qr'} onClose={() => setDialogOpen(null)} maxWidth="sm" fullWidth>
        <DialogTitle>QR de equipo</DialogTitle>
        <DialogContent sx={{ textAlign: 'center' }}>
          <Stack spacing={2} alignItems="center">
            {qrDataUrl ? (
              <img src={qrDataUrl} alt="QR" style={{ width: '100%', maxWidth: 320 }} />
            ) : (
              <Typography variant="body2">Generando QR…</Typography>
            )}
            <TextField
              label="Enlace público"
              value={qrShareUrl}
              fullWidth
              InputProps={{ readOnly: true }}
            />
            {selected?.qrToken && (
              <Typography variant="body2">
                Token: {selected.qrToken}
              </Typography>
            )}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button
            onClick={async () => {
              if (!qrShareUrl) return;
              try {
                await navigator.clipboard.writeText(qrShareUrl);
                setFeedback('Enlace público copiado.');
              } catch {
                setFeedback(qrShareUrl);
              }
            }}
            disabled={!qrShareUrl}
          >
            Copiar enlace
          </Button>
          <Button onClick={() => qrShareUrl && window.open(qrShareUrl, '_blank')} disabled={!qrShareUrl}>
            Abrir
          </Button>
          <Button onClick={() => setDialogOpen(null)}>Cerrar</Button>
        </DialogActions>
      </Dialog>

      <Menu
        anchorEl={actionsMenuTarget?.anchorEl ?? null}
        open={Boolean(actionsMenuTarget)}
        onClose={closeActionsMenu}
      >
        <MenuItem onClick={() => runAssetMenuAction((asset) => void openQr(asset))}>
          Ver QR
        </MenuItem>
        <MenuItem onClick={() => runAssetMenuAction((asset) => { void copyShareUrl(asset); })}>
          Copiar enlace
        </MenuItem>
        <MenuItem onClick={() => runAssetMenuAction(openHistory)}>
          Historial
        </MenuItem>
      </Menu>

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
                    <TableCell>Tipo</TableCell>
                    <TableCell>Salida</TableCell>
                    <TableCell>Devuelto</TableCell>
                    <TableCell>Destino</TableCell>
                    <TableCell>Estado visual</TableCell>
                    <TableCell>Acuerdo</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {history.map((h) => (
                    <TableRow key={h.checkoutId}>
                      <TableCell>{getCheckoutDispositionLabel(h.disposition)}</TableCell>
                      <TableCell>{formatDate(h.checkedOutAt)}</TableCell>
                      <TableCell>{h.returnedAt ? formatDate(h.returnedAt) : '—'}</TableCell>
                      <TableCell>
                        <Stack spacing={0.25}>
                          <Typography variant="body2">
                            {formatCheckoutTargetDisplay(
                              h.targetKind,
                              h.targetPartyRef ?? h.targetRoomId ?? h.targetSessionId,
                              roomMap,
                            )}
                          </Typography>
                          {(h.holderEmail || h.holderPhone) && (
                            <Typography variant="caption" color="text.secondary">
                              {[h.holderEmail, h.holderPhone].filter(Boolean).join(' · ')}
                            </Typography>
                          )}
                        </Stack>
                      </TableCell>
                      <TableCell>
                        {h.photoOutUrl || h.photoInUrl ? (
                          <Stack direction="row" spacing={1}>
                            {h.photoOutUrl && (
                              <Button size="small" href={h.photoOutUrl} target="_blank" rel="noreferrer">
                                Salida
                              </Button>
                            )}
                            {h.photoInUrl && (
                              <Button size="small" href={h.photoInUrl} target="_blank" rel="noreferrer">
                                Retorno
                              </Button>
                            )}
                          </Stack>
                        ) : '—'}
                      </TableCell>
                      <TableCell>
                        <Stack spacing={0.25}>
                          {h.dueAt && (
                            <Typography variant="caption" color="text.secondary">
                              Retorno pactado: {formatDate(h.dueAt)}
                            </Typography>
                          )}
                          {formatCheckoutPaymentSummary(
                            h.paymentType,
                            h.paymentInstallments,
                            h.paymentAmountCents,
                            h.paymentCurrency,
                            h.paymentOutstandingCents,
                          ) && (
                            <Typography variant="caption" color="text.secondary">
                              Pago:{' '}
                              {formatCheckoutPaymentSummary(
                                h.paymentType,
                                h.paymentInstallments,
                                h.paymentAmountCents,
                                h.paymentCurrency,
                                h.paymentOutstandingCents,
                              )}
                            </Typography>
                          )}
                          {h.paymentReference && (
                            <Typography variant="caption" color="text.secondary">
                              Referencia: {h.paymentReference}
                            </Typography>
                          )}
                          {h.termsAndConditions && (
                            <Typography variant="caption" color="text.secondary">
                              Términos: {h.termsAndConditions}
                            </Typography>
                          )}
                          <Typography variant="body2">{h.notes ?? '—'}</Typography>
                        </Stack>
                      </TableCell>
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
