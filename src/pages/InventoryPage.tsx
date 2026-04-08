import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  IconButton,
  Menu,
  MenuItem,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import QrCode2Icon from '@mui/icons-material/QrCode2';
import EditIcon from '@mui/icons-material/Edit';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import FileDownloadIcon from '@mui/icons-material/FileDownload';
import UploadFileIcon from '@mui/icons-material/UploadFile';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useForm, Controller } from 'react-hook-form';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { Inventory } from '../api/inventory';
import type { AssetCreate, AssetDTO, AssetUpdate, Page } from '../api/types';

const createSchema = z.object({
  cName: z.string().min(2, 'Nombre requerido'),
  cCategory: z.string().min(2, 'Categoría requerida'),
});

type CreateForm = z.infer<typeof createSchema>;

type StatusOption = {
  value: string;
  label: string;
};

const STATUS_OPTIONS: StatusOption[] = [
  { value: 'Active', label: 'Activo' },
  { value: 'Booked', label: 'Reservado' },
  { value: 'OutForMaintenance', label: 'Mantenimiento' },
  { value: 'Retired', label: 'Retirado' },
];

function CreateAssetDialog({ open, onClose }: { open: boolean; onClose: () => void }) {
  const qc = useQueryClient();
  const { register, handleSubmit, reset, formState: { errors } } = useForm<CreateForm>({
    resolver: zodResolver(createSchema),
    defaultValues: { cName: '', cCategory: '' },
  });

  const mutation = useMutation({
    mutationFn: (payload: AssetCreate) => Inventory.create(payload),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['inventory'] });
      reset();
      onClose();
    },
  });

  const submit = (values: CreateForm) => {
    mutation.mutate({
      cName: values.cName.trim(),
      cCategory: values.cCategory.trim(),
    });
  };

  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>Agregar activo</DialogTitle>
      <DialogContent>
        <Stack spacing={2} sx={{ mt: 1 }}>
          <TextField
            label="Nombre"
            {...register('cName')}
            error={!!errors.cName}
            helperText={errors.cName?.message}
          />
          <TextField
            label="Categoría"
            {...register('cCategory')}
            error={!!errors.cCategory}
            helperText={errors.cCategory?.message}
          />
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cancelar</Button>
        <Button variant="contained" onClick={handleSubmit(submit)} disabled={mutation.isPending}>
          {mutation.isPending ? 'Guardando…' : 'Guardar'}
        </Button>
      </DialogActions>
    </Dialog>
  );
}

type EditDialogProps = {
  asset: AssetDTO | null;
  open: boolean;
  onClose: () => void;
};

function EditAssetDialog({ asset, open, onClose }: EditDialogProps) {
  const qc = useQueryClient();
  const { register, handleSubmit, reset, control } = useForm<AssetUpdate>({
    defaultValues: {
      uName: asset?.name ?? '',
      uCategory: asset?.category ?? '',
      uStatus: asset?.status ?? 'Active',
      uNotes: undefined,
    },
  });

  useEffect(() => {
    reset({
      uName: asset?.name ?? '',
      uCategory: asset?.category ?? '',
      uStatus: asset?.status ?? 'Active',
      uNotes: undefined,
    });
  }, [asset, reset]);

  const mutation = useMutation({
    mutationFn: (payload: AssetUpdate) => asset ? Inventory.update(asset.assetId, payload) : Promise.resolve(undefined),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['inventory'] });
      onClose();
    },
  });

  const submit = (values: AssetUpdate) => {
    const payload: AssetUpdate = {
      ...values,
    };
    if (payload.uNotes !== undefined) {
      const trimmed = typeof payload.uNotes === 'string' ? payload.uNotes.trim() : payload.uNotes;
      payload.uNotes = trimmed && trimmed.length > 0 ? trimmed : null;
    }
    mutation.mutate(payload);
  };

  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>Editar activo</DialogTitle>
      <DialogContent>
        {asset && (
          <Stack spacing={2} sx={{ mt: 1 }}>
            <TextField label="Nombre" {...register('uName')} defaultValue={asset.name} />
            <TextField label="Categoría" {...register('uCategory')} defaultValue={asset.category} />
            <Controller
              name="uStatus"
              control={control}
              render={({ field }) => (
                <TextField select label="Estado" value={field.value ?? asset?.status ?? 'Active'} onChange={field.onChange}>
                  {STATUS_OPTIONS.map(option => (
                    <MenuItem key={option.value} value={option.value}>{option.label}</MenuItem>
                  ))}
                </TextField>
              )}
            />
            <TextField
              label="Notas"
              multiline
              minRows={3}
              {...register('uNotes')}
            />
          </Stack>
        )}
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cerrar</Button>
        <Button variant="contained" onClick={handleSubmit(submit)} disabled={mutation.isPending}>
          {mutation.isPending ? 'Guardando…' : 'Guardar cambios'}
        </Button>
      </DialogActions>
    </Dialog>
  );
}

function downloadCsv(rows: AssetDTO[]) {
  const header = ['ID', 'Nombre', 'Categoría', 'Estado', 'Ubicación'];
  const data = rows.map(asset => [asset.assetId, asset.name, asset.category, asset.status, asset.location ?? '']);
  const csv = [header, ...data].map(line => line.map(value => `"${String(value).replace(/"/g, '""')}"`).join(',')).join('\n');
  const blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });
  const url = URL.createObjectURL(blob);
  const link = document.createElement('a');
  link.href = url;
  link.download = `tdf-inventory-${Date.now()}.csv`;
  link.click();
  URL.revokeObjectURL(url);
}

function printQr(asset: AssetDTO) {
  const qrUrl = `https://api.qrserver.com/v1/create-qr-code/?size=180x180&data=${encodeURIComponent(asset.assetId)}`;
  const win = window.open('', '_blank');
  if (!win) return;
  win.document.write(`<html><body style="font-family: sans-serif; text-align: center;">
    <h1>${asset.name}</h1>
    <img src="${qrUrl}" alt="QR" />
    <p>${asset.assetId}</p>
  </body></html>`);
  win.document.close();
  win.print();
}

export default function InventoryPage() {
  const [createOpen, setCreateOpen] = useState(false);
  const [editing, setEditing] = useState<AssetDTO | null>(null);
  const [menuAsset, setMenuAsset] = useState<AssetDTO | null>(null);
  const [anchorEl, setAnchorEl] = useState<null | HTMLElement>(null);
  const [query, setQuery] = useState('');
  const [debouncedQuery, setDebouncedQuery] = useState('');
  const [feedback, setFeedback] = useState<string | null>(null);
  const qc = useQueryClient();

  useEffect(() => {
    const timeout = window.setTimeout(() => setDebouncedQuery(query.trim()), 300);
    return () => window.clearTimeout(timeout);
  }, [query]);

  const assetsQuery = useQuery<Page<AssetDTO>>({
    queryKey: ['inventory', debouncedQuery],
    queryFn: () => Inventory.list(debouncedQuery ? { q: debouncedQuery } : undefined),
  });

  const deleteMutation = useMutation({
    mutationFn: (assetId: string) => Inventory.remove(assetId),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['inventory'] });
      setFeedback('Activo eliminado');
    },
  });

  const assetsData = assetsQuery.data;
  const assets: AssetDTO[] = assetsData?.items ?? [];

  const handleDelete = (asset: AssetDTO) => {
    deleteMutation.mutate(asset.assetId);
  };

  return (
    <Stack spacing={2}>
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" alignItems={{ xs: 'stretch', md: 'center' }} spacing={2}>
        <Typography variant="h5">Inventario</Typography>
        <Stack direction="row" spacing={1}>
          <Button variant="outlined" startIcon={<UploadFileIcon />} disabled>
            Importar CSV
          </Button>
          <Button variant="outlined" startIcon={<FileDownloadIcon />} onClick={() => downloadCsv(assets)}>
            Exportar CSV
          </Button>
          <Button variant="contained" onClick={() => setCreateOpen(true)}>Agregar activo</Button>
        </Stack>
      </Stack>
      {feedback && (
        <Alert severity="success" onClose={() => setFeedback(null)}>{feedback}</Alert>
      )}
      <Paper variant="outlined">
        <Box sx={{ p: 2, display: 'flex', flexDirection: 'column', gap: 2 }}>
          <TextField
            label="Buscar"
            size="small"
            placeholder="Filtra por nombre, categoría o sala"
            value={query}
            onChange={(event) => setQuery(event.target.value)}
          />
          <TableContainer>
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Nombre</TableCell>
                  <TableCell>Categoría</TableCell>
                  <TableCell>Estado</TableCell>
                  <TableCell>Ubicación</TableCell>
                  <TableCell width={120}>Acciones</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {assets.map(asset => (
                  <TableRow key={asset.assetId} hover>
                    <TableCell>{asset.name}</TableCell>
                    <TableCell>{asset.category}</TableCell>
                    <TableCell>{asset.status}</TableCell>
                    <TableCell>{asset.location ?? '—'}</TableCell>
                    <TableCell>
                      <Tooltip title="Editar activo">
                        <IconButton size="small" onClick={() => setEditing(asset)}>
                          <EditIcon fontSize="small" />
                        </IconButton>
                      </Tooltip>
                      <Tooltip title="Eliminar">
                        <IconButton size="small" onClick={() => handleDelete(asset)}>
                          <DeleteOutlineIcon fontSize="small" />
                        </IconButton>
                      </Tooltip>
                      <Tooltip title="Imprimir QR">
                        <IconButton size="small" onClick={(event) => { setAnchorEl(event.currentTarget); setMenuAsset(asset); }}>
                          <QrCode2Icon fontSize="small" />
                        </IconButton>
                      </Tooltip>
                    </TableCell>
                  </TableRow>
                ))}
                {assets.length === 0 && (
                  <TableRow>
                    <TableCell colSpan={5}>
                      <Typography variant="body2" color="text.secondary" align="center" sx={{ py: 2 }}>
                        No se encontraron activos.
                      </Typography>
                    </TableCell>
                  </TableRow>
                )}
              </TableBody>
            </Table>
          </TableContainer>
        </Box>
      </Paper>

      <CreateAssetDialog open={createOpen} onClose={() => setCreateOpen(false)} />
      <EditAssetDialog asset={editing} open={!!editing} onClose={() => setEditing(null)} />

      <Menu
        anchorEl={anchorEl}
        open={Boolean(anchorEl)}
        onClose={() => {
          setAnchorEl(null);
          setMenuAsset(null);
        }}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
      >
        <MenuItem
          onClick={() => {
            if (menuAsset) {
              printQr(menuAsset);
            }
            setAnchorEl(null);
            setMenuAsset(null);
          }}
        >
          Imprimir QR
        </MenuItem>
        <MenuItem
          onClick={() => {
            if (menuAsset) {
              setEditing(menuAsset);
            }
            setAnchorEl(null);
            setMenuAsset(null);
          }}
        >
          Editar…
        </MenuItem>
      </Menu>
    </Stack>
  );
}
