import { useMemo, useState } from 'react';
import { Link as RouterLink } from 'react-router-dom';
import {
  Alert,
  Box,
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControl,
  FormControlLabel,
  InputAdornment,
  InputLabel,
  MenuItem,
  Paper,
  Select,
  Stack,
  Switch,
  Tab,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Tabs,
  TextField,
  Typography,
} from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useForm, Controller } from 'react-hook-form';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { PackagesApi } from '../api/packages';
import type {
  PackageProductCreate,
  PackageProductDTO,
  PackageProductUpdate,
  PackagePurchaseReq,
} from '../api/types';

const SERVICE_KIND_OPTIONS = ['Recording', 'Mixing', 'Mastering', 'Rehearsal', 'Classes', 'EventProduction'];
const UNITS_KIND_OPTIONS = ['Hours', 'Lessons', 'Credits', 'Sessions'];

const productSchema = z.object({
  name: z.string().min(2, 'Ingresa un nombre'),
  serviceKind: z.enum(['Recording', 'Mixing', 'Mastering', 'Rehearsal', 'Classes', 'EventProduction']),
  unitsKind: z.enum(['Hours', 'Lessons', 'Credits', 'Sessions']),
  unitsQty: z.coerce.number().int().positive('Cantidad inválida'),
  price: z.coerce.number().min(0, 'Precio inválido'),
  taxBps: z.coerce.number().int().min(0).max(3000).optional(),
  active: z.boolean().default(true),
});

type ProductFormValues = z.infer<typeof productSchema>;

const purchaseSchema = z.object({
  buyerId: z.coerce.number().int().positive('ID inválido'),
  productId: z.coerce.number().int().positive('Selecciona un paquete'),
  autoInvoice: z.boolean().default(true),
});

type PurchaseFormValues = z.infer<typeof purchaseSchema>;

type ProductDialogProps = {
  open: boolean;
  initial?: PackageProductDTO | null;
  onClose: () => void;
};

function centsToCurrency(cents: number) {
  return (cents / 100).toFixed(2);
}

function ProductDialog({ open, onClose, initial }: ProductDialogProps) {
  const qc = useQueryClient();
  const { control, handleSubmit, reset, formState: { errors } } = useForm<ProductFormValues>({
    resolver: zodResolver(productSchema),
    defaultValues: initial ? {
      name: initial.ppName,
      serviceKind: initial.ppService as ProductFormValues['serviceKind'],
      unitsKind: initial.ppUnitsKind as ProductFormValues['unitsKind'],
      unitsQty: initial.ppUnitsQty,
      price: Number(centsToCurrency(initial.ppPriceCents)),
      taxBps: undefined,
      active: true,
    } : {
      name: '',
      serviceKind: 'Recording',
      unitsKind: 'Hours',
      unitsQty: 10,
      price: 0,
      taxBps: undefined,
      active: true,
    },
  });

  const [modalError, setModalError] = useState<string | null>(null);

  const createMutation = useMutation({
    mutationFn: (body: PackageProductCreate) => PackagesApi.createProduct(body),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['packages', 'products'] });
      onClose();
      reset();
      setModalError(null);
    },
    onError: (error: unknown) => {
      setModalError(error instanceof Error ? error.message : 'No se pudo crear el paquete');
    },
  });

  const updateMutation = useMutation({
    mutationFn: (payload: { id: number; body: PackageProductUpdate }) => PackagesApi.updateProduct(payload.id, payload.body),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['packages', 'products'] });
      onClose();
      setModalError(null);
    },
    onError: (error: unknown) => {
      setModalError(error instanceof Error ? error.message : 'No se pudo actualizar el paquete');
    },
  });

  const submit = (values: ProductFormValues) => {
    const payload: PackageProductCreate = {
      name: values.name.trim(),
      serviceKind: values.serviceKind,
      unitsKind: values.unitsKind,
      unitsQty: values.unitsQty,
      priceCents: Math.round(values.price * 100),
      taxBps: values.taxBps,
      active: values.active,
    };
    if (initial) {
      updateMutation.mutate({ id: initial.ppId, body: payload });
    } else {
      createMutation.mutate(payload);
    }
  };

  const isSaving = createMutation.isPending || updateMutation.isPending;

  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>{initial ? 'Editar paquete' : 'Nuevo paquete'}</DialogTitle>
      <DialogContent>
        <Stack spacing={2} sx={{ mt: 1 }}>
          {modalError && <Alert severity="error">{modalError}</Alert>}
          <Controller
            name="name"
            control={control}
            render={({ field }) => (
              <TextField
                label="Nombre comercial"
                {...field}
                error={!!errors.name}
                helperText={errors.name?.message}
              />
            )}
          />
          <Controller
            name="serviceKind"
            control={control}
            render={({ field }) => (
              <FormControl fullWidth>
                <InputLabel id="service-label">Servicio</InputLabel>
                <Select
                  {...field}
                  labelId="service-label"
                  label="Servicio"
                >
                  {SERVICE_KIND_OPTIONS.map(option => (
                    <MenuItem key={option} value={option}>{option}</MenuItem>
                  ))}
                </Select>
              </FormControl>
            )}
          />
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
            <Controller
              name="unitsKind"
              control={control}
              render={({ field }) => (
                <FormControl fullWidth>
                  <InputLabel id="units-label">Unidades</InputLabel>
                  <Select
                    {...field}
                    labelId="units-label"
                    label="Unidades"
                  >
                    {UNITS_KIND_OPTIONS.map(option => (
                      <MenuItem key={option} value={option}>{option}</MenuItem>
                    ))}
                  </Select>
                </FormControl>
              )}
            />
            <Controller
              name="unitsQty"
              control={control}
              render={({ field }) => (
                <TextField
                  label="Cantidad"
                  type="number"
                  {...field}
                  error={!!errors.unitsQty}
                  helperText={errors.unitsQty?.message}
                />
              )}
            />
          </Stack>
          <Controller
            name="price"
            control={control}
            render={({ field }) => (
              <TextField
                label="Precio"
                type="number"
                InputProps={{
                  startAdornment: <InputAdornment position="start">$</InputAdornment>,
                }}
                {...field}
                error={!!errors.price}
                helperText={errors.price?.message}
              />
            )}
          />
          <Controller
            name="taxBps"
            control={control}
            render={({ field }) => (
              <TextField
                label="Impuesto (bps)"
                type="number"
                {...field}
                error={!!errors.taxBps}
                helperText={errors.taxBps?.message ?? '0 = sin IVA, 1200 = 12%'}
              />
            )}
          />
          <Controller
            name="active"
            control={control}
            render={({ field }) => (
              <FormControlLabel
                control={(
                  <Switch
                    checked={field.value}
                    onChange={(event) => field.onChange(event.target.checked)}
                  />
                )}
                label="Disponible para venta"
              />
            )}
          />
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cancelar</Button>
        <Button variant="contained" onClick={handleSubmit(submit)} disabled={isSaving}>
          {isSaving ? 'Guardando…' : 'Guardar'}
        </Button>
      </DialogActions>
    </Dialog>
  );
}

export default function PackagesPage() {
  const [tab, setTab] = useState<'catalog' | 'purchases'>('catalog');
  const [dialogOpen, setDialogOpen] = useState(false);
  const [editingProduct, setEditingProduct] = useState<PackageProductDTO | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  const [errorMessage, setErrorMessage] = useState<string | null>(null);
  const qc = useQueryClient();

  const productsQuery = useQuery({
    queryKey: ['packages', 'products'],
    queryFn: PackagesApi.listProducts,
  });

  const purchaseForm = useForm<PurchaseFormValues>({
    resolver: zodResolver(purchaseSchema),
    defaultValues: {
      autoInvoice: true,
    },
  });

  const purchaseMutation = useMutation({
    mutationFn: (payload: PackagePurchaseReq) => PackagesApi.createPurchase(payload),
    onSuccess: () => {
      setSuccessMessage('Compra registrada y asociada correctamente.');
      purchaseForm.reset({ autoInvoice: true, buyerId: 0, productId: 0 });
      setErrorMessage(null);
    },
    onError: (error: unknown) => {
      setErrorMessage(error instanceof Error ? error.message : 'No se pudo registrar la compra');
    },
  });

  const products = productsQuery.data ?? [];

  const activeProducts = useMemo(() => products.filter(p => !!p), [products]);

  const onCreate = () => {
    setEditingProduct(null);
    setDialogOpen(true);
  };

  const onEdit = (product: PackageProductDTO) => {
    setEditingProduct(product);
    setDialogOpen(true);
  };

  const submitPurchase = (values: PurchaseFormValues) => {
    const payload: PackagePurchaseReq = {
      buyerId: values.buyerId,
      productId: values.productId,
    };
    purchaseMutation.mutate(payload);
  };

  return (
    <Stack spacing={2}>
      <Stack
        direction={{ xs: 'column', sm: 'row' }}
        alignItems={{ xs: 'stretch', sm: 'center' }}
        justifyContent="space-between"
        spacing={1}
      >
        <Typography variant="h5">Paquetes</Typography>
        <Stack direction="row" spacing={1} justifyContent="flex-end">
          <Button component={RouterLink} to="resumen" variant="outlined">
            Vista resumida
          </Button>
          {tab === 'catalog' ? (
            <Button variant="contained" onClick={onCreate}>Nuevo producto</Button>
          ) : (
            <Button
              variant="contained"
              onClick={purchaseForm.handleSubmit(submitPurchase)}
              disabled={purchaseMutation.isPending}
            >
              {purchaseMutation.isPending ? 'Registrando…' : 'Registrar compra'}
            </Button>
          )}
        </Stack>
      </Stack>
      {successMessage && (
        <Alert severity="success" onClose={() => setSuccessMessage(null)}>{successMessage}</Alert>
      )}
      <Paper variant="outlined">
        <Tabs value={tab} onChange={(_event, value) => setTab(value)} variant="fullWidth">
          <Tab label="Catálogo" value="catalog" />
          <Tab label="Compras" value="purchases" />
        </Tabs>
        {tab === 'catalog' ? (
          <TableContainer>
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Producto</TableCell>
                  <TableCell>Servicio</TableCell>
                  <TableCell>Unidades</TableCell>
                  <TableCell>Precio</TableCell>
                  <TableCell width={120}>Acciones</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {activeProducts.map(product => (
                  <TableRow key={product.ppId} hover>
                    <TableCell>{product.ppName}</TableCell>
                    <TableCell>{product.ppService}</TableCell>
                    <TableCell>{product.ppUnitsQty} {product.ppUnitsKind}</TableCell>
                    <TableCell>${centsToCurrency(product.ppPriceCents)}</TableCell>
                    <TableCell>
                      <Button size="small" variant="outlined" onClick={() => onEdit(product)}>
                        Editar
                      </Button>
                    </TableCell>
                  </TableRow>
                ))}
                {activeProducts.length === 0 && (
                  <TableRow>
                    <TableCell colSpan={5}>
                      <Typography variant="body2" color="text.secondary" align="center" sx={{ py: 2 }}>
                        Aún no hay productos creados.
                      </Typography>
                    </TableCell>
                  </TableRow>
                )}
              </TableBody>
            </Table>
          </TableContainer>
        ) : (
          <Box component="form" onSubmit={purchaseForm.handleSubmit(submitPurchase)} sx={{ p: 2, display: 'grid', gap: 2 }}>
            {errorMessage && <Alert severity="error">{errorMessage}</Alert>}
            <TextField
              label="ID del cliente (PartyId)"
              type="number"
              {...purchaseForm.register('buyerId')}
              error={!!purchaseForm.formState.errors.buyerId}
              helperText={purchaseForm.formState.errors.buyerId?.message}
              required
            />
            <FormControl fullWidth>
              <InputLabel id="product-select">Paquete</InputLabel>
              <Select
                labelId="product-select"
                label="Paquete"
                value={purchaseForm.watch('productId') || ''}
                onChange={(event) => purchaseForm.setValue('productId', Number(event.target.value))}
                required
              >
                <MenuItem value="" disabled>
                  Selecciona un paquete
                </MenuItem>
                {activeProducts.map(product => (
                  <MenuItem key={product.ppId} value={product.ppId}>
                    {product.ppName} · ${centsToCurrency(product.ppPriceCents)}
                  </MenuItem>
                ))}
              </Select>
              {purchaseForm.formState.errors.productId && (
                <Typography variant="caption" color="error">
                  {purchaseForm.formState.errors.productId.message}
                </Typography>
              )}
            </FormControl>
            <FormControlLabel
              control={(<Switch
                checked={purchaseForm.watch('autoInvoice')}
                onChange={(event) => purchaseForm.setValue('autoInvoice', event.target.checked)}
              />)}
              label="Crear factura automática"
            />
            <Typography variant="body2" color="text.secondary">
              La compra generará movimientos en los saldos del paquete y, si está habilitado, emitirá una factura borrador ligada al booking o cliente.
            </Typography>
          </Box>
        )}
      </Paper>

      <ProductDialog open={dialogOpen} onClose={() => setDialogOpen(false)} initial={editingProduct} />
    </Stack>
  );
}
