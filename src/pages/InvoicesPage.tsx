import { useMemo, useState } from 'react';
import {
  Alert,
  Button,
  Checkbox,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControlLabel,
  IconButton,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TextField,
  Typography,
} from '@mui/material';
import DeleteIcon from '@mui/icons-material/DeleteOutline';
import AddIcon from '@mui/icons-material/Add';
import { Link as RouterLink } from 'react-router-dom';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { Controller, useFieldArray, useForm } from 'react-hook-form';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Invoices } from '../api/invoices';
import type { CreateInvoiceReq, InvoiceDTO } from '../api/types';

const lineItemSchema = z.object({
  description: z.string().min(1, 'Requerido'),
  quantity: z.coerce.number().int().positive('Cantidad inválida'),
  unitPrice: z.coerce.number().min(0, 'Precio inválido'),
  taxPercent: z.coerce.number().min(0, 'IVA inválido').max(100, 'Máximo 100%').optional(),
});

const schema = z.object({
  ciCustomerId: z.coerce.number().int().positive('ID inválido'),
  currency: z.string().optional(),
  ciNumber: z.string().optional(),
  notes: z.string().optional(),
  generateReceipt: z.boolean().default(true),
  lineItems: z.array(lineItemSchema).min(1, 'Agrega al menos un concepto'),
});

type FormValues = z.infer<typeof schema>;

type CreateInvoiceDialogProps = {
  open: boolean;
  onClose: () => void;
  onCreated: (invoice: InvoiceDTO) => void;
};

function formatAmount(cents: number, currency: string) {
  const amount = cents / 100;
  try {
    return new Intl.NumberFormat('es-EC', { style: 'currency', currency }).format(amount);
  } catch {
    return `${amount.toFixed(2)} ${currency}`;
  }
}

function CreateInvoiceDialog({ open, onClose, onCreated }: CreateInvoiceDialogProps) {
  const qc = useQueryClient();
  const {
    control,
    register,
    handleSubmit,
    watch,
    reset,
    formState: { errors },
  } = useForm<FormValues>({
    resolver: zodResolver(schema),
    defaultValues: {
      ciCustomerId: 0,
      currency: 'USD',
      ciNumber: '',
      notes: '',
      generateReceipt: true,
      lineItems: [{ description: '', quantity: 1, unitPrice: 0, taxPercent: 0 }],
    },
  });

  const { fields, append, remove } = useFieldArray({ control, name: 'lineItems' });

  const mutation = useMutation({
    mutationFn: (payload: CreateInvoiceReq) => Invoices.create(payload),
    onSuccess: (invoice) => {
      qc.invalidateQueries({ queryKey: ['invoices'] });
      reset({
        ciCustomerId: 0,
        currency: 'USD',
        ciNumber: '',
        notes: '',
        generateReceipt: true,
        lineItems: [{ description: '', quantity: 1, unitPrice: 0, taxPercent: 0 }],
      });
      onCreated(invoice);
      onClose();
    },
  });

  const watchedItems = watch('lineItems');
  const currency = watch('currency')?.trim().toUpperCase() || 'USD';

  const { subtotal, tax, total } = useMemo(() => {
    if (!watchedItems || watchedItems.length === 0) {
      return { subtotal: 0, tax: 0, total: 0 };
    }
    const subtotalValue = watchedItems.reduce((acc, item) => {
      const quantity = Number(item.quantity) || 0;
      const price = Number(item.unitPrice) || 0;
      return acc + quantity * price;
    }, 0);
    const taxValue = watchedItems.reduce((acc, item) => {
      const quantity = Number(item.quantity) || 0;
      const price = Number(item.unitPrice) || 0;
      const rate = Number(item.taxPercent) || 0;
      return acc + quantity * price * (rate / 100);
    }, 0);
    return { subtotal: subtotalValue, tax: taxValue, total: subtotalValue + taxValue };
  }, [watchedItems]);

  const submit = (values: FormValues) => {
    const lineItems = values.lineItems.map((item) => {
      const quantity = Number(item.quantity);
      const unitCents = Math.round(Number(item.unitPrice) * 100);
      const taxPercent = Number(item.taxPercent ?? 0);
      return {
        cilDescription: item.description.trim(),
        cilQuantity: quantity,
        cilUnitCents: unitCents,
        cilTaxBps: Math.round(taxPercent * 100),
      };
    });

    const payload: CreateInvoiceReq = {
      ciCustomerId: values.ciCustomerId,
      ciCurrency: values.currency?.trim().toUpperCase() || 'USD',
      ciNumber: values.ciNumber?.trim() ? values.ciNumber.trim() : undefined,
      ciNotes: values.notes?.trim() ? values.notes.trim() : undefined,
      ciLineItems: lineItems,
      ciGenerateReceipt: values.generateReceipt,
    };

    mutation.mutate(payload);
  };

  return (
    <Dialog open={open} onClose={onClose} maxWidth="md" fullWidth>
      <DialogTitle>Nueva factura</DialogTitle>
      <DialogContent>
        <Stack spacing={3} sx={{ mt: 1 }}>
          <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
            <TextField
              label="ID del cliente (PartyId)"
              type="number"
              fullWidth
              {...register('ciCustomerId')}
              error={!!errors.ciCustomerId}
              helperText={errors.ciCustomerId?.message}
              required
            />
            <TextField
              label="Moneda"
              fullWidth
              {...register('currency')}
              error={!!errors.currency}
              helperText={errors.currency?.message ?? 'Ej: USD'}
            />
          </Stack>

          <Stack spacing={2}>
            <Typography variant="subtitle1">Conceptos</Typography>
            <Stack spacing={2}>
              {fields.map((field, index) => (
                <Stack
                  key={field.id}
                  direction={{ xs: 'column', md: 'row' }}
                  spacing={2}
                  alignItems={{ xs: 'stretch', md: 'flex-end' }}
                >
                  <TextField
                    label="Descripción"
                    fullWidth
                    {...register(`lineItems.${index}.description` as const)}
                    error={!!errors.lineItems?.[index]?.description}
                    helperText={errors.lineItems?.[index]?.description?.message}
                  />
                  <TextField
                    label="Cantidad"
                    type="number"
                    sx={{ width: { xs: '100%', md: 130 } }}
                    {...register(`lineItems.${index}.quantity` as const)}
                    error={!!errors.lineItems?.[index]?.quantity}
                    helperText={errors.lineItems?.[index]?.quantity?.message}
                  />
                  <TextField
                    label={`Precio (${currency})`}
                    type="number"
                    sx={{ width: { xs: '100%', md: 180 } }}
                    {...register(`lineItems.${index}.unitPrice` as const)}
                    error={!!errors.lineItems?.[index]?.unitPrice}
                    helperText={errors.lineItems?.[index]?.unitPrice?.message}
                  />
                  <TextField
                    label="IVA (%)"
                    type="number"
                    sx={{ width: { xs: '100%', md: 140 } }}
                    {...register(`lineItems.${index}.taxPercent` as const)}
                    error={!!errors.lineItems?.[index]?.taxPercent}
                    helperText={errors.lineItems?.[index]?.taxPercent?.message}
                  />
                  <IconButton
                    aria-label="Eliminar concepto"
                    onClick={() => remove(index)}
                    disabled={fields.length === 1 || mutation.isPending}
                  >
                    <DeleteIcon />
                  </IconButton>
                </Stack>
              ))}
            </Stack>
            <Button
              startIcon={<AddIcon />}
              onClick={() => append({ description: '', quantity: 1, unitPrice: 0, taxPercent: 0 })}
              variant="text"
              disabled={mutation.isPending}
            >
              Agregar concepto
            </Button>
            {errors.lineItems && typeof errors.lineItems.message === 'string' && (
              <Typography color="error" variant="body2">{errors.lineItems.message}</Typography>
            )}
          </Stack>

          <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
            <TextField
              label="Número interno (opcional)"
              fullWidth
              {...register('ciNumber')}
              error={!!errors.ciNumber}
              helperText={errors.ciNumber?.message}
            />
            <TextField
              label="Notas"
              fullWidth
              multiline
              minRows={1}
              {...register('notes')}
              error={!!errors.notes}
              helperText={errors.notes?.message}
            />
          </Stack>

          <Stack spacing={0.5} sx={{ ml: 'auto', minWidth: 260 }}>
            <Stack direction="row" justifyContent="space-between">
              <Typography color="text.secondary">Subtotal</Typography>
              <Typography>{formatAmount(Math.round(subtotal * 100), currency)}</Typography>
            </Stack>
            <Stack direction="row" justifyContent="space-between">
              <Typography color="text.secondary">IVA</Typography>
              <Typography>{formatAmount(Math.round(tax * 100), currency)}</Typography>
            </Stack>
            <Stack direction="row" justifyContent="space-between" alignItems="baseline">
              <Typography fontWeight={600}>Total estimado</Typography>
              <Typography fontWeight={600}>
                {formatAmount(Math.round(total * 100), currency)}
              </Typography>
            </Stack>
          </Stack>

          <Controller
            control={control}
            name="generateReceipt"
            render={({ field }) => (
              <FormControlLabel
                control={<Checkbox {...field} checked={field.value} />}
                label="Emitir recibo automáticamente"
              />
            )}
          />

          {mutation.isError && (
            <Alert severity="error">{(mutation.error as Error).message}</Alert>
          )}
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose} disabled={mutation.isPending}>Cancelar</Button>
        <Button variant="contained" onClick={handleSubmit(submit)} disabled={mutation.isPending}>
          {mutation.isPending ? 'Creando…' : 'Crear'}
        </Button>
      </DialogActions>
    </Dialog>
  );
}

export default function InvoicesPage() {
  const [dialogOpen, setDialogOpen] = useState(false);
  const [createdInvoice, setCreatedInvoice] = useState<InvoiceDTO | null>(null);
  const invoicesQuery = useQuery({ queryKey: ['invoices'], queryFn: Invoices.list });

  const invoices = invoicesQuery.data ?? [];

  return (
    <Stack spacing={2}>
      <Stack direction="row" alignItems="center" justifyContent="space-between">
        <Typography variant="h5">Facturación</Typography>
        <Button variant="contained" onClick={() => setDialogOpen(true)}>Nueva factura</Button>
      </Stack>

      {createdInvoice && (
        <Alert severity="success" onClose={() => setCreatedInvoice(null)}>
          Factura #{createdInvoice.number ?? createdInvoice.invId} creada correctamente.
          {createdInvoice.receiptId != null && (
            <Button
              component={RouterLink}
              to={`/finance/receipts/${createdInvoice.receiptId}`}
              size="small"
              sx={{ ml: 1 }}
            >
              Ver recibo
            </Button>
          )}
        </Alert>
      )}

      <Paper variant="outlined">
        <TableContainer>
          <Table size="small">
            <TableHead>
              <TableRow>
                <TableCell>#</TableCell>
                <TableCell>Total</TableCell>
                <TableCell>Estado</TableCell>
                <TableCell>Subtotal</TableCell>
                <TableCell>IVA</TableCell>
                <TableCell>Recibo</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {invoices.map((invoice) => (
                <TableRow key={invoice.invId} hover>
                  <TableCell>{invoice.number ?? invoice.invId}</TableCell>
                  <TableCell>{formatAmount(invoice.totalC, invoice.currency)}</TableCell>
                  <TableCell>
                    <Typography variant="body2" fontWeight={600}>{invoice.statusI}</Typography>
                  </TableCell>
                  <TableCell>{formatAmount(invoice.subtotalC, invoice.currency)}</TableCell>
                  <TableCell>{formatAmount(invoice.taxC, invoice.currency)}</TableCell>
                  <TableCell>
                    {invoice.receiptId != null ? (
                      <Button component={RouterLink} to={`/finance/receipts/${invoice.receiptId}`} size="small">
                        Ver recibo
                      </Button>
                    ) : (
                      <Typography variant="body2" color="text.secondary">—</Typography>
                    )}
                  </TableCell>
                </TableRow>
              ))}
              {invoices.length === 0 && (
                <TableRow>
                  <TableCell colSpan={6}>
                    <Typography variant="body2" color="text.secondary" align="center" sx={{ py: 2 }}>
                      No hay facturas registradas.
                    </Typography>
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </TableContainer>
      </Paper>

      <CreateInvoiceDialog
        open={dialogOpen}
        onClose={() => setDialogOpen(false)}
        onCreated={(invoice) => setCreatedInvoice(invoice)}
      />
    </Stack>
  );
}
