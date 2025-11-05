import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Box,
  Button,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Grid,
  TextField,
  Typography,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
  Chip,
  Alert,
  IconButton,
  FormControl,
  InputLabel,
  Select,
  MenuItem,
} from '@mui/material';
import {
  Add as AddIcon,
  Receipt as ReceiptIcon,
  Payment as PaymentIcon,
} from '@mui/icons-material';
import { Invoices } from '../api/invoices';
import type { InvoiceCreate, PaymentCreate } from '../api/types';

export default function InvoicesPage() {
  const [createOpen, setCreateOpen] = useState(false);
  const [paymentDialogOpen, setPaymentDialogOpen] = useState(false);
  const [selectedInvoiceId, setSelectedInvoiceId] = useState<number | null>(null);
  const queryClient = useQueryClient();

  const { data: invoices = [], isLoading, error } = useQuery({
    queryKey: ['invoices'],
    queryFn: () => Invoices.list(),
  });

  const createMutation = useMutation({
    mutationFn: Invoices.create,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['invoices'] });
      setCreateOpen(false);
    },
  });

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'paid':
        return 'success';
      case 'overdue':
        return 'error';
      case 'issued':
        return 'warning';
      default:
        return 'default';
    }
  };

  if (error) {
    return <Alert severity="error">Failed to load invoices: {String(error)}</Alert>;
  }

  return (
    <Box>
      <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', mb: 3 }}>
        <Typography variant="h4">Invoices</Typography>
        <Button
          variant="contained"
          startIcon={<AddIcon />}
          onClick={() => setCreateOpen(true)}
        >
          Create Invoice
        </Button>
      </Box>

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>Invoice #</TableCell>
              <TableCell>Customer</TableCell>
              <TableCell>Issued</TableCell>
              <TableCell>Due</TableCell>
              <TableCell align="right">Total</TableCell>
              <TableCell align="right">Paid</TableCell>
              <TableCell align="right">Balance</TableCell>
              <TableCell>Status</TableCell>
              <TableCell align="center">Actions</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {invoices.map((invoice) => (
              <TableRow key={invoice.invoiceId}>
                <TableCell>{invoice.invoiceNumber}</TableCell>
                <TableCell>{invoice.partyName}</TableCell>
                <TableCell>{new Date(invoice.issuedAt).toLocaleDateString()}</TableCell>
                <TableCell>{new Date(invoice.dueAt).toLocaleDateString()}</TableCell>
                <TableCell align="right">${invoice.totalUsd.toFixed(2)}</TableCell>
                <TableCell align="right">${invoice.paidUsd.toFixed(2)}</TableCell>
                <TableCell align="right">
                  ${(invoice.totalUsd - invoice.paidUsd).toFixed(2)}
                </TableCell>
                <TableCell>
                  <Chip
                    label={invoice.status}
                    color={getStatusColor(invoice.status)}
                    size="small"
                  />
                </TableCell>
                <TableCell align="center">
                  <IconButton
                    size="small"
                    onClick={() => {
                      setSelectedInvoiceId(invoice.invoiceId);
                      setPaymentDialogOpen(true);
                    }}
                    disabled={invoice.status === 'paid'}
                  >
                    <PaymentIcon />
                  </IconButton>
                  <IconButton size="small">
                    <ReceiptIcon />
                  </IconButton>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>

      <CreateInvoiceDialog
        open={createOpen}
        onClose={() => setCreateOpen(false)}
        onSubmit={(data) => createMutation.mutate(data)}
        isSubmitting={createMutation.isPending}
      />

      {selectedInvoiceId && (
        <PaymentDialog
          open={paymentDialogOpen}
          onClose={() => {
            setPaymentDialogOpen(false);
            setSelectedInvoiceId(null);
          }}
          invoiceId={selectedInvoiceId}
        />
      )}
    </Box>
  );
}

function CreateInvoiceDialog({
  open,
  onClose,
  onSubmit,
  isSubmitting,
}: {
  open: boolean;
  onClose: () => void;
  onSubmit: (data: InvoiceCreate) => void;
  isSubmitting: boolean;
}) {
  const [formData, setFormData] = useState<InvoiceCreate>({
    cPartyId: 0,
    cDueAt: '',
    cItems: [{ description: '', quantity: 1, unitPriceUsd: 0 }],
    cNotes: null,
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    onSubmit(formData);
  };

  const addItem = () => {
    setFormData({
      ...formData,
      cItems: [...formData.cItems, { description: '', quantity: 1, unitPriceUsd: 0 }],
    });
  };

  const updateItem = (index: number, field: string, value: any) => {
    const newItems = [...formData.cItems];
    newItems[index] = { ...newItems[index], [field]: value };
    setFormData({ ...formData, cItems: newItems });
  };

  return (
    <Dialog open={open} onClose={onClose} maxWidth="md" fullWidth>
      <DialogTitle>Create Invoice</DialogTitle>
      <form onSubmit={handleSubmit}>
        <DialogContent>
          <Grid container spacing={2}>
            <Grid item xs={6}>
              <TextField
                fullWidth
                type="number"
                label="Customer ID"
                value={formData.cPartyId || ''}
                onChange={(e) => setFormData({ ...formData, cPartyId: Number(e.target.value) })}
                required
              />
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                type="date"
                label="Due Date"
                value={formData.cDueAt}
                onChange={(e) => setFormData({ ...formData, cDueAt: e.target.value })}
                InputLabelProps={{ shrink: true }}
                required
              />
            </Grid>
            <Grid item xs={12}>
              <Typography variant="h6" sx={{ mt: 2, mb: 1 }}>
                Line Items
              </Typography>
            </Grid>
            {formData.cItems.map((item, idx) => (
              <Grid item xs={12} key={idx}>
                <Grid container spacing={2}>
                  <Grid item xs={6}>
                    <TextField
                      fullWidth
                      label="Description"
                      value={item.description}
                      onChange={(e) => updateItem(idx, 'description', e.target.value)}
                      required
                    />
                  </Grid>
                  <Grid item xs={3}>
                    <TextField
                      fullWidth
                      type="number"
                      label="Quantity"
                      value={item.quantity}
                      onChange={(e) => updateItem(idx, 'quantity', Number(e.target.value))}
                      required
                    />
                  </Grid>
                  <Grid item xs={3}>
                    <TextField
                      fullWidth
                      type="number"
                      label="Unit Price"
                      value={item.unitPriceUsd}
                      onChange={(e) => updateItem(idx, 'unitPriceUsd', Number(e.target.value))}
                      required
                    />
                  </Grid>
                </Grid>
              </Grid>
            ))}
            <Grid item xs={12}>
              <Button onClick={addItem}>Add Line Item</Button>
            </Grid>
            <Grid item xs={12}>
              <TextField
                fullWidth
                multiline
                rows={2}
                label="Notes"
                value={formData.cNotes || ''}
                onChange={(e) => setFormData({ ...formData, cNotes: e.target.value || null })}
              />
            </Grid>
          </Grid>
        </DialogContent>
        <DialogActions>
          <Button onClick={onClose}>Cancel</Button>
          <Button type="submit" variant="contained" disabled={isSubmitting}>
            Create
          </Button>
        </DialogActions>
      </form>
    </Dialog>
  );
}

function PaymentDialog({
  open,
  onClose,
  invoiceId,
}: {
  open: boolean;
  onClose: () => void;
  invoiceId: number;
}) {
  const queryClient = useQueryClient();
  const [formData, setFormData] = useState<PaymentCreate>({
    cInvoiceId: invoiceId,
    cAmountUsd: 0,
    cMethod: 'cash',
    cReference: null,
    cNotes: null,
  });

  const createPayment = useMutation({
    mutationFn: Invoices.createPayment,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['invoices'] });
      onClose();
    },
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    createPayment.mutate(formData);
  };

  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>Record Payment</DialogTitle>
      <form onSubmit={handleSubmit}>
        <DialogContent>
          <Grid container spacing={2}>
            <Grid item xs={12}>
              <TextField
                fullWidth
                type="number"
                label="Amount (USD)"
                value={formData.cAmountUsd}
                onChange={(e) => setFormData({ ...formData, cAmountUsd: Number(e.target.value) })}
                required
              />
            </Grid>
            <Grid item xs={12}>
              <FormControl fullWidth>
                <InputLabel>Payment Method</InputLabel>
                <Select
                  value={formData.cMethod}
                  onChange={(e) => setFormData({ ...formData, cMethod: e.target.value as any })}
                  label="Payment Method"
                >
                  <MenuItem value="cash">Cash</MenuItem>
                  <MenuItem value="bank_transfer">Bank Transfer</MenuItem>
                  <MenuItem value="card_pos">Card/POS</MenuItem>
                </Select>
              </FormControl>
            </Grid>
            <Grid item xs={12}>
              <TextField
                fullWidth
                label="Reference/Transaction ID"
                value={formData.cReference || ''}
                onChange={(e) => setFormData({ ...formData, cReference: e.target.value || null })}
              />
            </Grid>
            <Grid item xs={12}>
              <TextField
                fullWidth
                multiline
                rows={2}
                label="Notes"
                value={formData.cNotes || ''}
                onChange={(e) => setFormData({ ...formData, cNotes: e.target.value || null })}
              />
            </Grid>
          </Grid>
        </DialogContent>
        <DialogActions>
          <Button onClick={onClose}>Cancel</Button>
          <Button type="submit" variant="contained" disabled={createPayment.isPending}>
            Record Payment
          </Button>
        </DialogActions>
      </form>
    </Dialog>
  );
}
