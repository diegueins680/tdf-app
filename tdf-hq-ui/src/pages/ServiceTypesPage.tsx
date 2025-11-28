import { useEffect, useMemo, useState } from 'react';
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
  Grid,
  IconButton,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  TextField,
  Typography,
} from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import EditIcon from '@mui/icons-material/Edit';
import DeleteIcon from '@mui/icons-material/Delete';
import { defaultServiceTypes, loadServiceTypes, saveServiceTypes, type ServiceType } from '../utils/serviceTypesStore';

interface FormState {
  id?: string;
  name: string;
  price: number | '';
  currency: string;
  billingUnit: string;
}

export default function ServiceTypesPage() {
  const [items, setItems] = useState<ServiceType[]>(defaultServiceTypes);
  const [dialogOpen, setDialogOpen] = useState(false);
  const [form, setForm] = useState<FormState>({ name: '', price: '', currency: 'USD', billingUnit: '' });
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    setItems(loadServiceTypes());
  }, []);

  const sortedItems = useMemo(
    () => [...items].sort((a, b) => a.name.localeCompare(b.name)),
    [items],
  );

  const handleOpenNew = () => {
    setForm({ name: '', price: '', currency: 'USD', billingUnit: '' });
    setError(null);
    setDialogOpen(true);
  };

  const handleEdit = (item: ServiceType) => {
    setForm({
      id: item.id,
      name: item.name,
      price: item.price,
      currency: item.currency,
      billingUnit: item.billingUnit ?? '',
    });
    setError(null);
    setDialogOpen(true);
  };

  const handleDelete = (id: string) => {
    const next = items.filter((it) => it.id !== id);
    setItems(next);
    saveServiceTypes(next);
  };

  const handleSubmit = (evt: React.FormEvent) => {
    evt.preventDefault();
    if (!form.name.trim()) {
      setError('Agrega un nombre.');
      return;
    }
    if (form.price === '' || Number.isNaN(Number(form.price))) {
      setError('Agrega un precio válido.');
      return;
    }
    const entity: ServiceType = {
      id: form.id ?? `svc-${Date.now()}`,
      name: form.name.trim(),
      price: Number(form.price),
      currency: form.currency.trim() || 'USD',
      billingUnit: form.billingUnit.trim() || undefined,
    };
    const exists = items.some((it) => it.id === entity.id);
    const next = exists ? items.map((it) => (it.id === entity.id ? entity : it)) : [...items, entity];
    setItems(next);
    saveServiceTypes(next);
    setDialogOpen(false);
  };

  return (
    <Box sx={{ color: '#e2e8f0' }}>
      <Stack direction="row" justifyContent="space-between" alignItems="center" mb={2}>
        <Box>
          <Typography variant="h5" fontWeight={800}>
            Tipos de servicio
          </Typography>
          <Typography variant="body2" color="rgba(226,232,240,0.75)">
            Mantén el catálogo de servicios del estudio con precios. Se usa al crear sesiones en el calendario.
          </Typography>
        </Box>
        <Button variant="contained" startIcon={<AddIcon />} onClick={handleOpenNew} sx={{ textTransform: 'none' }}>
          Nuevo servicio
        </Button>
      </Stack>

      <Card sx={{ bgcolor: 'rgba(255,255,255,0.02)', border: '1px solid rgba(255,255,255,0.08)' }}>
        <CardContent>
          {sortedItems.length === 0 ? (
            <Alert severity="info">Aún no tienes servicios. Crea el primero.</Alert>
          ) : (
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Servicio</TableCell>
                  <TableCell>Precio</TableCell>
                  <TableCell>Unidad</TableCell>
                  <TableCell align="right">Acciones</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {sortedItems.map((item) => (
                  <TableRow key={item.id} hover>
                    <TableCell>{item.name}</TableCell>
                    <TableCell>
                      {item.currency} {item.price.toLocaleString(undefined, { minimumFractionDigits: 0 })}
                    </TableCell>
                    <TableCell>{item.billingUnit ?? '—'}</TableCell>
                    <TableCell align="right">
                      <IconButton size="small" onClick={() => handleEdit(item)}>
                        <EditIcon fontSize="small" />
                      </IconButton>
                      <IconButton size="small" onClick={() => handleDelete(item.id)}>
                        <DeleteIcon fontSize="small" />
                      </IconButton>
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          )}
        </CardContent>
      </Card>

      <Dialog open={dialogOpen} onClose={() => setDialogOpen(false)} maxWidth="sm" fullWidth>
        <form onSubmit={handleSubmit}>
          <DialogTitle>{form.id ? 'Editar servicio' : 'Nuevo servicio'}</DialogTitle>
          <DialogContent dividers>
            <Grid container spacing={2}>
              <Grid item xs={12}>
                <TextField
                  label="Nombre"
                  value={form.name}
                  onChange={(e) => setForm((prev) => ({ ...prev, name: e.target.value }))}
                  fullWidth
                  required
                />
              </Grid>
              <Grid item xs={6}>
                <TextField
                  label="Precio"
                  type="number"
                  value={form.price}
                  onChange={(e) => setForm((prev) => ({ ...prev, price: Number(e.target.value) }))}
                  fullWidth
                  required
                />
              </Grid>
              <Grid item xs={6}>
                <TextField
                  label="Moneda"
                  value={form.currency}
                  onChange={(e) => setForm((prev) => ({ ...prev, currency: e.target.value }))}
                  fullWidth
                  required
                />
              </Grid>
              <Grid item xs={12}>
                <TextField
                  label="Unidad (hora, canción, episodio...)"
                  value={form.billingUnit}
                  onChange={(e) => setForm((prev) => ({ ...prev, billingUnit: e.target.value }))}
                  fullWidth
                />
              </Grid>
              {error && (
                <Grid item xs={12}>
                  <Alert severity="error">{error}</Alert>
                </Grid>
              )}
            </Grid>
          </DialogContent>
          <DialogActions>
            <Button onClick={() => setDialogOpen(false)}>Cancelar</Button>
            <Button type="submit" variant="contained">
              Guardar
            </Button>
          </DialogActions>
        </form>
      </Dialog>
    </Box>
  );
}
