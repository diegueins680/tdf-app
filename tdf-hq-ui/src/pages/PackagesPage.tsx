import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Box,
  Button,
  Card,
  CardContent,
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
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  Alert,
} from '@mui/material';
import { Add as AddIcon } from '@mui/icons-material';
import { Packages } from '../api/packages';
import type { PackageCreate } from '../api/types';

export default function PackagesPage() {
  const [createOpen, setCreateOpen] = useState(false);
  const queryClient = useQueryClient();

  const { data: packages = [], isLoading, error } = useQuery({
    queryKey: ['packages'],
    queryFn: Packages.list,
  });

  const createMutation = useMutation({
    mutationFn: Packages.create,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['packages'] });
      setCreateOpen(false);
    },
  });

  if (error) {
    return <Alert severity="error">Failed to load packages: {String(error)}</Alert>;
  }

  return (
    <Box>
      <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', mb: 3 }}>
        <Typography variant="h4">Package Catalog</Typography>
        <Button
          variant="contained"
          startIcon={<AddIcon />}
          onClick={() => setCreateOpen(true)}
        >
          Create Package
        </Button>
      </Box>

      <Grid container spacing={3}>
        {packages.map((pkg) => (
          <Grid item xs={12} md={6} lg={4} key={pkg.packageId}>
            <Card>
              <CardContent>
                <Typography variant="h6" gutterBottom>
                  {pkg.name}
                </Typography>
                <Chip
                  label={pkg.service}
                  color="primary"
                  size="small"
                  sx={{ mb: 2 }}
                />
                <Typography variant="h5" color="primary" gutterBottom>
                  ${pkg.priceUsd}
                </Typography>
                <Typography color="text.secondary" gutterBottom>
                  {pkg.unitsQty} {pkg.unitsKind}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Expires in {pkg.expiresDays} days
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Refund: {pkg.refundPolicy.replace('_', ' ')}
                </Typography>
                {pkg.description && (
                  <Typography variant="body2" sx={{ mt: 1 }}>
                    {pkg.description}
                  </Typography>
                )}
              </CardContent>
            </Card>
          </Grid>
        ))}
      </Grid>

      <CreatePackageDialog
        open={createOpen}
        onClose={() => setCreateOpen(false)}
        onSubmit={(data) => createMutation.mutate(data)}
        isSubmitting={createMutation.isPending}
      />
    </Box>
  );
}

function CreatePackageDialog({
  open,
  onClose,
  onSubmit,
  isSubmitting,
}: {
  open: boolean;
  onClose: () => void;
  onSubmit: (data: PackageCreate) => void;
  isSubmitting: boolean;
}) {
  const [formData, setFormData] = useState<PackageCreate>({
    cName: '',
    cService: 'Classes',
    cPriceUsd: 0,
    cUnitsKind: 'hours',
    cUnitsQty: 0,
    cExpiresDays: 120,
    cTransferable: false,
    cRefundPolicy: 'credit_only',
    cDescription: null,
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    onSubmit(formData);
  };

  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>Create New Package</DialogTitle>
      <form onSubmit={handleSubmit}>
        <DialogContent>
          <Grid container spacing={2}>
            <Grid item xs={12}>
              <TextField
                fullWidth
                label="Package Name"
                value={formData.cName}
                onChange={(e) => setFormData({ ...formData, cName: e.target.value })}
                required
              />
            </Grid>
            <Grid item xs={12}>
              <FormControl fullWidth>
                <InputLabel>Service</InputLabel>
                <Select
                  value={formData.cService}
                  onChange={(e) => setFormData({ ...formData, cService: e.target.value })}
                  label="Service"
                >
                  <MenuItem value="Classes">Classes</MenuItem>
                  <MenuItem value="Recording">Recording</MenuItem>
                  <MenuItem value="Mixing">Mixing</MenuItem>
                  <MenuItem value="Mastering">Mastering</MenuItem>
                  <MenuItem value="Rehearsal">Rehearsal</MenuItem>
                </Select>
              </FormControl>
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                type="number"
                label="Price (USD)"
                value={formData.cPriceUsd}
                onChange={(e) => setFormData({ ...formData, cPriceUsd: Number(e.target.value) })}
                required
              />
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                type="number"
                label="Expires (days)"
                value={formData.cExpiresDays}
                onChange={(e) => setFormData({ ...formData, cExpiresDays: Number(e.target.value) })}
                required
              />
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                type="number"
                label="Units Quantity"
                value={formData.cUnitsQty}
                onChange={(e) => setFormData({ ...formData, cUnitsQty: Number(e.target.value) })}
                required
              />
            </Grid>
            <Grid item xs={6}>
              <FormControl fullWidth>
                <InputLabel>Units Kind</InputLabel>
                <Select
                  value={formData.cUnitsKind}
                  onChange={(e) => setFormData({ ...formData, cUnitsKind: e.target.value as any })}
                  label="Units Kind"
                >
                  <MenuItem value="hours">Hours</MenuItem>
                  <MenuItem value="sessions">Sessions</MenuItem>
                  <MenuItem value="songs">Songs</MenuItem>
                </Select>
              </FormControl>
            </Grid>
            <Grid item xs={12}>
              <FormControl fullWidth>
                <InputLabel>Refund Policy</InputLabel>
                <Select
                  value={formData.cRefundPolicy}
                  onChange={(e) => setFormData({ ...formData, cRefundPolicy: e.target.value as any })}
                  label="Refund Policy"
                >
                  <MenuItem value="credit_only">Credit Only</MenuItem>
                  <MenuItem value="full">Full Refund</MenuItem>
                  <MenuItem value="none">No Refund</MenuItem>
                </Select>
              </FormControl>
            </Grid>
            <Grid item xs={12}>
              <TextField
                fullWidth
                multiline
                rows={3}
                label="Description"
                value={formData.cDescription || ''}
                onChange={(e) => setFormData({ ...formData, cDescription: e.target.value || null })}
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
