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
  Tabs,
  Tab,
} from '@mui/material';
import {
  Add as AddIcon,
  ExitToApp as CheckoutIcon,
  AssignmentReturn as CheckinIcon,
  Build as MaintenanceIcon,
} from '@mui/icons-material';
import { Inventory } from '../api/inventory';
import type { InventoryItemCreate, CheckoutCreate } from '../api/types';

export default function InventoryPage() {
  const [createOpen, setCreateOpen] = useState(false);
  const [checkoutOpen, setCheckoutOpen] = useState(false);
  const [selectedItemId, setSelectedItemId] = useState<number | null>(null);
  const [tabValue, setTabValue] = useState(0);
  const queryClient = useQueryClient();

  const { data: items = [], isLoading, error } = useQuery({
    queryKey: ['inventory'],
    queryFn: () => Inventory.list(),
  });

  const { data: checkouts = [] } = useQuery({
    queryKey: ['checkouts'],
    queryFn: Inventory.listCheckouts,
  });

  const { data: maintenanceDue = [] } = useQuery({
    queryKey: ['maintenance-due'],
    queryFn: Inventory.getMaintenanceDue,
  });

  const createMutation = useMutation({
    mutationFn: Inventory.create,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['inventory'] });
      setCreateOpen(false);
    },
  });

  const checkinMutation = useMutation({
    mutationFn: Inventory.checkin,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['checkouts'] });
      queryClient.invalidateQueries({ queryKey: ['inventory'] });
    },
  });

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'available':
        return 'success';
      case 'checked_out':
        return 'warning';
      case 'maintenance':
        return 'error';
      case 'retired':
        return 'default';
      default:
        return 'default';
    }
  };

  const getConditionColor = (condition: string) => {
    switch (condition) {
      case 'excellent':
        return 'success';
      case 'good':
        return 'info';
      case 'fair':
        return 'warning';
      case 'poor':
        return 'error';
      default:
        return 'default';
    }
  };

  if (error) {
    return <Alert severity="error">Failed to load inventory: {String(error)}</Alert>;
  }

  return (
    <Box>
      <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', mb: 3 }}>
        <Typography variant="h4">Inventory Management</Typography>
        <Button
          variant="contained"
          startIcon={<AddIcon />}
          onClick={() => setCreateOpen(true)}
        >
          Add Item
        </Button>
      </Box>

      <Box sx={{ borderBottom: 1, borderColor: 'divider', mb: 3 }}>
        <Tabs value={tabValue} onChange={(_, v) => setTabValue(v)}>
          <Tab label="All Items" />
          <Tab label="Checked Out" />
          <Tab label="Maintenance Due" />
        </Tabs>
      </Box>

      {tabValue === 0 && (
        <TableContainer component={Paper}>
          <Table>
            <TableHead>
              <TableRow>
                <TableCell>Name</TableCell>
                <TableCell>Category</TableCell>
                <TableCell>Serial Number</TableCell>
                <TableCell>Location</TableCell>
                <TableCell>Status</TableCell>
                <TableCell>Condition</TableCell>
                <TableCell align="center">Actions</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {items.map((item) => (
                <TableRow key={item.itemId}>
                  <TableCell>{item.name}</TableCell>
                  <TableCell>{item.category}</TableCell>
                  <TableCell>{item.serialNumber || '-'}</TableCell>
                  <TableCell>{item.location}</TableCell>
                  <TableCell>
                    <Chip
                      label={item.status}
                      color={getStatusColor(item.status)}
                      size="small"
                    />
                  </TableCell>
                  <TableCell>
                    <Chip
                      label={item.condition}
                      color={getConditionColor(item.condition)}
                      size="small"
                    />
                  </TableCell>
                  <TableCell align="center">
                    <IconButton
                      size="small"
                      onClick={() => {
                        setSelectedItemId(item.itemId);
                        setCheckoutOpen(true);
                      }}
                      disabled={item.status !== 'available'}
                    >
                      <CheckoutIcon />
                    </IconButton>
                    <IconButton size="small">
                      <MaintenanceIcon />
                    </IconButton>
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </TableContainer>
      )}

      {tabValue === 1 && (
        <TableContainer component={Paper}>
          <Table>
            <TableHead>
              <TableRow>
                <TableCell>Item</TableCell>
                <TableCell>Checked Out To</TableCell>
                <TableCell>Checked Out</TableCell>
                <TableCell>Expected Return</TableCell>
                <TableCell>Notes</TableCell>
                <TableCell align="center">Actions</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {checkouts
                .filter((c) => !c.returnedAt)
                .map((checkout) => (
                  <TableRow key={checkout.checkoutId}>
                    <TableCell>{checkout.itemName}</TableCell>
                    <TableCell>{checkout.partyName}</TableCell>
                    <TableCell>
                      {new Date(checkout.checkedOutAt).toLocaleDateString()}
                    </TableCell>
                    <TableCell>
                      {new Date(checkout.expectedReturnAt).toLocaleDateString()}
                    </TableCell>
                    <TableCell>{checkout.notes || '-'}</TableCell>
                    <TableCell align="center">
                      <IconButton
                        size="small"
                        color="primary"
                        onClick={() => checkinMutation.mutate(checkout.checkoutId)}
                      >
                        <CheckinIcon />
                      </IconButton>
                    </TableCell>
                  </TableRow>
                ))}
            </TableBody>
          </Table>
        </TableContainer>
      )}

      {tabValue === 2 && (
        <TableContainer component={Paper}>
          <Table>
            <TableHead>
              <TableRow>
                <TableCell>Name</TableCell>
                <TableCell>Category</TableCell>
                <TableCell>Location</TableCell>
                <TableCell>Last Maintenance</TableCell>
                <TableCell>Next Maintenance</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {maintenanceDue.map((item) => (
                <TableRow key={item.itemId}>
                  <TableCell>{item.name}</TableCell>
                  <TableCell>{item.category}</TableCell>
                  <TableCell>{item.location}</TableCell>
                  <TableCell>
                    {item.lastMaintenanceAt
                      ? new Date(item.lastMaintenanceAt).toLocaleDateString()
                      : 'Never'}
                  </TableCell>
                  <TableCell>
                    {item.nextMaintenanceAt
                      ? new Date(item.nextMaintenanceAt).toLocaleDateString()
                      : '-'}
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </TableContainer>
      )}

      <CreateItemDialog
        open={createOpen}
        onClose={() => setCreateOpen(false)}
        onSubmit={(data) => createMutation.mutate(data)}
        isSubmitting={createMutation.isPending}
      />

      {selectedItemId && (
        <CheckoutDialog
          open={checkoutOpen}
          onClose={() => {
            setCheckoutOpen(false);
            setSelectedItemId(null);
          }}
          itemId={selectedItemId}
        />
      )}
    </Box>
  );
}

function CreateItemDialog({
  open,
  onClose,
  onSubmit,
  isSubmitting,
}: {
  open: boolean;
  onClose: () => void;
  onSubmit: (data: InventoryItemCreate) => void;
  isSubmitting: boolean;
}) {
  const [formData, setFormData] = useState<InventoryItemCreate>({
    cName: '',
    cCategory: '',
    cLocation: '',
    cCondition: 'excellent',
    cSerialNumber: null,
    cPurchasedAt: null,
    cNotes: null,
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    onSubmit(formData);
  };

  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>Add Inventory Item</DialogTitle>
      <form onSubmit={handleSubmit}>
        <DialogContent>
          <Grid container spacing={2}>
            <Grid item xs={12}>
              <TextField
                fullWidth
                label="Item Name"
                value={formData.cName}
                onChange={(e) => setFormData({ ...formData, cName: e.target.value })}
                required
              />
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                label="Category"
                value={formData.cCategory}
                onChange={(e) => setFormData({ ...formData, cCategory: e.target.value })}
                required
              />
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                label="Serial Number"
                value={formData.cSerialNumber || ''}
                onChange={(e) => setFormData({ ...formData, cSerialNumber: e.target.value || null })}
              />
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                label="Location"
                value={formData.cLocation}
                onChange={(e) => setFormData({ ...formData, cLocation: e.target.value })}
                required
              />
            </Grid>
            <Grid item xs={6}>
              <FormControl fullWidth>
                <InputLabel>Condition</InputLabel>
                <Select
                  value={formData.cCondition}
                  onChange={(e) => setFormData({ ...formData, cCondition: e.target.value as any })}
                  label="Condition"
                >
                  <MenuItem value="excellent">Excellent</MenuItem>
                  <MenuItem value="good">Good</MenuItem>
                  <MenuItem value="fair">Fair</MenuItem>
                  <MenuItem value="poor">Poor</MenuItem>
                </Select>
              </FormControl>
            </Grid>
            <Grid item xs={12}>
              <TextField
                fullWidth
                type="date"
                label="Purchase Date"
                value={formData.cPurchasedAt || ''}
                onChange={(e) => setFormData({ ...formData, cPurchasedAt: e.target.value || null })}
                InputLabelProps={{ shrink: true }}
              />
            </Grid>
            <Grid item xs={12}>
              <TextField
                fullWidth
                multiline
                rows={3}
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
            Add Item
          </Button>
        </DialogActions>
      </form>
    </Dialog>
  );
}

function CheckoutDialog({
  open,
  onClose,
  itemId,
}: {
  open: boolean;
  onClose: () => void;
  itemId: number;
}) {
  const queryClient = useQueryClient();
  const [formData, setFormData] = useState<CheckoutCreate>({
    cItemId: itemId,
    cPartyId: 0,
    cExpectedReturnAt: '',
    cNotes: null,
  });

  const checkoutMutation = useMutation({
    mutationFn: Inventory.checkout,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['inventory'] });
      queryClient.invalidateQueries({ queryKey: ['checkouts'] });
      onClose();
    },
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    checkoutMutation.mutate(formData);
  };

  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>Check Out Item</DialogTitle>
      <form onSubmit={handleSubmit}>
        <DialogContent>
          <Grid container spacing={2}>
            <Grid item xs={12}>
              <TextField
                fullWidth
                type="number"
                label="Customer/Party ID"
                value={formData.cPartyId || ''}
                onChange={(e) => setFormData({ ...formData, cPartyId: Number(e.target.value) })}
                required
              />
            </Grid>
            <Grid item xs={12}>
              <TextField
                fullWidth
                type="date"
                label="Expected Return Date"
                value={formData.cExpectedReturnAt}
                onChange={(e) => setFormData({ ...formData, cExpectedReturnAt: e.target.value })}
                InputLabelProps={{ shrink: true }}
                required
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
          <Button type="submit" variant="contained" disabled={checkoutMutation.isPending}>
            Check Out
          </Button>
        </DialogActions>
      </form>
    </Dialog>
  );
}
