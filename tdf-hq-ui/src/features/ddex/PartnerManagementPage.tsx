import React, { useState } from 'react';
import {
  Box,
  Paper,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Typography,
  Button,
  Chip,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  TextField,
  Checkbox,
  FormControl,
  InputLabel,
  ListItemText,
  MenuItem,
  Select,
  Stack,
  Alert,
  CircularProgress,
} from '@mui/material';
import {
  Add as AddIcon,
} from '@mui/icons-material';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { DDEX } from '../../api/ddex';
import type { DdexPartnerCreateRequest } from '../../api/ddex';

const PartnerManagementPage: React.FC = () => {
  const queryClient = useQueryClient();
  const [openDialog, setOpenDialog] = useState(false);
  const [formData, setFormData] = useState<DdexPartnerCreateRequest>({
    partnerName: '',
    partnerDpid: null,
    partnerAllowedStandardVersionIds: [],
  });

  const { data: references, isLoading: referencesLoading, error: referencesError } = useQuery({
    queryKey: ['ddex-references', 'es'],
    queryFn: () => DDEX.getReferences('es'),
  });

  const { data: partners, isLoading, error } = useQuery({
    queryKey: ['ddex-partners'],
    queryFn: () => DDEX.listPartners(),
  });

  const createMutation = useMutation({
    mutationFn: (data: DdexPartnerCreateRequest) => DDEX.createPartner(data),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ['ddex-partners'] });
      handleCloseDialog();
    },
  });

  const handleOpenDialog = () => {
    setFormData({
      partnerName: '',
      partnerDpid: null,
      partnerAllowedStandardVersionIds: [],
    });
    setOpenDialog(true);
  };

  const handleCloseDialog = () => {
    setOpenDialog(false);
    setFormData({
      partnerName: '',
      partnerDpid: null,
      partnerAllowedStandardVersionIds: [],
    });
  };

  const handleSubmit = () => {
    createMutation.mutate(formData);
  };

  if (isLoading || referencesLoading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  if (error || referencesError) {
    return (
      <Box p={3}>
        <Alert severity="error">
          Error loading DDEX partner configuration: {(error || referencesError)?.message}
        </Alert>
      </Box>
    );
  }

  return (
    <Box p={3}>
      <Stack direction="row" justifyContent="space-between" alignItems="center" mb={3}>
        <Typography variant="h4">Partner Management</Typography>
        <Button
          variant="contained"
          startIcon={<AddIcon />}
          onClick={handleOpenDialog}
        >
          Add Partner
        </Button>
      </Stack>

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>Name</TableCell>
              <TableCell>DPID</TableCell>
              <TableCell>Allowed Versions</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {partners?.map((partner) => (
              <TableRow key={partner.ddexPartnerId} hover>
                <TableCell>{partner.ddexPartnerName}</TableCell>
                <TableCell>
                  <Typography variant="body2" sx={{ fontFamily: 'monospace' }}>
                    {partner.ddexPartnerDpid || '-'}
                  </Typography>
                </TableCell>
                <TableCell>
                  <Stack direction="row" spacing={1}>
                    {partner.ddexPartnerAllowedStandardVersions.map((version) => (
                      <Chip
                        key={version.ddexStandardVersionId}
                        label={`${version.ddexStandardCode} ${version.ddexVersionCode}`}
                        size="small"
                      />
                    ))}
                  </Stack>
                </TableCell>
              </TableRow>
            ))}
            {(!partners || partners.length === 0) && (
              <TableRow>
                <TableCell colSpan={3} align="center">
                  <Typography color="text.secondary" py={4}>
                    No partners configured
                  </Typography>
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </TableContainer>

      {/* Creation only: published partner policy has no destructive or update endpoint yet. */}
      <Dialog open={openDialog} onClose={handleCloseDialog} maxWidth="sm" fullWidth>
        <DialogTitle>
          Add Partner
        </DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            <TextField
              label="Partner Name"
              value={formData.partnerName}
              onChange={(e) => setFormData({ ...formData, partnerName: e.target.value })}
              fullWidth
              required
              inputProps={{ maxLength: 160 }}
            />
            <TextField
              label="DPID"
              value={formData.partnerDpid || ''}
              onChange={(e) => setFormData({ ...formData, partnerDpid: e.target.value || null })}
              fullWidth
              placeholder="DPID:XXXXXXXX"
              helperText="DDEX Party Identifier (optional)"
              inputProps={{ maxLength: 200 }}
            />
            <FormControl fullWidth required>
              <InputLabel id="ddex-standard-version-label">Versiones DDEX permitidas</InputLabel>
              <Select
                labelId="ddex-standard-version-label"
                multiple
                label="Versiones DDEX permitidas"
                value={formData.partnerAllowedStandardVersionIds}
                onChange={(event) => setFormData({
                  ...formData,
                  partnerAllowedStandardVersionIds: typeof event.target.value === 'string'
                    ? [event.target.value]
                    : event.target.value,
                })}
                renderValue={(selected) => selected.map((selectedId) => {
                  const version = references?.ddexReferenceStandardVersions.find(
                    (item) => item.ddexStandardVersionId === selectedId,
                  );
                  return version ? `${version.ddexStandardCode} ${version.ddexVersionCode}` : selectedId;
                }).join(', ')}
              >
                {references?.ddexReferenceStandardVersions
                  .filter((version) => version.ddexStandardDetectionEnabled)
                  .map((version) => (
                    <MenuItem key={version.ddexStandardVersionId} value={version.ddexStandardVersionId}>
                      <Checkbox
                        checked={formData.partnerAllowedStandardVersionIds.includes(version.ddexStandardVersionId)}
                      />
                      <ListItemText
                        primary={`${version.ddexStandardCode} ${version.ddexVersionCode}`}
                        secondary={version.ddexStandardVersionName}
                      />
                    </MenuItem>
                  ))}
              </Select>
            </FormControl>
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={handleCloseDialog} sx={{ minHeight: 44 }}>Cancel</Button>
          <Button
            variant="contained"
            onClick={handleSubmit}
            disabled={
              !formData.partnerName
              || formData.partnerAllowedStandardVersionIds.length === 0
              || createMutation.isPending
            }
          >
            {createMutation.isPending ? 'Saving...' : 'Save'}
          </Button>
        </DialogActions>
      </Dialog>

      {createMutation.isError && (
        <Alert severity="error" sx={{ mt: 2 }}>
          Error: {createMutation.error.message}
        </Alert>
      )}
    </Box>
  );
};

export default PartnerManagementPage;
