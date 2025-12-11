import { useMemo, useState } from 'react';
import {
  Autocomplete,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Grid,
  Stack,
  TextField,
  Typography,
  Alert,
} from '@mui/material';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import InventoryIcon from '@mui/icons-material/Inventory';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import type { AssetDTO } from '../api/types';
import type { AssetCheckoutRequest } from '../api/inventory';
import { Inventory } from '../api/inventory';
import { Parties } from '../api/parties';

const statusChip = (status?: string | null) => {
  const lower = (status ?? '').toLowerCase();
  if (lower.includes('book')) return { color: 'warning' as const, icon: <EventAvailableIcon fontSize="small" /> };
  if (lower.includes('maintenance') || lower.includes('mantenimiento')) return { color: 'warning' as const, icon: <WarningAmberIcon fontSize="small" /> };
  if (lower.includes('retired')) return { color: 'default' as const };
  return { color: 'success' as const, icon: <InventoryIcon fontSize="small" /> };
};

export default function ReservasEquipoPage() {
  const qc = useQueryClient();
  const assetsQuery = useQuery({
    queryKey: ['assets-reservas'],
    queryFn: () => Inventory.list().then((payload) => (Array.isArray(payload) ? payload : payload.items ?? [])),
  });
  const partiesQuery = useQuery({
    queryKey: ['parties'],
    queryFn: () => Parties.list(),
  });
  const [selected, setSelected] = useState<AssetDTO | null>(null);
  const [feedback, setFeedback] = useState<string | null>(null);
  const [dialogOpen, setDialogOpen] = useState(false);
  const [form, setForm] = useState<AssetCheckoutRequest>({
    coTargetKind: 'party',
    coTargetParty: '',
    coDueAt: '',
    coNotes: '',
  });

  const reserveMutation = useMutation({
    mutationFn: ({ assetId, payload }: { assetId: string; payload: AssetCheckoutRequest }) =>
      Inventory.checkout(assetId, payload),
    onSuccess: () => {
      setFeedback('Reserva creada y equipo marcado como reservado.');
      setDialogOpen(false);
      void qc.invalidateQueries({ queryKey: ['assets-reservas'] });
    },
    onError: () => setFeedback('No se pudo crear la reserva (¿ya está reservado o prestado?).'),
  });

  const assets = useMemo(() => assetsQuery.data ?? [], [assetsQuery.data]);
  const partyOptions = useMemo(() => partiesQuery.data ?? [], [partiesQuery.data]);

  const openReserve = (asset: AssetDTO) => {
    setSelected(asset);
    setDialogOpen(true);
  };

  const submitReserve = () => {
    if (!selected) return;
    reserveMutation.mutate({ assetId: selected.assetId, payload: form });
  };

  return (
    <Box p={2}>
      <Stack direction="row" spacing={1} alignItems="center" mb={2}>
        <EventAvailableIcon color="primary" />
        <Typography variant="h4" fontWeight={800}>
          Reservas de equipo
        </Typography>
      </Stack>
      {feedback && (
        <Alert severity="info" onClose={() => setFeedback(null)} sx={{ mb: 2 }}>
          {feedback}
        </Alert>
      )}
      <Grid container spacing={2}>
        {assets.map((asset) => {
          const chip = statusChip(asset.status);
          const disabled = (asset.status ?? '').toLowerCase().includes('book') || (asset.status ?? '').toLowerCase().includes('maintenance');
          return (
            <Grid key={asset.assetId} item xs={12} md={6} lg={4}>
              <Card variant="outlined">
                <CardContent>
                  <Stack spacing={0.5}>
                    <Typography variant="subtitle1" fontWeight={700}>
                      {asset.name}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      {asset.category}
                    </Typography>
                    <Stack direction="row" spacing={1} alignItems="center">
                      <Chip size="small" label={asset.status || '—'} color={chip.color} icon={chip.icon} />
                      {asset.condition && (
                        <Chip size="small" label={asset.condition} variant="outlined" />
                      )}
                    </Stack>
                    <Button
                      variant="contained"
                      size="small"
                      sx={{ alignSelf: 'flex-start', mt: 1 }}
                      onClick={() => openReserve(asset)}
                      disabled={disabled}
                    >
                      {disabled ? 'No disponible' : 'Reservar'}
                    </Button>
                  </Stack>
                </CardContent>
              </Card>
            </Grid>
          );
        })}
      </Grid>

      <Dialog open={dialogOpen} onClose={() => setDialogOpen(false)} fullWidth maxWidth="sm">
        <DialogTitle>Reservar equipo</DialogTitle>
        <DialogContent>
          <Stack spacing={1.5} mt={1}>
            <Typography variant="body2" color="text.secondary">
              {selected?.name}
            </Typography>
            <Autocomplete
              freeSolo
              options={partyOptions}
              loading={partiesQuery.isLoading}
              value={
                partyOptions.find(
                  (p) => p.displayName === (form.coTargetParty ?? ''),
                ) ?? null
              }
              inputValue={form.coTargetParty ?? ''}
              onInputChange={(_, value) =>
                setForm((prev) => ({ ...prev, coTargetParty: value, coTargetKind: 'party' }))
              }
              onChange={(_, value) => {
                if (!value) {
                  setForm((prev) => ({ ...prev, coTargetParty: '', coTargetKind: 'party' }));
                } else if (typeof value === 'string') {
                  setForm((prev) => ({ ...prev, coTargetParty: value, coTargetKind: 'party' }));
                } else {
                  setForm((prev) => ({ ...prev, coTargetParty: value.displayName, coTargetKind: 'party' }));
                }
              }}
              getOptionLabel={(option) =>
                typeof option === 'string'
                  ? option
                  : `${option.displayName}${option.primaryEmail ? ` · ${option.primaryEmail}` : ''}`
              }
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Para quién / proyecto"
                  placeholder="Busca un contacto o escribe un nombre"
                  fullWidth
                />
              )}
              noOptionsText={partiesQuery.isLoading ? 'Cargando contactos…' : 'Sin contactos'}
            />
            <TextField
              label="Fecha/hora de devolución (opcional)"
              type="datetime-local"
              fullWidth
              value={form.coDueAt ?? ''}
              onChange={(e) => setForm((prev) => ({ ...prev, coDueAt: e.target.value }))}
              InputLabelProps={{ shrink: true }}
            />
            <TextField
              label="Notas"
              fullWidth
              multiline
              minRows={2}
              value={form.coNotes ?? ''}
              onChange={(e) => setForm((prev) => ({ ...prev, coNotes: e.target.value }))}
            />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setDialogOpen(false)}>Cancelar</Button>
          <Button variant="contained" onClick={submitReserve} disabled={reserveMutation.isPending}>
            Confirmar reserva
          </Button>
        </DialogActions>
      </Dialog>
    </Box>
  );
}
