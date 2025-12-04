import {
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  MenuItem,
  Stack,
  TextField,
} from '@mui/material';
import type { AssetDTO } from '../api/types';
import type { AssetCheckinRequest, AssetCheckoutRequest } from '../api/inventory';

export const CHECKOUT_TARGET_OPTIONS = [
  { value: 'party', label: 'Staff / externo' },
  { value: 'room', label: 'Sala / cuarto' },
  { value: 'session', label: 'Sesión' },
];

export function CheckoutDialog({
  open,
  onClose,
  asset,
  form,
  onFormChange,
  onSubmit,
  loading,
}: {
  open: boolean;
  onClose: () => void;
  asset: AssetDTO | null;
  form: AssetCheckoutRequest;
  onFormChange: (form: AssetCheckoutRequest) => void;
  onSubmit: () => void;
  loading: boolean;
}) {
  if (!asset) return null;
  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>Check-out · {asset.name}</DialogTitle>
      <DialogContent>
        <Stack spacing={2} sx={{ mt: 1 }}>
          <TextField
            select
            label="Destino"
            value={form.coTargetKind ?? 'party'}
            onChange={(e) => onFormChange({ ...form, coTargetKind: e.target.value })}
          >
            {CHECKOUT_TARGET_OPTIONS.map((opt) => (
              <MenuItem key={opt.value} value={opt.value}>
                {opt.label}
              </MenuItem>
            ))}
          </TextField>
          <TextField
            label="Sala destino (room id)"
            value={form.coTargetRoom ?? ''}
            onChange={(e) => onFormChange({ ...form, coTargetRoom: e.target.value })}
            helperText="Solo si el destino es Sala"
          />
          <TextField
            label="Sesión destino (session id)"
            value={form.coTargetSession ?? ''}
            onChange={(e) => onFormChange({ ...form, coTargetSession: e.target.value })}
            helperText="Solo si el destino es Sesión"
          />
          <TextField
            label="Referencia (persona/empresa)"
            value={form.coTargetParty ?? ''}
            onChange={(e) => onFormChange({ ...form, coTargetParty: e.target.value })}
          />
          <TextField
            label="Fecha estimada de retorno"
            type="datetime-local"
            value={form.coDueAt ?? ''}
            onChange={(e) => onFormChange({ ...form, coDueAt: e.target.value })}
            InputLabelProps={{ shrink: true }}
          />
          <TextField
            label="Condición al salir"
            value={form.coConditionOut ?? ''}
            onChange={(e) => onFormChange({ ...form, coConditionOut: e.target.value })}
            multiline
          />
          <TextField
            label="Notas"
            value={form.coNotes ?? ''}
            onChange={(e) => onFormChange({ ...form, coNotes: e.target.value })}
            multiline
          />
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cancelar</Button>
        <Button variant="contained" onClick={onSubmit} disabled={loading}>
          {loading ? 'Guardando…' : 'Confirmar'}
        </Button>
      </DialogActions>
    </Dialog>
  );
}

export function CheckinDialog({
  open,
  onClose,
  asset,
  form,
  onFormChange,
  onSubmit,
  loading,
}: {
  open: boolean;
  onClose: () => void;
  asset: AssetDTO | null;
  form: AssetCheckinRequest;
  onFormChange: (form: AssetCheckinRequest) => void;
  onSubmit: () => void;
  loading: boolean;
}) {
  if (!asset) return null;
  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>Check-in · {asset.name}</DialogTitle>
      <DialogContent>
        <Stack spacing={2} sx={{ mt: 1 }}>
          <TextField
            label="Condición al entrar"
            value={form.ciConditionIn ?? ''}
            onChange={(e) => onFormChange({ ...form, ciConditionIn: e.target.value })}
            multiline
          />
          <TextField
            label="Notas"
            value={form.ciNotes ?? ''}
            onChange={(e) => onFormChange({ ...form, ciNotes: e.target.value })}
            multiline
          />
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cancelar</Button>
        <Button variant="contained" onClick={onSubmit} disabled={loading}>
          {loading ? 'Guardando…' : 'Confirmar'}
        </Button>
      </DialogActions>
    </Dialog>
  );
}
