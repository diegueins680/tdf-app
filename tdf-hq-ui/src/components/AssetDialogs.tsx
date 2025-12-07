import {
  Alert,
  Autocomplete,
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import type { AssetCheckoutDTO, AssetDTO, PartyDTO, RoomDTO } from '../api/types';
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
  roomOptions,
  partyOptions,
  loadingRooms,
  loadingParties,
  currentCheckout,
}: {
  open: boolean;
  onClose: () => void;
  asset: AssetDTO | null;
  form: AssetCheckoutRequest;
  onFormChange: (form: AssetCheckoutRequest) => void;
  onSubmit: () => void;
  loading: boolean;
  roomOptions?: RoomDTO[];
  partyOptions?: PartyDTO[];
  loadingRooms?: boolean;
  loadingParties?: boolean;
  currentCheckout?: AssetCheckoutDTO | null;
}) {
  if (!asset) return null;
  const targetKind = form.coTargetKind ?? 'party';
  const selectedRoom = roomOptions?.find((room) => room.roomId === form.coTargetRoom) ?? null;
  const activeCheckout = currentCheckout && !currentCheckout.returnedAt ? currentCheckout : null;
  const hasActive = Boolean(activeCheckout);
  const formatDue = (value?: string | null) => {
    if (!value) return null;
    const d = new Date(value);
    if (Number.isNaN(d.getTime())) return null;
    return d.toLocaleString();
  };
  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>Check-out · {asset.name}</DialogTitle>
      <DialogContent>
        <Typography variant="body2" color="text.secondary" sx={{ mb: 1 }}>
          Estado: {asset.status} · Ubicación: {asset.location ?? '—'}
        </Typography>
        {hasActive && (
          <Alert severity="warning" sx={{ mb: 1 }}>
            Actualmente en uso por {activeCheckout.targetPartyRef ?? activeCheckout.targetRoomId ?? activeCheckout.targetKind}.{' '}
            {activeCheckout.dueAt ? `Vence: ${formatDue(activeCheckout.dueAt)}` : 'Sin fecha de devolución.'}
            {' '}Registra el check-in antes de asignarlo de nuevo.
          </Alert>
        )}
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
          {targetKind === 'room' && (
            <Autocomplete<RoomDTO, false, false, false>
              options={roomOptions ?? []}
              getOptionLabel={(option) => option.rName}
              loading={loadingRooms}
              value={selectedRoom}
              onChange={(_, value) => onFormChange({ ...form, coTargetRoom: value?.roomId ?? '' })}
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Sala destino"
                  placeholder="Selecciona una sala"
                  helperText="Solo si el destino es Sala."
                />
              )}
              noOptionsText={loadingRooms ? 'Cargando salas…' : 'No hay salas registradas'}
            />
          )}
          {targetKind === 'session' && (
            <TextField
              label="Sesión destino (ID)"
              value={form.coTargetSession ?? ''}
              onChange={(e) => onFormChange({ ...form, coTargetSession: e.target.value })}
              helperText="Solo si el destino es Sesión."
            />
          )}
          {targetKind === 'party' && (
            <Autocomplete<PartyDTO, false, false, true>
              freeSolo
              options={partyOptions ?? []}
              getOptionLabel={(option) => (typeof option === 'string' ? option : option.displayName)}
              loading={loadingParties}
              value={partyOptions?.find((p) => p.displayName === form.coTargetParty) ?? null}
              inputValue={form.coTargetParty ?? ''}
              onInputChange={(_, value) => onFormChange({ ...form, coTargetParty: value })}
              onChange={(_, value) => {
                if (!value) {
                  onFormChange({ ...form, coTargetParty: '' });
                  return;
                }
                if (typeof value === 'string') {
                  onFormChange({ ...form, coTargetParty: value });
                } else {
                  onFormChange({ ...form, coTargetParty: value.displayName });
                }
              }}
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Referencia (persona/empresa)"
                  placeholder="Busca o escribe un nombre"
                  helperText="Guardaremos el nombre seleccionado o escrito."
                />
              )}
              noOptionsText={loadingParties ? 'Cargando contactos…' : 'Sin contactos disponibles'}
            />
          )}
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
        <Button variant="contained" onClick={onSubmit} disabled={loading || hasActive}>
          {hasActive ? 'Pendiente de check-in' : loading ? 'Guardando…' : 'Confirmar'}
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
