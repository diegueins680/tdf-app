import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Parties } from '../api/parties';
import type { PartyDTO, PartyCreate, PartyUpdate } from '../api/types';
import {
  Typography, Stack, Paper, TextField, Button, Dialog, DialogTitle,
  DialogContent, DialogActions, IconButton, Chip, Box
} from '@mui/material';
import EditIcon from '@mui/icons-material/Edit';

function CreatePartyForm({ onCreated }: { onCreated: () => void }) {
  const qc = useQueryClient();
  const [name, setName] = useState('');
  const [isOrg, setIsOrg] = useState(false);

  const m = useMutation({
    mutationFn: (body: PartyCreate) => Parties.create(body),
    onSuccess: () => { qc.invalidateQueries({ queryKey: ['parties'] }); onCreated(); setName(''); }
  });

  return (
    <Stack direction={{ xs:'column', sm:'row' }} gap={2} sx={{ mb: 2 }}>
      <TextField label="Nombre / Display" size="small" value={name} onChange={e => setName(e.target.value)} />
      <Button
        variant="contained"
        onClick={() => m.mutate({ cDisplayName: name, cIsOrg: isOrg })}
        disabled={!name || m.isPending}
      >
        Crear
      </Button>
    </Stack>
  );
}

function EditPartyDialog({ party, open, onClose }: {
  party: PartyDTO | null; open: boolean; onClose: () => void;
}) {
  const qc = useQueryClient();
  const [instagram, setInstagram] = useState(party?.instagram ?? '');
  const [phone, setPhone] = useState(party?.primaryPhone ?? '');
  const m = useMutation({
    mutationFn: (body: PartyUpdate) => Parties.update(party!.partyId, body),
    onSuccess: () => { qc.invalidateQueries({ queryKey: ['parties'] }); onClose(); }
  });

  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="sm">
      <DialogTitle>Editar {party?.displayName}</DialogTitle>
      <DialogContent>
        <Stack gap={2} sx={{ mt: 1 }}>
          <TextField label="Instagram" fullWidth value={instagram} onChange={e => setInstagram(e.target.value)} />
          <TextField label="Teléfono" fullWidth value={phone} onChange={e => setPhone(e.target.value)} />
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cancelar</Button>
        <Button onClick={() => m.mutate({ uInstagram: instagram, uPrimaryPhone: phone })} variant="contained" disabled={m.isPending}>
          Guardar
        </Button>
      </DialogActions>
    </Dialog>
  );
}

export default function PartiesPage() {
  const { data, isLoading, error } = useQuery({ queryKey: ['parties'], queryFn: Parties.list });
  const [editing, setEditing] = useState<PartyDTO | null>(null);

  return (
    <>
      <Typography variant="h5" gutterBottom>Personas / CRM</Typography>
      <CreatePartyForm onCreated={() => {}} />
      {isLoading && <div>Cargando...</div>}
      {error && <div>Error: {(error as Error).message}</div>}
      <Stack gap={1}>
        {data?.map(p => (
          <Paper key={p.partyId} sx={{ p: 1.5, display: 'flex', alignItems: 'center', justifyContent: 'space-between' }}>
            <Box>
              <Typography variant="subtitle1" component="div"><strong>{p.displayName}</strong> {p.isOrg ? <Chip size="small" label="ORG" sx={{ ml: 1 }} /> : null}</Typography>
              <Typography variant="body2" color="text.secondary">
                {p.instagram ? `IG: ${p.instagram}` : ''} {p.primaryPhone ? ` · ${p.primaryPhone}` : ''}
              </Typography>
            </Box>
            <IconButton onClick={() => setEditing(p)}><EditIcon /></IconButton>
          </Paper>
        ))}
      </Stack>
      <EditPartyDialog party={editing} open={!!editing} onClose={() => setEditing(null)} />
    </>
  );
}
