
import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Parties } from '../api/parties';
import type { PartyDTO, PartyCreate, PartyUpdate } from '../api/types';
import { Stack, Paper, TextField, Button, IconButton, Typography, Dialog, DialogTitle, DialogContent, DialogActions } from '@mui/material';
import EditIcon from '@mui/icons-material/Edit';

function CreateParty({ onCreated }: { onCreated: () => void }) {
  const qc = useQueryClient();
  const [name, setName] = useState('');
  const [isOrg, setIsOrg] = useState(false);
  const m = useMutation({
    mutationFn: (body: PartyCreate) => Parties.create(body),
    onSuccess: () => { qc.invalidateQueries({ queryKey: ['parties'] }); onCreated(); setName(''); }
  });
  return (
    <Stack direction="row" gap={2} sx={{ mb: 2 }}>
      <TextField label="Nombre/Display" size="small" value={name} onChange={e => setName(e.target.value)} />
      <Button variant="contained" onClick={() => m.mutate({ cDisplayName: name, cIsOrg: isOrg })} disabled={!name || m.isPending}>
        Crear
      </Button>
    </Stack>
  );
}

function EditPartyDialog({ party, open, onClose }: { party: PartyDTO | null, open: boolean, onClose: () => void }) {
  const qc = useQueryClient();
  const [instagram, setInstagram] = useState(party?.instagram ?? '');
  const m = useMutation({
    mutationFn: (body: PartyUpdate) => Parties.update(party!.partyId, body),
    onSuccess: () => { qc.invalidateQueries({ queryKey: ['parties'] }); onClose(); }
  });
  return (
    <Dialog open={open} onClose={onClose}>
      <DialogTitle>Editar {party?.displayName}</DialogTitle>
      <DialogContent>
        <TextField label="Instagram" value={instagram} fullWidth onChange={e => setInstagram(e.target.value)} />
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cancelar</Button>
        <Button variant="contained" onClick={() => m.mutate({ uInstagram: instagram })} disabled={m.isPending}>Guardar</Button>
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
      <CreateParty onCreated={() => {}} />
      {isLoading && <div>Cargando...</div>}
      {error && <div>Error: {(error as Error).message}</div>}
      <Stack gap={1}>
        {data?.map(p => (
          <Paper key={p.partyId} sx={{ p: 1.5, display: 'flex', alignItems: 'center', justifyContent: 'space-between' }}>
            <div>
              <strong>{p.displayName}</strong> {p.isOrg ? '(Org)' : ''}
              {p.instagram ? <span style={{ marginLeft: 8, color: '#666' }}>• {p.instagram}</span> : null}
            </div>
            <IconButton onClick={() => setEditing(p)}><EditIcon /></IconButton>
          </Paper>
        ))}
      </Stack>
      <EditPartyDialog party={editing} open={!!editing} onClose={() => setEditing(null)} />
    </>
  );
}
