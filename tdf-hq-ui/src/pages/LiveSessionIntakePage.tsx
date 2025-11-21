import { useMemo, useState } from 'react';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Chip,
  Grid,
  IconButton,
  Paper,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import DeleteIcon from '@mui/icons-material/Delete';
import UploadFileIcon from '@mui/icons-material/UploadFile';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Parties } from '../api/parties';
import type { PartyDTO, PartyUpdate } from '../api/types';
import { Admin } from '../api/admin';
import type { PartyRole } from '../api/generated/client';
import { submitLiveSessionIntake } from '../api/liveSessions';

interface MusicianEntry {
  id: string;
  partyId?: number;
  name: string;
  email: string;
  phone: string;
  instrument: string;
  role: string;
  notes?: string;
  mode: 'existing' | 'new';
}

const emptyMusician = (): MusicianEntry => ({
  id: crypto.randomUUID(),
  name: '',
  email: '',
  phone: '',
  instrument: '',
  role: '',
  mode: 'new',
});

const asNullableString = (value?: string | null): string | null => {
  if (value == null) return null;
  const trimmed = value.trim();
  return trimmed.length > 0 ? trimmed : null;
};

export default function LiveSessionIntakePage() {
  const qc = useQueryClient();
  const { data: parties = [], isLoading: partiesLoading } = useQuery({
    queryKey: ['parties'],
    queryFn: () => Parties.list(),
  });

  const [bandName, setBandName] = useState('');
  const [contactEmail, setContactEmail] = useState('');
  const [contactPhone, setContactPhone] = useState('');
  const [sessionDate, setSessionDate] = useState(() => new Date().toISOString().slice(0, 10));
  const [musicians, setMusicians] = useState<MusicianEntry[]>([emptyMusician()]);
  const [riderFile, setRiderFile] = useState<File | null>(null);

  const partyOptions = useMemo(
    () =>
      parties.filter((p) => !p.isOrg).map((p) => ({
        label: `${p.displayName}${p.primaryEmail ? ` · ${p.primaryEmail}` : ''}`,
        party: p,
      })),
    [parties],
  );

  const createPartyAndUser = async (entry: MusicianEntry): Promise<PartyDTO> => {
    const created = await Parties.create({
      cDisplayName: entry.name,
      cIsOrg: false,
    });
    const updatePayload: PartyUpdate = {
      uPrimaryEmail: asNullableString(entry.email),
      uPrimaryPhone: asNullableString(entry.phone),
      uNotes: entry.instrument ? `Instrumento: ${entry.instrument}` : undefined,
    };
    await Parties.update(created.partyId, updatePayload);
    if (entry.email) {
      const roles: PartyRole[] = ['Artist'];
      await Admin.createUser({
        partyId: created.partyId,
        username: entry.email,
        roles,
      });
    }
    return created;
  };

  const mutation = useMutation({
    mutationFn: async () => {
      if (!bandName.trim()) {
        throw new Error('Ingresa el nombre de la banda.');
      }

      const ensuredMusicians = [];

      for (const entry of musicians) {
        if (!entry.name.trim() && !entry.partyId) continue;

        let partyId = entry.partyId;

        if (!partyId) {
          const created = await createPartyAndUser(entry);
          partyId = created.partyId;
          await qc.invalidateQueries({ queryKey: ['parties'] });
        }

        ensuredMusicians.push({
          partyId,
          name: entry.name,
          email: asNullableString(entry.email),
          instrument: asNullableString(entry.instrument),
          role: asNullableString(entry.role),
          notes: asNullableString(entry.notes),
          isExisting: Boolean(entry.partyId),
        });
      }

      await submitLiveSessionIntake({
        bandName,
        contactEmail: asNullableString(contactEmail),
        contactPhone: asNullableString(contactPhone),
        sessionDate: asNullableString(sessionDate),
        musicians: ensuredMusicians,
        riderFile,
      });
    },
  });

  const handleMusicianChange = (id: string, patch: Partial<MusicianEntry>) => {
    setMusicians((prev) => prev.map((m) => (m.id === id ? { ...m, ...patch } : m)));
  };

  const handleAddMusician = () => setMusicians((prev) => [...prev, emptyMusician()]);
  const handleRemoveMusician = (id: string) => setMusicians((prev) => prev.filter((m) => m.id !== id));

  const handleSelectParty = (id: string, party: PartyDTO | null) => {
    if (!party) {
      handleMusicianChange(id, { partyId: undefined, mode: 'new', name: '', email: '', phone: '' });
      return;
    }

    handleMusicianChange(id, {
      partyId: party.partyId,
      name: party.displayName,
      email: party.primaryEmail ?? '',
      phone: party.primaryPhone ?? '',
      mode: 'existing',
    });
  };

  const riderLabel = riderFile ? `${riderFile.name} (${Math.round(riderFile.size / 1024)} KB)` : 'Subir rider técnico (PDF, DOCX)';

  return (
    <Stack spacing={3}>
      <Box>
        <Typography variant="h4" fontWeight={700} gutterBottom>
          Live Session — Datos de banda
        </Typography>
        <Typography variant="body1" color="text.secondary">
          Ingresa la información del proyecto, los músicos y adjunta el rider técnico para generar el input list.
        </Typography>
      </Box>

      {mutation.isError && (
        <Alert severity="error">
          {mutation.error instanceof Error ? mutation.error.message : 'Ocurrió un error inesperado.'}
        </Alert>
      )}
      {mutation.isSuccess && <Alert severity="success">Información guardada. ¡Gracias!</Alert>}

      <Paper sx={{ p: 3 }}>
        <Stack spacing={2}>
          <Typography variant="h6">Proyecto</Typography>
          <Grid container spacing={2}>
            <Grid item xs={12} md={6}>
              <TextField
                label="Nombre de la banda / artista"
                value={bandName}
                onChange={(e) => setBandName(e.target.value)}
                required
                fullWidth
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                label="Email de contacto"
                type="email"
                value={contactEmail}
                onChange={(e) => setContactEmail(e.target.value)}
                fullWidth
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                label="Teléfono de contacto"
                value={contactPhone}
                onChange={(e) => setContactPhone(e.target.value)}
                fullWidth
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                label="Fecha tentativa"
                type="date"
                value={sessionDate}
                onChange={(e) => setSessionDate(e.target.value)}
                fullWidth
                InputLabelProps={{ shrink: true }}
              />
            </Grid>
          </Grid>
        </Stack>
      </Paper>

      <Paper sx={{ p: 3 }}>
        <Stack spacing={2}>
          <Stack direction="row" justifyContent="space-between" alignItems="center">
            <Typography variant="h6">Músicos</Typography>
            <Button startIcon={<AddIcon />} variant="outlined" onClick={handleAddMusician}>
              Agregar músico
            </Button>
          </Stack>

          <Stack spacing={2}>
            {musicians.map((musician, idx) => (
              <Paper
                key={musician.id}
                variant="outlined"
                sx={{ p: 2, borderColor: 'divider', bgcolor: 'background.paper' }}
              >
                <Stack direction="row" justifyContent="space-between" alignItems="center" sx={{ mb: 1 }}>
                  <Typography variant="subtitle1" fontWeight={600}>
                    Músico #{idx + 1}
                  </Typography>
                  {musicians.length > 1 && (
                    <IconButton onClick={() => handleRemoveMusician(musician.id)} size="small">
                      <DeleteIcon />
                    </IconButton>
                  )}
                </Stack>

                <Grid container spacing={2}>
                  <Grid item xs={12} md={6}>
                    <Autocomplete
                      options={partyOptions}
                      loading={partiesLoading}
                      onChange={(_, value) => handleSelectParty(musician.id, value?.party ?? null)}
                      renderInput={(params) => (
                        <TextField {...params} label="Seleccionar contacto existente" placeholder="Buscar por nombre o email" />
                      )}
                      value={
                        musician.partyId
                          ? partyOptions.find((opt) => opt.party.partyId === musician.partyId) ?? null
                          : null
                      }
                    />
                  </Grid>
                  <Grid item xs={12} md={6}>
                    <TextField
                      label="Nombre completo"
                      value={musician.name}
                      onChange={(e) => handleMusicianChange(musician.id, { name: e.target.value })}
                      required
                      fullWidth
                    />
                  </Grid>
                  <Grid item xs={12} md={4}>
                    <TextField
                      label="Email"
                      type="email"
                      value={musician.email}
                      onChange={(e) => handleMusicianChange(musician.id, { email: e.target.value })}
                      fullWidth
                    />
                  </Grid>
                  <Grid item xs={12} md={4}>
                    <TextField
                      label="Teléfono"
                      value={musician.phone}
                      onChange={(e) => handleMusicianChange(musician.id, { phone: e.target.value })}
                      fullWidth
                    />
                  </Grid>
                  <Grid item xs={12} md={4}>
                    <TextField
                      label="Rol en sesión (instrumento)"
                      value={musician.instrument}
                      onChange={(e) => handleMusicianChange(musician.id, { instrument: e.target.value, role: e.target.value })}
                      placeholder="Voz, guitarra, batería…"
                      fullWidth
                    />
                  </Grid>
                  <Grid item xs={12}>
                    <TextField
                      label="Notas"
                      value={musician.notes ?? ''}
                      onChange={(e) => handleMusicianChange(musician.id, { notes: e.target.value })}
                      fullWidth
                      multiline
                      minRows={2}
                    />
                  </Grid>
                  {musician.partyId ? (
                    <Grid item xs={12}>
                      <Chip label="Contacto existente" color="success" size="small" />
                    </Grid>
                  ) : (
                    <Grid item xs={12}>
                      <Chip label="Se creará usuario y contacto automáticamente" color="info" size="small" />
                    </Grid>
                  )}
                </Grid>
              </Paper>
            ))}
          </Stack>
        </Stack>
      </Paper>

      <Paper sx={{ p: 3 }}>
        <Stack spacing={1.5}>
          <Typography variant="h6">Rider técnico</Typography>
          <Button
            component="label"
            startIcon={<UploadFileIcon />}
            variant="outlined"
            sx={{ alignSelf: 'flex-start' }}
          >
            {riderLabel}
            <input
              type="file"
              accept=".pdf,.doc,.docx,.xlsx,.xls,.csv"
              hidden
              onChange={(e) => {
                const file = e.target.files?.[0];
                if (file) setRiderFile(file);
              }}
            />
          </Button>
          {riderFile && (
            <Typography variant="body2" color="text.secondary">
              Tamaño: {Math.round(riderFile.size / 1024)} KB
            </Typography>
          )}
        </Stack>
      </Paper>

      <Stack direction="row" spacing={2} justifyContent="flex-end">
        <Button variant="outlined" onClick={() => window.history.back()}>
          Cancelar
        </Button>
        <Button
          variant="contained"
          size="large"
          onClick={() => mutation.mutate()}
          disabled={mutation.isPending}
        >
          {mutation.isPending ? 'Guardando…' : 'Enviar Live Session'}
        </Button>
      </Stack>
    </Stack>
  );
}
