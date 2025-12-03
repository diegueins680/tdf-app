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
  Checkbox,
  FormControlLabel,
} from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import DeleteIcon from '@mui/icons-material/Delete';
import UploadFileIcon from '@mui/icons-material/UploadFile';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Parties } from '../api/parties';
import type { PartyDTO, PartyUpdate } from '../api/types';
import { Admin } from '../api/admin';
import type { Role } from '../api/generated/client';
import { submitLiveSessionIntake, listInputInventory, type InputInventoryItem } from '../api/liveSessions';
import { getStoredSessionToken } from '../session/SessionContext';

interface MusicianEntry {
  id: string;
  partyId?: number;
  name: string;
  email: string;
  phone: string;
  instrument: string;
  role: string;
  instagram: string;
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
  instagram: '',
  mode: 'new',
});

interface SongEntry {
  id: string;
  title: string;
  bpm: string;
  songKey: string;
  lyrics: string;
}

const emptySong = (): SongEntry => ({
  id: crypto.randomUUID(),
  title: '',
  bpm: '',
  songKey: '',
  lyrics: '',
});

type InventoryOption = {
  id: string;
  label: string;
  category: string;
  channelCount: number;
};

const GENRE_OPTIONS = [
  'Rock',
  'Pop',
  'Hip-Hop',
  'Rap',
  'Trap',
  'Reggaeton',
  'Indie',
  'Alternativo',
  'Metal',
  'Jazz',
  'Funk',
  'R&B',
  'Soul',
  'Electronica',
  'Folk',
  'Punk',
  'Regional',
  'Cumbia',
  'Ska',
];

const asNullableString = (value?: string | null): string | null => {
  if (value == null) return null;
  const trimmed = value.trim();
  return trimmed.length > 0 ? trimmed : null;
};

export interface LiveSessionIntakeFormProps {
  variant?: 'internal' | 'public';
  requireTerms?: boolean;
}

export function LiveSessionIntakeForm({ variant = 'internal', requireTerms }: LiveSessionIntakeFormProps) {
  const qc = useQueryClient();
  const hasToken = Boolean(getStoredSessionToken());
  const { data: parties = [], isLoading: partiesLoading } = useQuery({
    queryKey: ['parties'],
    queryFn: () => Parties.list(),
    enabled: hasToken,
  });
  const [bandName, setBandName] = useState('');
  const [bandDescription, setBandDescription] = useState('');
  const [primaryGenre, setPrimaryGenre] = useState('');
  const [contactEmail, setContactEmail] = useState('');
  const [contactPhone, setContactPhone] = useState('');
  const [sessionDate, setSessionDate] = useState(() => new Date().toISOString().slice(0, 10));
  const [availableDates, setAvailableDates] = useState('');
  const [sessionInputList, setSessionInputList] = useState('');
  const [inputChannels, setInputChannels] = useState<{
    id: string;
    channel: string;
    source: string;
    micId?: string | null;
    preampId?: string | null;
    interfaceId?: string | null;
    notes?: string;
  }[]>([{ id: crypto.randomUUID(), channel: 'Ch 1', source: '', micId: null, preampId: null, interfaceId: null, notes: '' }]);
  const [acceptedTerms, setAcceptedTerms] = useState(false);
  const [musicians, setMusicians] = useState<MusicianEntry[]>([emptyMusician()]);
  const [setlist, setSetlist] = useState<SongEntry[]>([emptySong()]);
  const [riderFile, setRiderFile] = useState<File | null>(null);
  const mustAcceptTerms = requireTerms ?? variant === 'public';
  const termsVersion = 'TDF Live Sessions v2';

  const inventoryQuery = useQuery({
    queryKey: ['input-inventory'],
    queryFn: () => listInputInventory(),
  });

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
      cInstagram: asNullableString(entry.instagram),
    });
    const updatePayload: PartyUpdate = {
      uPrimaryEmail: asNullableString(entry.email),
      uPrimaryPhone: asNullableString(entry.phone),
      uNotes: entry.instrument ? `Instrumento: ${entry.instrument}` : undefined,
      uInstagram: asNullableString(entry.instagram),
    };
    await Parties.update(created.partyId, updatePayload);
    if (entry.email) {
      const roles: Role[] = ['Artist'];
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
        } else if (entry.instagram.trim()) {
          await Parties.update(partyId, {
            uInstagram: asNullableString(entry.instagram),
          });
        }

        const instagramNote = asNullableString(entry.instagram)
          ? `Instagram: ${entry.instagram.trim().startsWith('@') ? entry.instagram.trim() : `@${entry.instagram.trim()}`}`
          : null;
        const mergedNotes = [asNullableString(entry.notes), instagramNote].filter(Boolean).join(' · ') || null;

        ensuredMusicians.push({
          partyId,
          name: entry.name,
          email: asNullableString(entry.email),
          instrument: asNullableString(entry.instrument),
          role: asNullableString(entry.role),
          notes: mergedNotes,
          isExisting: Boolean(entry.partyId),
        });
      }

      await submitLiveSessionIntake({
        bandName,
        bandDescription: asNullableString(bandDescription),
        primaryGenre: asNullableString(primaryGenre),
        inputList: asNullableString(
          JSON.stringify({
            channels: inputChannels.map((ch, idx) => ({
              channel: ch.channel || `Ch ${idx + 1}`,
              source: ch.source,
              micId: ch.micId ?? null,
              preampId: ch.preampId ?? null,
              interfaceId: ch.interfaceId ?? null,
              notes: ch.notes ?? '',
            })),
            notes: sessionInputList,
          }),
        ),
        contactEmail: asNullableString(contactEmail),
        contactPhone: asNullableString(contactPhone),
        sessionDate: asNullableString(sessionDate),
        availabilityNotes: asNullableString(availableDates),
        acceptedTerms: mustAcceptTerms ? acceptedTerms : undefined,
        termsVersion: mustAcceptTerms ? termsVersion : undefined,
        musicians: ensuredMusicians,
        setlist: setlist
          .map((song, idx) => {
            const bpmValue = Number.parseInt(song.bpm.trim(), 10);
            return {
              title: song.title.trim(),
              bpm: Number.isFinite(bpmValue) ? bpmValue : null,
              songKey: asNullableString(song.songKey),
              lyrics: asNullableString(song.lyrics),
              sortOrder: idx,
            };
          })
          .filter((song) => song.title.length > 0),
        riderFile,
      });
      setAcceptedTerms(false);
    },
  });

  const handleMusicianChange = (id: string, patch: Partial<MusicianEntry>) => {
    setMusicians((prev) => prev.map((m) => (m.id === id ? { ...m, ...patch } : m)));
  };

  const handleAddMusician = () => setMusicians((prev) => [...prev, emptyMusician()]);
  const handleRemoveMusician = (id: string) => setMusicians((prev) => prev.filter((m) => m.id !== id));

  const handleSelectParty = (id: string, party: PartyDTO | null) => {
    const current = musicians.find((m) => m.id === id);

    if (!party) {
      handleMusicianChange(id, { partyId: undefined, mode: 'new', name: '', email: '', phone: '', instagram: '', notes: '' });
      return;
    }

    handleMusicianChange(id, {
      partyId: party.partyId,
      mode: 'existing',
      name: party.displayName || current?.name || '',
      email: party.primaryEmail ?? current?.email ?? '',
      phone: party.primaryPhone ?? party.whatsapp ?? current?.phone ?? '',
      instagram: party.instagram ?? current?.instagram ?? '',
      notes: party.notes ?? current?.notes ?? '',
    });
  };

  const handleSongChange = (id: string, patch: Partial<SongEntry>) => {
    setSetlist((prev) => prev.map((song) => (song.id === id ? { ...song, ...patch } : song)));
  };

  const handleAddSong = () => setSetlist((prev) => [...prev, emptySong()]);
  const handleRemoveSong = (id: string) => setSetlist((prev) => prev.filter((song) => song.id !== id));

  const handleAddChannel = () => {
    const next = inputChannels.length + 1;
    setInputChannels((prev) => [...prev, { id: crypto.randomUUID(), channel: `Ch ${next}`, source: '', micId: null, preampId: null, interfaceId: null, notes: '' }]);
  };

  const parseChannelCount = (item: InputInventoryItem): number => {
    const haystack = `${item.name} ${item.model}`.toLowerCase();
    const match = haystack.match(/(\d+)\s*(ch|canal|ch\.)?/);
    if (match) {
      const parsed = Number.parseInt(match[1], 10);
      if (Number.isFinite(parsed) && parsed > 0) return parsed;
    }
    if (haystack.includes('8pre') || haystack.includes('mp8') || haystack.includes('bad8')) return 8;
    if (haystack.includes('b4')) return 4;
    if (haystack.includes('2-') || haystack.includes('dual') || haystack.includes('stereo')) return 2;
    return 1;
  };

  const handleRemoveChannel = (id: string) => {
    setInputChannels((prev) => (prev.length <= 1 ? prev : prev.filter((ch) => ch.id !== id)));
  };

  const handleChannelChange = (
    id: string,
    patch: Partial<{
      channel: string;
      source: string;
      micId?: string | null;
      preampId?: string | null;
      interfaceId?: string | null;
      notes?: string;
    }>,
  ) => {
    setInputChannels((prev) =>
      prev.map((ch) => {
        if (ch.id !== id) return ch;
        const next = { ...ch, ...patch };

        // Si el preamp elegido también es interfaz/conversor, autocompleta la interfaz (si está vacía).
        if (patch.preampId !== undefined) {
          const selectedPre = inventoryOptions.find((i) => i.id === patch.preampId);
          const isAlsoInterface =
            selectedPre && (selectedPre.category.includes('interface') || selectedPre.category.includes('conversor'));
          if (isAlsoInterface) {
            next.interfaceId = selectedPre.id;
          }
        }

        // Si la interfaz elegida también es preamp, autocompleta el preamp (si está vacío).
        if (patch.interfaceId !== undefined) {
          const selectedInt = inventoryOptions.find((i) => i.id === patch.interfaceId);
          const isAlsoPreamp = selectedInt && selectedInt.category.includes('pre');
          if (isAlsoPreamp) {
            next.preampId = selectedInt.id;
          }
        }

        return next;
      }),
    );
  };

  const usageCounts = useMemo(() => {
    const counts = {
      mic: new Map<string, number>(),
      pre: new Map<string, number>(),
      int: new Map<string, number>(),
    };
    inputChannels.forEach((ch) => {
      if (ch.micId) counts.mic.set(ch.micId, (counts.mic.get(ch.micId) ?? 0) + 1);
      if (ch.preampId) counts.pre.set(ch.preampId, (counts.pre.get(ch.preampId) ?? 0) + 1);
      if (ch.interfaceId) counts.int.set(ch.interfaceId, (counts.int.get(ch.interfaceId) ?? 0) + 1);
    });
    return counts;
  }, [inputChannels]);

  const inventoryOptions = useMemo<InventoryOption[]>(
    () =>
      (inventoryQuery.data ?? []).map((item: InputInventoryItem) => ({
        id: item.id,
        label: [item.name, item.brand, item.model].filter(Boolean).join(' · '),
        category: item.category?.toLowerCase() ?? '',
        channelCount: parseChannelCount(item),
      })),
    [inventoryQuery.data],
  );
  const selectedMicIds = useMemo(() => new Set(inputChannels.map((c) => c.micId).filter(Boolean)), [inputChannels]);
  const selectedPreampIds = useMemo(() => new Set(inputChannels.map((c) => c.preampId).filter(Boolean)), [inputChannels]);
  const selectedInterfaceIds = useMemo(
    () => new Set(inputChannels.map((c) => c.interfaceId).filter(Boolean)),
    [inputChannels],
  );

  const availableMics = (currentId?: string | null) =>
    inventoryOptions.filter((i) => {
      if (!i.category.includes('mic')) return false;
      const used = usageCounts.mic.get(i.id) ?? 0;
      const cap = i.channelCount;
      if (i.id === (currentId ?? '')) return true;
      return used < cap;
    });
  const availablePreamps = (currentId?: string | null) =>
    inventoryOptions.filter((i) => {
      if (!i.category.includes('pre')) return false;
      const used = usageCounts.pre.get(i.id) ?? 0;
      const cap = i.channelCount;
      if (i.id === (currentId ?? '')) return true;
      return used < cap;
    });
  const availableInterfaces = (currentId?: string | null) =>
    inventoryOptions.filter((i) => {
      if (!(i.category.includes('interface') || i.category.includes('conversor'))) return false;
      const used = usageCounts.int.get(i.id) ?? 0;
      const cap = i.channelCount;
      if (i.id === (currentId ?? '')) return true;
      return used < cap;
    });

  const riderLabel = riderFile ? `${riderFile.name} (${Math.round(riderFile.size / 1024)} KB)` : 'Subir rider técnico (PDF, DOCX)';

  return (
    <Stack spacing={3}>
      <Box>
        <Typography variant="h4" fontWeight={700} gutterBottom>
          {variant === 'public' ? 'Aplicar a TDF Live Sessions' : 'Live Session — Datos de banda'}
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
              <Autocomplete
                freeSolo
                options={GENRE_OPTIONS}
                value={primaryGenre}
                onChange={(_, value) => setPrimaryGenre(value ?? '')}
                onInputChange={(_, value) => setPrimaryGenre(value)}
                renderInput={(params) => (
                  <TextField
                    {...params}
                    label="Género principal"
                    placeholder="Rock, Rap, Pop..."
                  />
                )}
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
            <Grid item xs={12} md={9}>
              <TextField
                label="Fechas tentativas / disponibilidad"
                value={availableDates}
                onChange={(e) => setAvailableDates(e.target.value)}
                placeholder="Ej: 5-8 de mayo, 12 de mayo tarde, 18 de mayo noche"
                fullWidth
                multiline
                minRows={2}
              />
            </Grid>
            <Grid item xs={12}>
              <TextField
                label="Descripción / bio del proyecto"
                value={bandDescription}
                onChange={(e) => setBandDescription(e.target.value)}
                placeholder="Influencias, historia breve, concepto del show."
                fullWidth
                multiline
                minRows={3}
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
                  <Grid item xs={12} md={3}>
                    <TextField
                      label="Email"
                      type="email"
                      value={musician.email}
                      onChange={(e) => handleMusicianChange(musician.id, { email: e.target.value })}
                      fullWidth
                    />
                  </Grid>
                  <Grid item xs={12} md={3}>
                    <TextField
                      label="Teléfono"
                      value={musician.phone}
                      onChange={(e) => handleMusicianChange(musician.id, { phone: e.target.value })}
                      fullWidth
                    />
                  </Grid>
                  <Grid item xs={12} md={3}>
                    <TextField
                      label="Instagram"
                      value={musician.instagram}
                      onChange={(e) => handleMusicianChange(musician.id, { instagram: e.target.value })}
                      placeholder="@usuario"
                      fullWidth
                    />
                  </Grid>
                  <Grid item xs={12} md={3}>
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
        <Stack spacing={2}>
          <Stack direction="row" justifyContent="space-between" alignItems="center">
            <Typography variant="h6">Setlist y canciones</Typography>
            <Button startIcon={<AddIcon />} variant="outlined" onClick={handleAddSong}>
              Agregar canción
            </Button>
          </Stack>
          <Typography variant="body2" color="text.secondary">
            Define el setlist con BPM, tonalidad y letra. Usa el input list general para toda la sesión y así anticipar microfonía y ruteo.
          </Typography>
          <TextField
            label="Input list / microfonía de la sesión"
            value={sessionInputList}
            onChange={(e) => setSessionInputList(e.target.value)}
            placeholder="Kick IN - Beta91, Kick OUT - D112, Snare - SM57, Guitarra DI + SM57, Voz - SM7..."
            fullWidth
            multiline
            minRows={2}
          />
          <Stack spacing={1}>
            <Stack direction="row" justifyContent="space-between" alignItems="center">
              <Typography variant="subtitle1" fontWeight={700}>
                Canales (control de inventario)
              </Typography>
              <Button size="small" startIcon={<AddIcon />} onClick={handleAddChannel} variant="outlined">
                Agregar canal
              </Button>
            </Stack>
            {inventoryQuery.isError && <Alert severity="warning">No se pudo cargar inventario de microfonía/preamp.</Alert>}
            {inputChannels.map((ch, idx) => (
              <Paper key={ch.id} variant="outlined" sx={{ p: 1.5, borderRadius: 2 }}>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'flex-start', sm: 'center' }}>
                  <TextField
                    label="Canal"
                    value={ch.channel}
                    onChange={(e) => handleChannelChange(ch.id, { channel: e.target.value })}
                    sx={{ width: { xs: '100%', sm: 140 } }}
                  />
                  <TextField
                    label="Fuente (voz, gtr DI, kick out)"
                    value={ch.source}
                    onChange={(e) => handleChannelChange(ch.id, { source: e.target.value })}
                    fullWidth
                  />
                  {inputChannels.length > 1 && (
                    <IconButton onClick={() => handleRemoveChannel(ch.id)} size="small">
                      <DeleteIcon fontSize="small" />
                    </IconButton>
                  )}
                </Stack>
                <Grid container spacing={1} sx={{ mt: 1 }}>
                  <Grid item xs={12} md={4}>
                    <Autocomplete
                      options={availableMics(ch.micId)}
                      value={inventoryOptions.find((i) => i.id === ch.micId) ?? null}
                      onChange={(_, option) => handleChannelChange(ch.id, { micId: option?.id ?? null })}
                      renderInput={(params) => <TextField {...params} label="Micrófono" placeholder="SM7, Beta91, 57..." />}
                    />
                  </Grid>
                  <Grid item xs={12} md={4}>
                    <Autocomplete
                      options={availablePreamps(ch.preampId)}
                      value={inventoryOptions.find((i) => i.id === ch.preampId) ?? null}
                      onChange={(_, option) => handleChannelChange(ch.id, { preampId: option?.id ?? null })}
                      renderInput={(params) => <TextField {...params} label="Preamp" placeholder="Neve, API..." />}
                    />
                  </Grid>
                  <Grid item xs={12} md={4}>
                    <Autocomplete
                      options={availableInterfaces(ch.interfaceId)}
                      value={inventoryOptions.find((i) => i.id === ch.interfaceId) ?? null}
                      onChange={(_, option) => handleChannelChange(ch.id, { interfaceId: option?.id ?? null })}
                      renderInput={(params) => <TextField {...params} label="Interface / Conversor" placeholder="Apollo, Clarett..." />}
                    />
                  </Grid>
                  <Grid item xs={12}>
                    <TextField
                      label="Notas del canal (placement, comp, ruteo)"
                      value={ch.notes ?? ''}
                      onChange={(e) => handleChannelChange(ch.id, { notes: e.target.value })}
                      fullWidth
                      multiline
                      minRows={1}
                    />
                  </Grid>
                </Grid>
                <Typography variant="caption" color="text.secondary">
                  Canción #{idx + 1} usará este canal. Ajusta el nombre para mapear con el setlist.
                </Typography>
              </Paper>
            ))}
          </Stack>

          <Stack spacing={2}>
            {setlist.map((song, idx) => (
              <Paper
                key={song.id}
                variant="outlined"
                sx={{ p: 2, borderColor: 'divider', bgcolor: 'background.paper' }}
              >
                <Stack direction="row" justifyContent="space-between" alignItems="center" sx={{ mb: 1 }}>
                  <Typography variant="subtitle1" fontWeight={600}>
                    Canción #{idx + 1}
                  </Typography>
                  {setlist.length > 1 && (
                    <IconButton onClick={() => handleRemoveSong(song.id)} size="small">
                      <DeleteIcon />
                    </IconButton>
                  )}
                </Stack>

                <Grid container spacing={2}>
                  <Grid item xs={12} md={6}>
                    <TextField
                      label="Título"
                      value={song.title}
                      onChange={(e) => handleSongChange(song.id, { title: e.target.value })}
                      required
                      fullWidth
                    />
                  </Grid>
                  <Grid item xs={12} md={2}>
                    <TextField
                      label="BPM"
                      type="number"
                      value={song.bpm}
                      onChange={(e) => handleSongChange(song.id, { bpm: e.target.value })}
                      inputProps={{ min: 20, max: 260 }}
                      fullWidth
                    />
                  </Grid>
                  <Grid item xs={12} md={4}>
                    <TextField
                      label="Tonalidad"
                      value={song.songKey}
                      onChange={(e) => handleSongChange(song.id, { songKey: e.target.value })}
                      placeholder="Ej: C#m, Bb, F# menor"
                      fullWidth
                    />
                  </Grid>
                  <Grid item xs={12}>
                    <TextField
                      label="Letra"
                      value={song.lyrics}
                      onChange={(e) => handleSongChange(song.id, { lyrics: e.target.value })}
                      placeholder="Pega aquí la letra completa o notas clave."
                      fullWidth
                      multiline
                      minRows={2}
                    />
                  </Grid>
                  <Grid item xs={12}>
                  </Grid>
                </Grid>
              </Paper>
            ))}
          </Stack>
        </Stack>
      </Paper>

      <Paper sx={{ p: 3 }}>
        <Stack spacing={2}>
          <Typography variant="h6">Términos y logística</Typography>
          <Typography variant="body2" color="text.secondary">
            Participar en las TDF Live Sessions implica autorizar el uso de audio y video grabado durante la sesión para fines
            promocionales, respetar los horarios pactados y entregar información veraz del rider técnico y los músicos.
          </Typography>
          <Typography variant="body2" color="text.secondary">
            También confirmas que tienes los derechos para interpretar y grabar el material presentado y que aceptarás la agenda
            definitiva propuesta por TDF Records según disponibilidad de salas y staff.
          </Typography>
          {mustAcceptTerms && (
            <FormControlLabel
              control={
                <Checkbox
                  checked={acceptedTerms}
                  onChange={(e) => setAcceptedTerms(e.target.checked)}
                />
              }
              label={
                <Box>
                  <Typography variant="body2" fontWeight={600}>
                    Acepto los términos de participación ({termsVersion})
                  </Typography>
                  <Typography variant="caption" color="text.secondary">
                    Lee y acepta antes de enviar. Esto sirve como constancia de consentimiento.
                  </Typography>
                </Box>
              }
              sx={{ alignItems: 'flex-start' }}
            />
          )}
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
          disabled={mutation.isPending || (mustAcceptTerms && !acceptedTerms)}
        >
          {mutation.isPending ? 'Guardando…' : 'Enviar Live Session'}
        </Button>
      </Stack>
    </Stack>
  );
}

export default function LiveSessionIntakePage() {
  return <LiveSessionIntakeForm variant="internal" />;
}
