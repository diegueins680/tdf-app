import { useEffect, useRef, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Alert, Autocomplete, Button, Chip, Stack, TextField } from '@mui/material';

import { SocialEventsAPI } from '../../api/socialEvents';
import {
  MAX_ARTIST_NAME_LENGTH,
  MAX_EVENT_ARTISTS,
  normalizeLineup,
  type EventLineupArtistDraft,
} from '../../utils/eventLineup';
import { useSession } from '../../session/SessionContext';

interface Props {
  value: readonly EventLineupArtistDraft[];
  onChange: (artists: EventLineupArtistDraft[]) => void;
  label?: string;
}

/** `create` marks the offer to add the typed name as an artist who is not on the platform yet. */
type LineupOption = EventLineupArtistDraft & { create?: boolean };

const nameKey = (name: string) => name.trim().replace(/\s+/g, ' ').toLocaleLowerCase();
// MUI also compares raw typed text (either side) when Enter adds a free-text entry.
const sameArtist = (a: EventLineupArtistDraft | string, b: EventLineupArtistDraft | string) => {
  if (typeof a === 'string' || typeof b === 'string') {
    return nameKey(typeof a === 'string' ? a : a.artistName) === nameKey(typeof b === 'string' ? b : b.artistName);
  }
  return a.artistId || b.artistId ? a.artistId === b.artistId : nameKey(a.artistName) === nameKey(b.artistName);
};

/**
 * Lineup field for a new event: pick artists already on the platform, or type
 * the name of one who is not and add it as a new artist.
 */
export default function EventLineupPicker({ value, onChange, label = 'Artistas que se presentan' }: Props) {
  const { session } = useSession();
  const [input, setInput] = useState('');
  const [search, setSearch] = useState('');
  // Typed name shared by several listed artists: the user must pick which one.
  const [ambiguous, setAmbiguous] = useState<string | null>(null);
  const [open, setOpen] = useState(false);
  // Set while an ambiguous Enter is handled, so the list stays open to choose from.
  const keepOpen = useRef(false);
  useEffect(() => {
    const timer = window.setTimeout(() => setSearch(input.trim()), 300);
    return () => window.clearTimeout(timer);
  }, [input]);
  const optionsQuery = useQuery({
    queryKey: ['event-artist-options', session?.partyId, search],
    queryFn: ({ signal }) => SocialEventsAPI.listArtists({ name: search || undefined, signal }),
    enabled: Boolean(session),
    retry: false,
  });
  const options: LineupOption[] = (optionsQuery.data ?? []).flatMap((artist) =>
    (artist.artistId ? [{ artistId: artist.artistId, artistName: artist.artistName }] : []));
  const full = value.length >= MAX_EVENT_ARTISTS;

  return (
    <Stack spacing={0.5} sx={{ minWidth: 0, width: '100%' }}>
      <Autocomplete<LineupOption, true, false, true>
        multiple
        freeSolo
        filterSelectedOptions
        options={options}
        value={[...value]}
        inputValue={input}
        open={open}
        onOpen={() => setOpen(true)}
        onClose={() => {
          if (!keepOpen.current) setOpen(false);
          keepOpen.current = false;
        }}
        loading={optionsQuery.isFetching}
        disabled={!session}
        filterOptions={(found) => {
          const typed = input.trim().replace(/\s+/g, ' ').slice(0, MAX_ARTIST_NAME_LENGTH);
          const known = (artist: EventLineupArtistDraft) => nameKey(artist.artistName) === nameKey(typed);
          return typed && !found.some(known) && !value.some(known)
            ? [...found, { artistId: null, artistName: typed, create: true }]
            : found;
        }}
        getOptionLabel={(option) => (typeof option === 'string' ? option : option.artistName)}
        getOptionKey={(option) => (typeof option === 'string' ? option : option.artistId ?? `nuevo:${option.artistName}`)}
        isOptionEqualToValue={sameArtist}
        getOptionDisabled={() => full}
        onInputChange={(_event, next, reason) => {
          // Selections clear the text in onChange, which may also keep it.
          if (reason !== 'input' && reason !== 'clear') return;
          setInput(next);
          setAmbiguous(null);
          keepOpen.current = false;
        }}
        onChange={(_event, selected) => {
          let homonym: string | null = null;
          onChange(normalizeLineup(selected.flatMap((entry): EventLineupArtistDraft[] => {
            if (typeof entry !== 'string') return [{ artistId: entry.artistId, artistName: entry.artistName }];
            // Enter on free text: link the listed artist with that exact name,
            // but never guess between several artists who share it.
            const matches = options.filter((option) => nameKey(option.artistName) === nameKey(entry));
            if (matches.length > 1) {
              homonym = entry.trim();
              return [];
            }
            return [matches[0] ?? { artistId: null, artistName: entry }];
          })));
          setAmbiguous(homonym);
          keepOpen.current = homonym !== null;
          if (homonym === null) setInput('');
        }}
        renderOption={(props, option) => {
          const { key, ...rest } = props;
          return (
            <li key={key} {...rest}>
              {option.create ? `Agregar «${option.artistName}» como artista nuevo` : option.artistName}
            </li>
          );
        }}
        renderTags={(selected, getTagProps) => selected.map((artist, index) => {
          const { key, ...tagProps } = getTagProps({ index });
          return (
            <Chip
              key={key}
              {...tagProps}
              size="small"
              color={artist.artistId ? 'default' : 'secondary'}
              variant={artist.artistId ? 'filled' : 'outlined'}
              label={artist.artistId ? artist.artistName : `${artist.artistName} · nuevo`}
            />
          );
        })}
        noOptionsText={optionsQuery.isError ? 'No se pudieron cargar los artistas' : 'Escribe el nombre del artista'}
        renderInput={(params) => (
          <TextField
            {...params}
            label={label}
            placeholder="Busca o escribe un nombre"
            error={ambiguous !== null}
            helperText={!session
              ? 'Inicia sesión para buscar artistas'
              : ambiguous !== null
                ? `Hay más de un artista llamado «${ambiguous}». Elige el correcto en la lista.`
                : full
                  ? `Máximo ${MAX_EVENT_ARTISTS} artistas por evento.`
                  : '¿No está en la plataforma? Escribe su nombre y presiona Enter: lo creamos junto con el evento.'}
          />
        )}
        sx={{ '& .MuiChip-root': { maxWidth: '100%' } }}
      />
      {optionsQuery.isError && (
        <Alert
          severity="warning"
          action={<Button color="inherit" size="small" onClick={() => void optionsQuery.refetch()}>Reintentar</Button>}
        >
          No pudimos buscar artistas. Aún puedes escribir un nombre y agregarlo.
        </Alert>
      )}
    </Stack>
  );
}
