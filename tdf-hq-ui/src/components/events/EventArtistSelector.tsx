import { useEffect, useMemo, useState } from 'react';
import { useQueries, useQuery } from '@tanstack/react-query';
import { Alert, Autocomplete, Stack, TextField } from '@mui/material';
import { SocialEventsAPI, type SocialArtistDTO } from '../../api/socialEvents';
import { useSession } from '../../session/SessionContext';

interface Props {
  artistIds: readonly string[];
  onChange: (ids: string[]) => void;
  label?: string;
}

export default function EventArtistSelector({ artistIds, onChange, label = 'Artistas' }: Props) {
  const { session } = useSession();
  const [input, setInput] = useState('');
  const [search, setSearch] = useState('');
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
  const selectedQueries = useQueries({
    queries: artistIds.map(id => ({
      queryKey: ['event-artist-option', session?.partyId, id],
      queryFn: ({ signal }: { signal: AbortSignal }) => SocialEventsAPI.getArtist(id, signal),
      enabled: Boolean(session),
      staleTime: 300_000,
      retry: false,
    })),
  });
  const options = useMemo(() => (optionsQuery.data ?? []).filter(a => a.artistId), [optionsQuery.data]);
  const selected: SocialArtistDTO[] = artistIds.map((id, index) =>
    selectedQueries[index]?.data ?? options.find(a => a.artistId === id)
      ?? { artistId: id, artistName: `Artista #${id}` });
  return (
    <Stack spacing={0.5} sx={{ minWidth: 0, width: '100%' }}>
      <Autocomplete
        multiple
        filterSelectedOptions
        filterOptions={values => values}
        options={options}
        value={selected}
        inputValue={input}
        loading={optionsQuery.isFetching}
        disabled={!session}
        getOptionLabel={artist => artist.artistName}
        getOptionKey={artist => artist.artistId ?? artist.artistName}
        isOptionEqualToValue={(a, b) => a.artistId === b.artistId}
        onInputChange={(_event, value) => setInput(value)}
        onChange={(_event, artists) => onChange(artists.flatMap(a => a.artistId ? [a.artistId] : []))}
        getOptionDisabled={artist => artistIds.length >= 50 && !artistIds.includes(artist.artistId ?? '')}
        renderOption={(props, artist) => {
          const { key, ...rest } = props;
          return <li key={key} {...rest}>{artist.artistName} · #{artist.artistId}</li>;
        }}
        noOptionsText={optionsQuery.isError ? 'No se pudieron cargar los artistas' : 'Sin artistas coincidentes'}
        renderInput={params => <TextField {...params} label={label} size="small"
          helperText={!session ? 'Inicia sesión para buscar artistas' : 'Eventos de cualquiera de los artistas seleccionados'} />}
        sx={{ '& .MuiChip-root': { maxWidth: '100%' } }}
      />
      {optionsQuery.isError && <Alert severity="warning" action={<button onClick={() => void optionsQuery.refetch()}>Reintentar</button>}>
        No pudimos buscar artistas. Intenta de nuevo.
      </Alert>}
      {selectedQueries.some(query => query.isError) && <Alert severity="warning">
        No se pudo resolver algún artista seleccionado. Puedes quitarlo o intentar de nuevo.
      </Alert>}
    </Stack>
  );
}
