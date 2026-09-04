import { useEffect, useId, useMemo, useState, type ComponentProps } from 'react';
import {
  Autocomplete,
  Avatar,
  Box,
  Button,
  Chip,
  CircularProgress,
  Paper,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useInfiniteQuery } from '@tanstack/react-query';
import { searchPartiesForSelector, type PartySelectorContext, type PartySelectorKind, type PartySelectorOption } from '../../api/partySelector';
import { getActiveSession } from '../../session/SessionContext';

export interface PartySelectorProps {
  value: PartySelectorOption | null;
  onChange: (value: PartySelectorOption | null) => void;
  field: {
    label: string;
    required?: boolean;
    disabled?: boolean;
    helperText?: string;
  };
  search?: {
    context?: PartySelectorContext;
    kind?: PartySelectorKind;
    accountOnly?: boolean;
    excludedPartyIds?: number[];
  };
}

export interface PartyMultiSelectorProps {
  value: PartySelectorOption[];
  onChange: (value: PartySelectorOption[]) => void;
  field: PartySelectorProps['field'];
  search?: PartySelectorProps['search'];
}

const initials = (name: string) => name.split(/\s+/).filter(Boolean).slice(0, 2).map((part) => part[0]).join('').toUpperCase() || '?';

const OptionIdentity = ({ option }: { option: PartySelectorOption }) => (
  <Stack direction="row" spacing={1.25} alignItems="center" sx={{ minWidth: 0 }}>
    <Avatar src={option.avatarUrl ?? undefined} alt="" imgProps={{ loading: 'lazy' }} sx={{ width: 32, height: 32 }}>
      {initials(option.displayName)}
    </Avatar>
    <Box sx={{ minWidth: 0 }}>
      <Typography variant="body2" noWrap>{option.displayName}</Typography>
      {(option.username ?? option.secondaryLabel) && (
        <Typography variant="caption" color="text.secondary" noWrap>
          {[option.username ? `@${option.username}` : null, option.secondaryLabel].filter(Boolean).join(' · ')}
        </Typography>
      )}
    </Box>
  </Stack>
);

const mergeSelectedOptions = (
  selected: PartySelectorOption[],
  results: PartySelectorOption[],
) => {
  const merged = new Map<number, PartySelectorOption>();
  selected.forEach((option) => merged.set(option.partyId, option));
  results.forEach((option) => merged.set(option.partyId, option));
  return Array.from(merged.values());
};

const visuallyHidden = {
  border: 0,
  clip: 'rect(0 0 0 0)',
  height: 1,
  margin: -1,
  overflow: 'hidden',
  padding: 0,
  position: 'absolute' as const,
  whiteSpace: 'nowrap',
  width: 1,
};

const usePartySelectorOptions = ({
  selected,
  inputValue,
  kind,
  context,
  accountOnly,
  excludedPartyIds,
}: {
  selected: PartySelectorOption[];
  inputValue: string;
  kind: PartySelectorKind;
  context: PartySelectorContext;
  accountOnly: boolean;
  excludedPartyIds: number[];
}) => {
  const [debouncedQuery, setDebouncedQuery] = useState('');
  const normalizedQuery = inputValue.trim();
  const excludedKey = useMemo(
    () => [...new Set(excludedPartyIds)].sort((a, b) => a - b).join(','),
    [excludedPartyIds],
  );

  useEffect(() => {
    setDebouncedQuery('');
    if (normalizedQuery.length < 2) return undefined;
    const timer = window.setTimeout(() => {
      setDebouncedQuery(normalizedQuery);
    }, 300);
    return () => window.clearTimeout(timer);
  }, [normalizedQuery]);

  // Do not display a cached result set for the previous text while the new
  // query is inside the debounce window.
  const activeQuery = debouncedQuery === normalizedQuery ? debouncedQuery : '';
  const session = getActiveSession();
  const sessionScope = [
    session?.partyId ?? session?.username ?? 'anonymous',
    ...(session?.roles ?? []),
    ...(session?.modules ?? []),
  ].join(':');
  const query = useInfiniteQuery({
    queryKey: ['party-selector', sessionScope, activeQuery, context, kind, accountOnly, excludedKey],
    queryFn: ({ pageParam, signal }) => searchPartiesForSelector({
      query: activeQuery,
      context,
      kind,
      accountOnly,
      excludedPartyIds,
      cursor: pageParam,
      signal,
    }),
    initialPageParam: undefined as number | undefined,
    getNextPageParam: (page) => page.nextCursor ?? undefined,
    enabled: activeQuery.length >= 2,
    staleTime: 30_000,
    gcTime: 5 * 60_000,
    retry: false,
  });
  const results = query.data?.pages.flatMap((page) => page.items) ?? [];
  const options = mergeSelectedOptions(selected, results);
  const waitingForDebounce = normalizedQuery.length >= 2 && activeQuery.length < 2;
  const error = query.error instanceof Error
    ? query.error.message
    : query.isError
      ? 'No se pudo buscar. Inténtalo otra vez.'
      : null;

  return {
    options,
    loading: waitingForDebounce || (query.isFetching && !query.isFetchingNextPage),
    loadingMore: query.isFetchingNextPage,
    error,
    hasMore: Boolean(query.hasNextPage),
    loadMore: () => { void query.fetchNextPage(); },
    retry: () => { void query.refetch(); },
    resultCount: results.length,
  };
};

type SelectorPaperProps = ComponentProps<typeof Paper> & {
  hasMore: boolean;
  loadingMore: boolean;
  onLoadMore: () => void;
};

const SelectorPaper = ({ hasMore, loadingMore, onLoadMore, children, ...paperProps }: SelectorPaperProps) => (
  <Paper {...paperProps}>
    {children}
    {hasMore && (
      <Box sx={{ borderTop: 1, borderColor: 'divider', p: 1, textAlign: 'center' }}>
        <Button
          size="small"
          onMouseDown={(event) => event.preventDefault()}
          onClick={onLoadMore}
          disabled={loadingMore}
        >
          {loadingMore ? 'Cargando…' : 'Ver más resultados'}
        </Button>
      </Box>
    )}
  </Paper>
);

/**
 * Accessible, server-backed selector. Typed text deliberately never updates
 * `value`; callers always persist the selected canonical Party ID.
 */
export function PartySelector(props: PartySelectorProps) {
  const { value, onChange, field, search = {} } = props;
  const { label, required = false, disabled = false, helperText } = field;
  const { context = 'crm_assignment', kind = 'person', accountOnly = false, excludedPartyIds = [] } = search;
  const inputId = useId();
  const [inputValue, setInputValue] = useState(value?.displayName ?? '');

  useEffect(() => {
    setInputValue(value?.displayName ?? '');
  }, [value?.displayName, value?.partyId]);

  const searchInput = value?.displayName === inputValue ? '' : inputValue;
  const { options, loading, loadingMore, error, hasMore, loadMore, retry, resultCount } = usePartySelectorOptions({
    selected: value ? [value] : [],
    inputValue: searchInput,
    context,
    kind,
    accountOnly,
    excludedPartyIds,
  });

  return (
    <Box>
    <Autocomplete
      id={inputId}
      value={value}
      options={options}
      loading={loading}
      disabled={disabled}
      filterOptions={(values) => values}
      getOptionLabel={(option) => option.displayName}
      isOptionEqualToValue={(option, selected) => option.partyId === selected.partyId}
      onChange={(_, next) => {
        onChange(next);
        setInputValue(next?.displayName ?? '');
      }}
      inputValue={inputValue}
      onInputChange={(_, next, reason) => {
        if (reason === 'input' || reason === 'clear') setInputValue(next);
      }}
      noOptionsText={inputValue.trim().length < 2 ? 'Escribe al menos dos caracteres.' : error ? 'Error al buscar.' : 'No encontramos coincidencias.'}
      renderOption={(props, option) => <li {...props} key={option.partyId}><OptionIdentity option={option} /></li>}
      PaperComponent={(paperProps) => (
        <SelectorPaper {...paperProps} hasMore={hasMore} loadingMore={loadingMore} onLoadMore={loadMore} />
      )}
      renderInput={(params) => (
        <TextField
          {...params}
          required={required}
          label={label}
          error={Boolean(error)}
          helperText={error ? 'No se modificó la selección. Corrige la conexión y vuelve a intentar.' : helperText}
          inputProps={{ ...params.inputProps, 'aria-describedby': helperText ? `${inputId}-helper-text` : undefined }}
          InputProps={{ ...params.InputProps, endAdornment: <>{loading ? <CircularProgress color="inherit" size={18} /> : null}{params.InputProps.endAdornment}</> }}
        />
      )}
      renderTags={() => null}
    />
    <Typography component="span" aria-live="polite" sx={visuallyHidden}>
      {loading ? 'Buscando entidades.' : `${resultCount} resultados disponibles.`}
    </Typography>
    {error && (
      <Stack direction="row" spacing={1} alignItems="center" role="alert" sx={{ mt: 0.5 }}>
        <Typography variant="caption" color="error">No se modificó la selección. La búsqueda falló.</Typography>
        <Button size="small" color="error" onClick={retry}>Reintentar</Button>
      </Stack>
    )}
    </Box>
  );
}

/**
 * Multiple relationship selector. Selections remain available while searching,
 * duplicate Party IDs are impossible, and MUI provides keyboard chip removal.
 */
export function PartyMultiSelector(props: PartyMultiSelectorProps) {
  const { value, onChange, field, search = {} } = props;
  const { label, required = false, disabled = false, helperText } = field;
  const { context = 'crm_assignment', kind = 'person', accountOnly = false, excludedPartyIds = [] } = search;
  const inputId = useId();
  const [inputValue, setInputValue] = useState('');
  const { options, loading, loadingMore, error, hasMore, loadMore, retry, resultCount } = usePartySelectorOptions({
    selected: value,
    inputValue,
    context,
    kind,
    accountOnly,
    excludedPartyIds,
  });

  return (
    <Box>
    <Autocomplete
      multiple
      filterSelectedOptions
      id={inputId}
      value={value}
      options={options}
      loading={loading}
      disabled={disabled}
      filterOptions={(values) => values}
      getOptionLabel={(option) => option.displayName}
      isOptionEqualToValue={(option, selected) => option.partyId === selected.partyId}
      onChange={(_, next) => {
        onChange(mergeSelectedOptions([], next));
        setInputValue('');
      }}
      inputValue={inputValue}
      onInputChange={(_, next, reason) => {
        if (reason === 'input' || reason === 'clear') setInputValue(next);
      }}
      noOptionsText={inputValue.trim().length < 2 ? 'Escribe al menos dos caracteres.' : error ? 'Error al buscar.' : 'No encontramos coincidencias.'}
      renderOption={(optionProps, option) => <li {...optionProps} key={option.partyId}><OptionIdentity option={option} /></li>}
      PaperComponent={(paperProps) => (
        <SelectorPaper {...paperProps} hasMore={hasMore} loadingMore={loadingMore} onLoadMore={loadMore} />
      )}
      renderTags={(selected, getTagProps) => selected.map((option, index) => {
        const tagProps = getTagProps({ index });
        return (
          <Chip
            {...tagProps}
            key={option.partyId}
            avatar={<Avatar src={option.avatarUrl ?? undefined} alt="">{initials(option.displayName)}</Avatar>}
            label={option.username ? `${option.displayName} · @${option.username}` : option.displayName}
          />
        );
      })}
      renderInput={(params) => (
        <TextField
          {...params}
          required={required && value.length === 0}
          label={label}
          error={Boolean(error)}
          helperText={error ? 'No se modificaron las selecciones. Corrige la conexión y vuelve a intentar.' : helperText}
          inputProps={{ ...params.inputProps, 'aria-describedby': helperText ? `${inputId}-helper-text` : undefined }}
          InputProps={{ ...params.InputProps, endAdornment: <>{loading ? <CircularProgress color="inherit" size={18} /> : null}{params.InputProps.endAdornment}</> }}
        />
      )}
    />
    <Typography component="span" aria-live="polite" sx={visuallyHidden}>
      {loading ? 'Buscando entidades.' : `${resultCount} resultados disponibles.`}
    </Typography>
    {error && (
      <Stack direction="row" spacing={1} alignItems="center" role="alert" sx={{ mt: 0.5 }}>
        <Typography variant="caption" color="error">No se modificaron las selecciones. La búsqueda falló.</Typography>
        <Button size="small" color="error" onClick={retry}>Reintentar</Button>
      </Stack>
    )}
    </Box>
  );
}

export const UserSelector = ({ search, ...props }: PartySelectorProps) => (
  <PartySelector {...props} search={{ ...search, kind: 'person', accountOnly: true }} />
);
