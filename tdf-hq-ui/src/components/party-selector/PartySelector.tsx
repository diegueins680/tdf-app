import { useEffect, useId, useMemo, useRef, useState } from 'react';
import {
  Alert,
  Autocomplete,
  Avatar,
  Box,
  CircularProgress,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { searchPartiesForSelector, type PartySelectorKind, type PartySelectorOption } from '../../api/partySelector';

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
    kind?: PartySelectorKind;
    accountOnly?: boolean;
    excludedPartyIds?: number[];
  };
}

const initials = (name: string) => name.split(/\s+/).filter(Boolean).slice(0, 2).map((part) => part[0]).join('').toUpperCase() || '?';

const OptionIdentity = ({ option }: { option: PartySelectorOption }) => (
  <Stack direction="row" spacing={1.25} alignItems="center" sx={{ minWidth: 0 }}>
    <Avatar src={option.avatarUrl ?? undefined} alt="" imgProps={{ loading: 'lazy' }} sx={{ width: 32, height: 32 }}>
      {initials(option.displayName)}
    </Avatar>
    <Box sx={{ minWidth: 0 }}>
      <Typography variant="body2" noWrap>{option.displayName}</Typography>
      {(option.username || option.secondaryLabel) && (
        <Typography variant="caption" color="text.secondary" noWrap>
          {[option.username ? `@${option.username}` : null, option.secondaryLabel].filter(Boolean).join(' · ')}
        </Typography>
      )}
    </Box>
  </Stack>
);

/**
 * Accessible, server-backed selector. Typed text deliberately never updates
 * `value`; callers always persist the selected canonical Party ID.
 */
export function PartySelector(props: PartySelectorProps) {
  const { value, onChange, field, search = {} } = props;
  const { label, required = false, disabled = false, helperText } = field;
  const { kind = 'person', accountOnly = false, excludedPartyIds = [] } = search;
  const inputId = useId();
  const [inputValue, setInputValue] = useState('');
  const [options, setOptions] = useState<PartySelectorOption[]>(value ? [value] : []);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const requestId = useRef(0);
  const excludedKey = useMemo(() => [...new Set(excludedPartyIds)].sort((a, b) => a - b).join(','), [excludedPartyIds]);

  useEffect(() => {
    if (value && !options.some((option) => option.partyId === value.partyId)) setOptions((current) => [value, ...current]);
  }, [options, value]);

  useEffect(() => {
    const query = inputValue.trim();
    if (query.length < 2) {
      setLoading(false);
      setError(null);
      setOptions(value ? [value] : []);
      return undefined;
    }
    const controller = new AbortController();
    const currentRequest = ++requestId.current;
    const timer = window.setTimeout(() => {
      setLoading(true);
      setError(null);
      void searchPartiesForSelector({
        query,
        kind,
        accountOnly,
        excludedPartyIds,
        signal: controller.signal,
      }).then((page) => {
        if (currentRequest !== requestId.current) return;
        setOptions(value && !page.items.some((option) => option.partyId === value.partyId) ? [value, ...page.items] : page.items);
      }).catch((reason: unknown) => {
        if (controller.signal.aborted || currentRequest !== requestId.current) return;
        setError(reason instanceof Error ? reason.message : 'No se pudo buscar. Inténtalo otra vez.');
      }).finally(() => {
        if (currentRequest === requestId.current) setLoading(false);
      });
    }, 300);
    return () => {
      window.clearTimeout(timer);
      controller.abort();
    };
  // `excludedKey` avoids a new request when parent passes an equivalent array.
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [inputValue, kind, accountOnly, excludedKey, value]);

  return (
    <Autocomplete
      id={inputId}
      value={value}
      options={options}
      loading={loading}
      disabled={disabled}
      filterOptions={(values) => values}
      getOptionLabel={(option) => option.displayName}
      isOptionEqualToValue={(option, selected) => option.partyId === selected.partyId}
      onChange={(_, next) => onChange(next)}
      inputValue={inputValue}
      onInputChange={(_, next, reason) => {
        if (reason === 'input' || reason === 'clear') setInputValue(next);
      }}
      noOptionsText={inputValue.trim().length < 2 ? 'Escribe al menos dos caracteres.' : error ? 'Error al buscar.' : 'No encontramos coincidencias.'}
      renderOption={(props, option) => <li {...props} key={option.partyId}><OptionIdentity option={option} /></li>}
      renderInput={(params) => (
        <TextField
          {...params}
          required={required}
          label={label}
          helperText={error ? <Alert severity="error" sx={{ py: 0 }}>No se modificó la selección. Corrige la conexión y vuelve a intentar.</Alert> : helperText}
          inputProps={{ ...params.inputProps, 'aria-describedby': helperText ? `${inputId}-helper-text` : undefined }}
          InputProps={{ ...params.InputProps, endAdornment: <>{loading ? <CircularProgress color="inherit" size={18} /> : null}{params.InputProps.endAdornment}</> }}
        />
      )}
      renderTags={() => null}
    />
  );
}

export const UserSelector = ({ search, ...props }: PartySelectorProps) => (
  <PartySelector {...props} search={{ ...search, kind: 'person', accountOnly: true }} />
);
