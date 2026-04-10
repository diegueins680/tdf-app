import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Checkbox,
  Collapse,
  FormControlLabel,
  IconButton,
  InputAdornment,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  TextField,
  Typography,
} from '@mui/material';
import ClearIcon from '@mui/icons-material/Clear';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useSearchParams } from 'react-router-dom';
import { Admin } from '../api/admin';
import type { DropdownOptionDTO, DropdownOptionUpdate } from '../api/types';

const DEFAULT_CATEGORIES = ['asset-category', 'band-genre', 'band-role'];
const emptyNewOption = () => ({ value: '', label: '', sortOrder: '', active: true });

function OptionRow({
  option,
  onSave,
  saving,
}: {
  option: DropdownOptionDTO;
  onSave: (optionId: string, payload: DropdownOptionUpdate) => Promise<void>;
  saving: boolean;
}) {
  const [value, setValue] = useState(option.value);
  const [label, setLabel] = useState(option.label ?? '');
  const [sortOrder, setSortOrder] = useState(option.sortOrder?.toString() ?? '');
  const [active, setActive] = useState(option.active);

  useEffect(() => {
    setValue(option.value);
    setLabel(option.label ?? '');
    setSortOrder(option.sortOrder?.toString() ?? '');
    setActive(option.active);
  }, [option.optionId, option.value, option.label, option.sortOrder, option.active]);

  const trimmedValue = value.trim();
  const cleanLabel = label.trim();
  const sortNumber = sortOrder.trim() === '' ? null : Number(sortOrder.trim());
  const sortInvalid = sortOrder.trim() !== '' && !Number.isFinite(sortNumber);
  const payload: DropdownOptionUpdate = {};
  if (trimmedValue !== option.value) payload.douValue = trimmedValue;
  if (cleanLabel !== (option.label ?? '')) payload.douLabel = cleanLabel || null;
  if ((sortNumber ?? null) !== (option.sortOrder ?? null)) payload.douSortOrder = sortNumber;
  if (active !== option.active) payload.douActive = active;
  const dirty = Object.keys(payload).length > 0;

  return (
    <TableRow hover key={option.optionId}>
      <TableCell sx={{ width: { xs: 180, md: 240 } }}>
        <TextField
          label="Valor"
          value={value}
          size="small"
          onChange={(e) => setValue(e.target.value)}
          fullWidth
        />
      </TableCell>
      <TableCell sx={{ width: { xs: 200, md: 260 } }}>
        <TextField
          label="Etiqueta"
          value={label}
          size="small"
          onChange={(e) => setLabel(e.target.value)}
          placeholder="Visible para usuarios"
          fullWidth
        />
      </TableCell>
      <TableCell sx={{ width: 140 }}>
        <TextField
          label="Orden"
          value={sortOrder}
          size="small"
          onChange={(e) => setSortOrder(e.target.value)}
          error={sortInvalid}
          helperText={sortInvalid ? 'Usa un número' : 'Opcional'}
          fullWidth
        />
      </TableCell>
      <TableCell sx={{ width: 120 }}>
        <FormControlLabel
          control={<Checkbox checked={active} onChange={(e) => setActive(e.target.checked)} />}
          label="Activo"
        />
      </TableCell>
      <TableCell align="right" sx={{ whiteSpace: 'nowrap' }}>
        {dirty ? (
          <>
            <Button
              variant="outlined"
              size="small"
              disabled={sortInvalid || saving}
              onClick={() => {
                void onSave(option.optionId, payload);
              }}
              sx={{ mr: 1 }}
            >
              {saving ? 'Guardando…' : 'Guardar'}
            </Button>
            <Button
              variant="text"
              size="small"
              disabled={saving}
              onClick={() => {
                setValue(option.value);
                setLabel(option.label ?? '');
                setSortOrder(option.sortOrder?.toString() ?? '');
                setActive(option.active);
              }}
            >
              Revertir
            </Button>
          </>
        ) : null}
      </TableCell>
    </TableRow>
  );
}

export default function UxOptionsPage() {
  const qc = useQueryClient();
  const [searchParams] = useSearchParams();
  const requestedCategory = (searchParams.get('category') ?? '').trim();
  const initialCategory = requestedCategory !== '' ? requestedCategory : (DEFAULT_CATEGORIES[0] ?? '');
  const [category, setCategory] = useState<string>(initialCategory);
  const [includeInactive, setIncludeInactive] = useState(false);
  const [newOption, setNewOption] = useState(emptyNewOption);
  const [optionFilter, setOptionFilter] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [showCreateForm, setShowCreateForm] = useState(false);

  useEffect(() => {
    if (!requestedCategory) return;
    setCategory((prev) => (prev === requestedCategory ? prev : requestedCategory));
  }, [requestedCategory]);

  useEffect(() => {
    setOptionFilter('');
    setNewOption(emptyNewOption());
    setError(null);
    setShowCreateForm(false);
  }, [category]);

  const optionsQuery = useQuery({
    queryKey: ['dropdowns', category, includeInactive],
    queryFn: () => Admin.listDropdowns(category, includeInactive),
    enabled: Boolean(category),
    staleTime: 5 * 60 * 1000,
  });

  const createMutation = useMutation({
    mutationFn: () =>
      Admin.createDropdown(category, {
        docValue: newOption.value.trim(),
        docLabel: newOption.label.trim() || null,
        docSortOrder: newOption.sortOrder.trim() ? Number(newOption.sortOrder.trim()) : null,
        docActive: newOption.active,
      }),
    onSuccess: () => {
      setNewOption(emptyNewOption());
      setError(null);
      setShowCreateForm(false);
      void qc.invalidateQueries({ queryKey: ['dropdowns', category] });
    },
    onError: (err) => setError(err instanceof Error ? err.message : 'No se pudo crear la opción.'),
  });

  const updateMutation = useMutation({
    mutationFn: ({ optionId, payload }: { optionId: string; payload: DropdownOptionUpdate }) =>
      Admin.updateDropdown(category, optionId, payload),
    onSuccess: () => {
      setError(null);
      void qc.invalidateQueries({ queryKey: ['dropdowns', category] });
    },
    onError: (err) => setError(err instanceof Error ? err.message : 'No se pudo actualizar la opción.'),
  });

  const savingId = (updateMutation.variables as { optionId?: string } | undefined)?.optionId ?? null;
  const options = useMemo(() => optionsQuery.data ?? [], [optionsQuery.data]);
  const hasLoadedOptions = optionsQuery.isSuccess;
  const hasOptions = options.length > 0;
  const activeCount = useMemo(
    () => options.filter((option) => option.active).length,
    [options],
  );
  const normalizedFilter = optionFilter.trim().toLowerCase();
  const hasActiveFilter = normalizedFilter !== '';
  const filteredOptions = useMemo(() => {
    if (!normalizedFilter) return options;
    return options.filter((option) => {
      const value = option.value.toLowerCase();
      const label = (option.label ?? '').toLowerCase();
      return value.includes(normalizedFilter) || label.includes(normalizedFilter);
    });
  }, [normalizedFilter, options]);
  const filteredActiveCount = useMemo(
    () => filteredOptions.filter((option) => option.active).length,
    [filteredOptions],
  );
  const isCreateFormVisible = (hasLoadedOptions && !hasOptions) || showCreateForm;
  const showListChrome = hasOptions && (options.length > 1 || hasActiveFilter);
  const showClearFilterAction = showListChrome && hasActiveFilter;
  const showSingleOptionGuidance = options.length === 1 && !hasActiveFilter;

  useEffect(() => {
    if (!hasLoadedOptions || hasOptions) return;
    setShowCreateForm(true);
  }, [hasLoadedOptions, hasOptions]);

  const handleCreate = () => {
    const cleanValue = newOption.value.trim();
    if (!cleanValue) {
      setError('Agrega un valor para la opción.');
      return;
    }
    if (newOption.sortOrder.trim() && !Number.isFinite(Number(newOption.sortOrder.trim()))) {
      setError('El orden debe ser numérico.');
      return;
    }
    setError(null);
    createMutation.mutate();
  };

  return (
    <Stack spacing={2}>
      <Box>
        <Typography variant="h4" fontWeight={800}>
          Opciones de UX
        </Typography>
        <Typography variant="body1" color="text.secondary">
          Administra listas desplegables como géneros o roles sin tocar código. Las opciones activas se usan en formularios.
        </Typography>
      </Box>

      <Paper sx={{ p: 2.5 }}>
        <Stack spacing={2}>
          <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} alignItems={{ md: 'center' }}>
            <Autocomplete
              freeSolo
              options={DEFAULT_CATEGORIES}
              value={category}
              onChange={(_, value) => setCategory(value ?? '')}
              onInputChange={(_, value) => setCategory(value)}
              renderInput={(params) => <TextField {...params} label="Categoría" placeholder="band-genre, band-role, etc." />}
              sx={{ minWidth: { xs: '100%', md: 320 } }}
            />
            <FormControlLabel
              control={
                <Checkbox
                  checked={includeInactive}
                  onChange={(e) => setIncludeInactive(e.target.checked)}
                />
              }
              label="Incluir inactivas"
            />
            <Button
              variant="outlined"
              onClick={() => {
                void optionsQuery.refetch();
              }}
              disabled={optionsQuery.isFetching}
            >
              Recargar
            </Button>
          </Stack>
          {error && <Alert severity="error">{error}</Alert>}
          <Stack
            direction={{ xs: 'column', md: 'row' }}
            spacing={1.5}
            justifyContent="space-between"
            alignItems={{ md: 'center' }}
          >
            <Box>
              <Typography variant="subtitle1" fontWeight={700}>
                {hasLoadedOptions && !hasOptions ? 'Primera opción' : 'Nueva opción'}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                {hasLoadedOptions && !hasOptions
                  ? 'Esta categoría todavía no tiene opciones. Crea la primera para habilitarla en formularios.'
                  : 'Abre este formulario solo cuando necesites agregar un valor nuevo a esta categoría.'}
              </Typography>
            </Box>
            {hasLoadedOptions && hasOptions && (
              <Button
                variant={isCreateFormVisible ? 'text' : 'outlined'}
                onClick={() => setShowCreateForm((current) => !current)}
              >
                {isCreateFormVisible ? 'Ocultar formulario' : 'Agregar opción'}
              </Button>
            )}
          </Stack>
          <Collapse in={isCreateFormVisible} unmountOnExit>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} sx={{ pt: 0.5 }}>
              <TextField
                label="Valor"
                value={newOption.value}
                onChange={(e) => setNewOption((prev) => ({ ...prev, value: e.target.value }))}
                required
              />
              <TextField
                label="Etiqueta (opcional)"
                value={newOption.label}
                onChange={(e) => setNewOption((prev) => ({ ...prev, label: e.target.value }))}
              />
              <TextField
                label="Orden (opcional)"
                value={newOption.sortOrder}
                onChange={(e) => setNewOption((prev) => ({ ...prev, sortOrder: e.target.value }))}
              />
              <FormControlLabel
                control={
                  <Checkbox
                    checked={newOption.active}
                    onChange={(e) => setNewOption((prev) => ({ ...prev, active: e.target.checked }))}
                  />
                }
                label="Activo"
              />
              <Button
                variant="contained"
                onClick={handleCreate}
                disabled={createMutation.isPending}
                sx={{ alignSelf: 'center' }}
              >
                {createMutation.isPending ? 'Guardando…' : 'Agregar'}
              </Button>
            </Stack>
          </Collapse>
        </Stack>
      </Paper>

      <Paper sx={{ p: 2.5 }}>
        <Stack
          direction={{ xs: 'column', sm: 'row' }}
          spacing={1}
          justifyContent="space-between"
          alignItems={{ xs: 'flex-start', sm: 'center' }}
          sx={{ mb: 1 }}
        >
          <Typography variant="subtitle1" fontWeight={700}>
            Opciones en {category || '—'}
          </Typography>
          {showListChrome && (
            <Typography variant="body2" color="text.secondary">
              {filteredOptions.length !== options.length
                ? `${filteredOptions.length} filtradas de ${options.length} · ${filteredActiveCount} activas`
                : `${options.length} totales · ${activeCount} activas`}
            </Typography>
          )}
        </Stack>
        {hasOptions && (
          <Typography variant="caption" color="text.secondary" sx={{ display: 'block', mb: 1 }}>
            Guardar y Revertir aparecen solo en la fila que editas.
          </Typography>
        )}
        {showListChrome ? (
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} sx={{ mb: 1 }}>
            <TextField
              label="Filtrar opciones"
              placeholder="Busca por valor o etiqueta"
              value={optionFilter}
              onChange={(e) => setOptionFilter(e.target.value)}
              size="small"
              sx={{ minWidth: { xs: '100%', sm: 280 } }}
              InputProps={{
                endAdornment: showClearFilterAction ? (
                  <InputAdornment position="end">
                    <IconButton
                      size="small"
                      aria-label="Limpiar filtro"
                      onClick={() => setOptionFilter('')}
                    >
                      <ClearIcon fontSize="small" />
                    </IconButton>
                  </InputAdornment>
                ) : null,
              }}
            />
          </Stack>
        ) : showSingleOptionGuidance ? (
          <Typography variant="body2" color="text.secondary" sx={{ mb: 1 }}>
            Solo hay una opción por ahora. Edítala directo aquí; el filtro aparecerá cuando exista una segunda.
          </Typography>
        ) : null}
        {optionsQuery.isLoading ? (
          <Typography>Cargando opciones…</Typography>
        ) : options.length === 0 ? (
          <Alert severity="info">No hay opciones aún para esta categoría.</Alert>
        ) : filteredOptions.length === 0 ? (
          <Alert severity="info">
            No hay coincidencias para el filtro actual. Ajusta el texto para ver opciones.
          </Alert>
        ) : (
          <Box sx={{ overflowX: 'auto' }}>
            <Typography variant="caption" color="text.secondary" sx={{ display: 'block', mb: 1 }}>
              Desliza horizontalmente para editar todas las columnas en pantallas pequeñas.
            </Typography>
            <Table size="small" sx={{ minWidth: 900 }}>
              <TableHead>
                <TableRow>
                  <TableCell sx={{ whiteSpace: 'nowrap' }}>Valor</TableCell>
                  <TableCell sx={{ whiteSpace: 'nowrap' }}>Etiqueta</TableCell>
                  <TableCell sx={{ whiteSpace: 'nowrap' }}>Orden</TableCell>
                  <TableCell sx={{ whiteSpace: 'nowrap' }}>Activo</TableCell>
                  <TableCell align="right" sx={{ whiteSpace: 'nowrap' }}>Acciones</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {filteredOptions.map((opt) => (
                  <OptionRow
                    key={opt.optionId}
                    option={opt}
                    saving={savingId === opt.optionId && updateMutation.isPending}
                    onSave={async (optionId, payload) => {
                      if (Object.keys(payload).length === 0) return;
                      await updateMutation.mutateAsync({ optionId, payload });
                    }}
                  />
                ))}
              </TableBody>
            </Table>
          </Box>
        )}
      </Paper>
    </Stack>
  );
}
