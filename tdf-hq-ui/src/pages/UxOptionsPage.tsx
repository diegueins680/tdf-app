import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Checkbox,
  FormControlLabel,
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
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Admin } from '../api/admin';
import type { DropdownOptionDTO, DropdownOptionUpdate } from '../api/types';

const DEFAULT_CATEGORIES = ['band-genre', 'band-role'];

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
  const sortInvalid = sortOrder.trim() !== '' && Number.isNaN(sortNumber);
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
        <Button
          variant="outlined"
          size="small"
          disabled={!dirty || sortInvalid || saving}
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
          disabled={!dirty || saving}
          onClick={() => {
            setValue(option.value);
            setLabel(option.label ?? '');
            setSortOrder(option.sortOrder?.toString() ?? '');
            setActive(option.active);
          }}
        >
          Revertir
        </Button>
      </TableCell>
    </TableRow>
  );
}

export default function UxOptionsPage() {
  const qc = useQueryClient();
  const [category, setCategory] = useState<string>(DEFAULT_CATEGORIES[0] ?? '');
  const [includeInactive, setIncludeInactive] = useState(false);
  const [newOption, setNewOption] = useState({ value: '', label: '', sortOrder: '', active: true });
  const [error, setError] = useState<string | null>(null);

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
      setNewOption({ value: '', label: '', sortOrder: '', active: true });
      setError(null);
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

  const handleCreate = () => {
    const cleanValue = newOption.value.trim();
    if (!cleanValue) {
      setError('Agrega un valor para la opción.');
      return;
    }
    if (newOption.sortOrder.trim() && Number.isNaN(Number(newOption.sortOrder.trim()))) {
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
          <Typography variant="subtitle1" fontWeight={700}>
            Nueva opción
          </Typography>
          <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
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
        </Stack>
      </Paper>

      <Paper sx={{ p: 2.5 }}>
        <Typography variant="subtitle1" fontWeight={700} gutterBottom>
          Opciones en {category || '—'}
        </Typography>
        {optionsQuery.isLoading ? (
          <Typography>Cargando opciones…</Typography>
        ) : options.length === 0 ? (
          <Alert severity="info">No hay opciones aún para esta categoría.</Alert>
        ) : (
          <Table size="small">
            <TableHead>
              <TableRow>
                <TableCell>Valor</TableCell>
                <TableCell>Etiqueta</TableCell>
                <TableCell>Orden</TableCell>
                <TableCell>Activo</TableCell>
                <TableCell align="right">Acciones</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {options.map((opt) => (
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
        )}
      </Paper>
    </Stack>
  );
}
