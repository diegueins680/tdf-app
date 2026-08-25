import * as React from 'react';
import { cents } from '../../api/client';
import {
  useLessonPackagesQuery,
  useCreateLessonPackageMutation,
} from '../../api/hq';
import type { components } from '../../api/generated/lessons-and-receipts';
import {
  Box,
  Button,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TextField,
  Typography,
} from '@mui/material';

type LessonPackage = components['schemas']['LessonPackage'];
type LessonPackageInput = components['schemas']['LessonPackageInput'];

type DraftState = {
  name: string;
  lessonsIncluded: number;
  priceCents: number;
  currency: string;
};

function buildDefaultDraft(): DraftState {
  return {
    name: '',
    lessonsIncluded: 4,
    priceCents: 0,
    currency: 'USD',
  };
}

export default function PackageList() {
  const packagesQuery = useLessonPackagesQuery();
  const createPackage = useCreateLessonPackageMutation({
    onSuccess: () => setDraft(buildDefaultDraft()),
  });

  const [draft, setDraft] = React.useState<DraftState>(buildDefaultDraft());

  const packages = (packagesQuery.data ?? []).filter(
    (pkg: LessonPackage) => pkg.is_active !== false,
  );

  const handleCreate = () => {
    const payload: LessonPackageInput = {
      name: draft.name.trim(),
      total_lessons: Number(draft.lessonsIncluded ?? 0),
      price_cents: Number(draft.priceCents ?? 0),
      currency: draft.currency ?? 'USD',
      is_active: true,
    };
    createPackage.mutate(payload);
  };

  return (
    <Stack spacing={3} p={2}>
      <Box component={Paper} sx={{ p: 2 }}>
        <Typography variant="h6" gutterBottom>Nuevo paquete rápido</Typography>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
          <TextField
            label="Nombre"
            value={draft.name}
            onChange={(event) => setDraft({ ...draft, name: event.target.value })}
            required
          />
          <TextField
            label="Clases incluidas"
            type="number"
            value={draft.lessonsIncluded}
            onChange={(event) => setDraft({ ...draft, lessonsIncluded: Number(event.target.value) })}
            sx={{ width: 180 }}
          />
          <TextField
            label="Precio (centavos)"
            type="number"
            value={draft.priceCents}
            onChange={(event) => setDraft({ ...draft, priceCents: Number(event.target.value) })}
            sx={{ width: 200 }}
          />
          <TextField
            label="Moneda"
            value={draft.currency}
            onChange={(event) => setDraft({ ...draft, currency: event.target.value })}
            sx={{ width: 140 }}
          />
          <Button
            variant="contained"
            onClick={handleCreate}
            disabled={createPackage.isPending || !draft.name.trim()}
          >
            {createPackage.isPending ? 'Guardando…' : 'Crear'}
          </Button>
        </Stack>
      </Box>

      <Paper>
        <Typography variant="h6" sx={{ p: 2 }}>Paquetes activos</Typography>
        <TableContainer>
          <Table size="small">
            <TableHead>
              <TableRow>
                <TableCell>Nombre</TableCell>
                <TableCell>Clases</TableCell>
                <TableCell>Precio</TableCell>
                <TableCell>Estado</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {packagesQuery.isLoading && (
                <TableRow>
                  <TableCell colSpan={4}>Cargando…</TableCell>
                </TableRow>
              )}
              {packagesQuery.isError && (
                <TableRow>
                  <TableCell colSpan={4} sx={{ color: 'error.main' }}>
                    {(packagesQuery.error as Error).message}
                  </TableCell>
                </TableRow>
              )}
              {packages.map(pkg => (
                <TableRow key={pkg.id}>
                  <TableCell>{pkg.name}</TableCell>
                  <TableCell>{pkg.total_lessons ?? '—'}</TableCell>
                  <TableCell>{cents(pkg.price?.amount_cents ?? 0, pkg.price?.currency ?? 'USD')}</TableCell>
                  <TableCell>{pkg.is_active ? 'Activo' : 'Inactivo'}</TableCell>
                </TableRow>
              ))}
              {!packagesQuery.isLoading && !packagesQuery.isError && packages.length === 0 && (
                <TableRow>
                  <TableCell colSpan={4} sx={{ color: 'text.secondary' }}>
                    No hay paquetes registrados todavía.
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </TableContainer>
      </Paper>
    </Stack>
  );
}
