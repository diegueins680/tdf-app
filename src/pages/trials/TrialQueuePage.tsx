import { useMemo } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Container, Typography, Paper, Table, TableHead, TableRow, TableCell, TableBody, Button, Chip, Stack } from '@mui/material';
import { Trials } from '../../api/trials';
import type { TrialRequestDTO } from '../../api/types';
import { useAuth } from '../../auth/AuthProvider';

const STATUS_COLORS: Record<string, 'default' | 'primary' | 'success' | 'warning' | 'error'> = {
  Requested: 'warning',
  Assigned: 'primary',
  Scheduled: 'success',
  Completed: 'default',
  Cancelled: 'error',
};

export default function TrialQueuePage() {
  const qc = useQueryClient();
  const { user } = useAuth();
  const { data: requests = [], isLoading } = useQuery({
    queryKey: ['trial-requests'],
    queryFn: () => Trials.listTrialRequests({}),
  });

  const assign = useMutation({
    mutationFn: (requestId: number) => {
      if (!user?.partyId) {
        throw new Error('Debes iniciar sesión para tomar la solicitud.');
      }
      return Trials.assignTrial(requestId, { teacherId: user.partyId });
    },
    onSuccess: () => qc.invalidateQueries({ queryKey: ['trial-requests'] }),
  });

  const grouped = useMemo(() => {
    return requests.reduce<Record<string, number>>((acc, item) => {
      const key = item.status ?? 'Desconocido';
      acc[key] = (acc[key] ?? 0) + 1;
      return acc;
    }, {});
  }, [requests]);

  return (
    <Container sx={{ py: 4 }} maxWidth="lg">
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" alignItems={{ xs: 'flex-start', md: 'center' }} mb={3} spacing={2}>
        <div>
          <Typography variant="h4" gutterBottom>Solicitudes de prueba</Typography>
          <Typography color="text.secondary">Asignar y coordinar nuevas clases de introducción.</Typography>
        </div>
        <Stack direction="row" spacing={1} flexWrap="wrap">
          {Object.entries(grouped).map(([status, count]) => (
            <Chip key={status} label={`${status}: ${count}`} color={STATUS_COLORS[status] ?? 'default'} />
          ))}
        </Stack>
      </Stack>
      <Paper>
        <Table size="small">
          <TableHead>
            <TableRow>
              <TableCell>ID</TableCell>
              <TableCell>Materia</TableCell>
              <TableCell>Estado</TableCell>
              <TableCell>Notas</TableCell>
              <TableCell>Creada</TableCell>
              <TableCell align="right" width={160}>Acciones</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {requests.map((request: TrialRequestDTO) => (
              <TableRow key={request.requestId} hover>
                <TableCell>{request.requestId}</TableCell>
                <TableCell>{request.subjectName ?? request.subjectId}</TableCell>
                <TableCell>
                  <Chip label={request.status} size="small" color={STATUS_COLORS[request.status ?? ''] ?? 'default'} />
                </TableCell>
                <TableCell>{request.notes ?? '—'}</TableCell>
                <TableCell>{request.createdAt ? new Date(request.createdAt).toLocaleString() : '—'}</TableCell>
                <TableCell align="right">
                  <Button
                    size="small"
                    variant="outlined"
                    onClick={() => assign.mutate(request.requestId)}
                    disabled={!user?.partyId || assign.isPending || request.status !== 'Requested'}
                  >
                    Tomar
                  </Button>
                </TableCell>
              </TableRow>
            ))}
            {!isLoading && requests.length === 0 && (
              <TableRow>
                <TableCell colSpan={6} align="center" sx={{ py: 4 }}>
                  No hay solicitudes en espera.
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </Paper>
    </Container>
  );
}
