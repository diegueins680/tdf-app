import { useQuery } from '@tanstack/react-query';
import { Alert, CircularProgress, Stack, Typography } from '@mui/material';

import { Directory, type DirectoryTaxonomyItem, type MarketplaceAgreement } from '../../api/directory';
import AgreementCard from './AgreementCard';

const disputeOpener: Record<string, string> = { buyer: 'el cliente', provider: 'el proveedor', admin: 'administración' };

/**
 * One disputed marketplace agreement (ADR 0136): who opened the dispute, from
 * which state and why, and the agreement card with the two administrator
 * resolutions (complete or cancel), each bound to the version shown.
 */
function DisputeCard({ agreement, currencies }: { agreement: MarketplaceAgreement; currencies?: DirectoryTaxonomyItem[] }) {
  const dispute = agreement.dispute;
  const openedAt = dispute?.openedAt
    ? new Intl.DateTimeFormat('es', { dateStyle: 'medium', timeStyle: 'short' }).format(new Date(dispute.openedAt))
    : null;
  const reason = dispute?.reason?.trim() ?? '';
  return (
    <Stack spacing={1} data-testid="agreement-dispute">
      <Alert severity="warning">
        <Typography fontWeight={700}>
          Cliente: {agreement.buyerProfile.name} · Proveedor: {agreement.providerProfile.name}
        </Typography>
        <Typography variant="body2">
          Disputa abierta por {disputeOpener[dispute?.openedBy ?? ''] ?? 'una de las partes'}
          {openedAt !== null ? ` el ${openedAt}` : ''}
          {dispute?.previousStatus ? ` (estado anterior: ${dispute.previousStatus})` : ''}.
        </Typography>
        <Typography variant="body2">Motivo: {reason !== '' ? reason : 'sin motivo registrado'}</Typography>
        <Typography variant="body2">
          Completar habilita las reseñas; cancelar libera el cupo del anuncio. Ninguna de las dos mueve dinero: una devolución se gestiona con el proveedor de pagos.
        </Typography>
      </Alert>
      <AgreementCard agreement={agreement} currencies={currencies} />
    </Stack>
  );
}

/**
 * Administrator queue of disputed marketplace agreements, oldest dispute
 * first. Loading it requires the Admin role and module; the server decides.
 */
export default function AgreementDisputesPanel() {
  const disputes = useQuery({ queryKey: ['directory', 'admin', 'agreement-disputes'], queryFn: Directory.adminAgreementDisputes });
  const taxonomies = useQuery({ queryKey: ['directory', 'taxonomies'], queryFn: () => Directory.taxonomies() });
  return (
    <Stack spacing={2} component="section" aria-labelledby="agreement-disputes-heading" data-testid="agreement-disputes">
      <Typography id="agreement-disputes-heading" component="h2" variant="h5" fontWeight={800}>Disputas de acuerdos</Typography>
      {disputes.isLoading && <CircularProgress aria-label="Cargando disputas de acuerdos" />}
      {disputes.isError && <Alert severity="error">No tienes acceso a las disputas de acuerdos o no se pudieron cargar.</Alert>}
      {disputes.data?.length === 0 && <Alert severity="info">No hay acuerdos en disputa.</Alert>}
      {disputes.data?.map((agreement) => <DisputeCard key={agreement.id} agreement={agreement} currencies={taxonomies.data?.currencies} />)}
    </Stack>
  );
}
