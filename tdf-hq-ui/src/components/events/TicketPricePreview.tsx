import { Alert, Stack, Typography } from '@mui/material';
import { estimateTicketFees, parseOptionalPriceCents } from '../../utils/eventTicketPricing';
import { formatCurrency } from '../../utils/formatters';

export default function TicketPricePreview({ price, currency, locale }: {
  price: string; currency: string; locale: string;
}) {
  try {
    const cents = parseOptionalPriceCents(price);
    if (cents === null) return null;
    const fee = estimateTicketFees(cents);
    const money = (value: number) => formatCurrency(value / 100, currency, locale);
    return <Alert severity="info" role="status">
      <Stack spacing={0.5}>
        <Typography variant="body2" fontWeight={700}>Si vendes con TDF: total estimado {money(fee.checkout)}</Typography>
        <Typography variant="body2">Precio base {money(cents)} + comisión al comprador {money(fee.buyer)}. Recibes {money(fee.proceeds)} por entrada, descontada tu comisión de {money(fee.organizer)}.</Typography>
        <Typography variant="caption">Estimación con la tarifa estándar del 4%, repartida entre comprador y organizador. El checkout confirma las comisiones vigentes y cualquier impuesto o descuento.</Typography>
      </Stack>
    </Alert>;
  } catch (error) {
    return <Alert severity="warning">{error instanceof Error ? error.message : 'Revisa el precio.'}</Alert>;
  }
}
