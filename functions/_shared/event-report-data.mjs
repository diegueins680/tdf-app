// Server-only, organizer-authorized snapshot. Never import this module into the
// React app or copy it to public/. Reported amounts are not ledger postings.
export const patchCultureReport = Object.freeze({
  tokenSha256: 'a7fd41516bb89166f44ff4ce0a8de538db445eb55b25f653a491a9f2e008e13a',
  eventId: '141',
  title: 'PATCH CULTURE Vol. 1',
  date: '24 de octubre de 2026',
  venue: 'Andes Brewing · La Pradera, Quito',
  snapshotDate: '26 de octubre de 2026',
  currency: 'USD',
  reportedGrossCents: 8000,
  owedToVenueCents: 3500,
  provisionalProfitCents: 4500,
  provisionalShareCents: 2250,
  beer: [
    { label: 'Incluidas en cuatro entradas', pints: 4, costCents: 2000 },
    { label: 'Vendidas por separado a USD5', pints: 2, costCents: 1000 },
    { label: 'Consumo personal de Diego', pints: 1, costCents: 500 },
  ],
  reconciliation: 'Cuatro entradas al precio registrado de USD20 más dos cervezas vendidas a USD5 darían USD90. El resumen compartido reporta USD80 brutos: hay una diferencia de USD10 por conciliar. Falta confirmar los importes cobrados por las entradas y si el total incluye las ventas de cerveza. USD90 es un cálculo condicionado, no un ingreso confirmado.',
  pending: [
    'Identificar quién recibió la transferencia de USD20.',
    'Confirmar la comisión de Meet2Go, el importe neto y su acreditación.',
    'Identificar quién pagará los USD35 a Andes y comprobar el pago.',
    'Identificar las dos partes del reparto y calcular la transferencia final después de conciliar los fondos.',
  ],
  accounting: 'La consulta del 26 de octubre de 2026 a las 21:35, hora de Ecuador, mostró una orden de ticket pendiente por USD20, con ingresos y gastos contabilizados en cero. Esa orden no acredita cobro ni se identifica automáticamente con la transferencia de USD20 mencionada. El cierre se conservó como nota; estos importes no son cobros ni pagos verificados.',
  source: 'Resumen financiero compartido por el organizador. Los comprobantes y el Excel mencionado no estaban incluidos. El nombre y la fecha del evento se contrastaron con la ficha de TDF. Esta es una copia del cierre provisional al 26 de octubre, no un saldo actualizado en tiempo real.',
});
