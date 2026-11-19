// Presentation only. State authority and transitions remain in the backend runtime.
const FULFILLMENT_STATUS_LABELS: Record<string, string> = {
  on_hold: 'Reserva activa',
  ready_to_fulfill: 'Lista para preparar',
  picking: 'En preparación',
  ready_for_pickup: 'Lista para retiro',
  shipped: 'Enviada',
  delivered: 'Entregada',
  cancellation_requested: 'Cancelación solicitada',
  cancelled: 'Cancelada',
  return_requested: 'Devolución solicitada',
  return_authorized: 'Devolución autorizada',
  return_in_transit: 'Devolución en tránsito',
  returned: 'Devuelta',
  closed: 'Cerrada',
  expired: 'Reserva vencida',
};

const RENTAL_STATUS_LABELS: Record<string, string> = {
  on_hold: 'Reserva con vencimiento',
  confirmed: 'Renta confirmada',
  ready_for_handoff: 'Lista para entrega',
  checked_out: 'En custodia del cliente',
  return_due: 'Devolución pendiente',
  returned_pending_inspection: 'Devuelta; inspección pendiente',
  damage_review: 'Daños en revisión',
  deposit_refund_due: 'Devolución de depósito pendiente',
  closed: 'Renta cerrada',
  cancellation_requested: 'Cancelación solicitada',
  cancelled: 'Renta cancelada',
  no_show: 'Cliente no se presentó',
  lost: 'Activo perdido',
  disputed: 'Renta disputada',
  expired: 'Reserva vencida',
};

const DEPOSIT_STATUS_LABELS: Record<string, string> = {
  awaiting_payment: 'Pendiente de pago',
  collected: 'Cobrado',
  inspection_pending: 'Inspección pendiente',
  deduction_proposed: 'Deducción propuesta',
  refund_due: 'Devolución pendiente',
  partial_refund_due: 'Devolución parcial pendiente',
  refunded: 'Devuelto',
  partially_refunded: 'Devuelto parcialmente',
  forfeited: 'Depósito retenido',
  disputed: 'En disputa',
};

const DEPOSIT_SETTLEMENT_STATUS_LABELS: Record<string, string> = {
  submitted: 'Evidencia en revisión',
  verified: 'Evidencia verificada',
  rejected: 'Evidencia rechazada',
  requires_reconciliation: 'Requiere conciliación',
};

const DEPOSIT_SETTLEMENT_METHOD_LABELS: Record<string, string> = {
  bank_transfer: 'Transferencia bancaria',
  cash: 'Efectivo',
  pos: 'POS',
  forfeiture: 'Retención total documentada',
};

// Missing and unfamiliar values must never imply payment, custody or settlement.
function displayLabel(labels: Record<string, string>, value?: string | null): string {
  if (!value) return 'Sin información';
  return Object.prototype.hasOwnProperty.call(labels, value) ? labels[value]! : value;
}

export function marketplaceFulfillmentStatusLabel(value?: string | null, orderKind?: string | null): string {
  return displayLabel(orderKind === 'rental' ? RENTAL_STATUS_LABELS : FULFILLMENT_STATUS_LABELS, value);
}

export function marketplaceDepositStatusLabel(value?: string | null): string {
  return displayLabel(DEPOSIT_STATUS_LABELS, value);
}

export function marketplaceDepositSettlementStatusLabel(value?: string | null): string {
  return displayLabel(DEPOSIT_SETTLEMENT_STATUS_LABELS, value);
}

export function marketplaceDepositSettlementMethodLabel(value?: string | null): string {
  return displayLabel(DEPOSIT_SETTLEMENT_METHOD_LABELS, value);
}
