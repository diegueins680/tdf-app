import { API_BASE_URL } from '../api/client';
import { serializeCsvCell } from './csv';
import { formatEventMoney } from './eventFinanceMoney';
import { parseOptionalPriceCents } from './priceInput';

export const formatMerchMoney = (minor: number, currency = 'USD', locale = 'es-EC') =>
  formatEventMoney(minor, currency, locale);

export const readMerchMoneyInput = (
  raw: string,
  language: 'es' | 'en',
  { required = false, positive = false }: { required?: boolean; positive?: boolean } = {},
): { minor: number | null; error: string | null } => {
  try {
    const minor = parseOptionalPriceCents(raw);
    if (required && minor === null) return { minor, error: language === 'en' ? 'Enter an amount.' : 'Ingresa un monto.' };
    if (positive && minor === 0) return { minor, error: language === 'en' ? 'The amount must be greater than zero.' : 'El monto debe ser mayor que cero.' };
    return { minor, error: null };
  } catch {
    return { minor: null, error: language === 'en'
      ? 'Use a non-negative amount with up to two decimals, within the supported range.'
      : 'Usa un monto no negativo con hasta dos decimales, dentro del rango admitido.' };
  }
};

export const resolveMerchImageUrl = (value?: string | null) => {
  if (!value) return undefined;
  if (/^https:\/\//i.test(value)) return value;
  const base = API_BASE_URL.replace(/\/$/, '');
  return `${base}/${value.replace(/^\//, '')}`;
};

export const merchStatusLabel = (status: string, language: 'es' | 'en') => {
  const labels: Record<string, readonly [string, string]> = {
    requested: ['Solicitada', 'Requested'],
    under_review: ['En revisión', 'Under review'],
    approved: ['Aprobada', 'Approved'],
    active: ['Activa', 'Active'],
    suspended: ['Suspendida', 'Suspended'],
    draft: ['Borrador', 'Draft'],
    pending_review: ['En revisión', 'Under review'],
    published: ['Publicado', 'Published'],
    sold_out: ['Agotado', 'Sold out'],
    paused: ['Pausado', 'Paused'],
    rejected: ['Rechazado', 'Rejected'],
    archived: ['Archivado', 'Archived'],
    pending: ['Pendiente', 'Pending'],
    processing: ['Procesando', 'Processing'],
    succeeded: ['Completado', 'Succeeded'],
    failed: ['Fallido', 'Failed'],
    cancelled: ['Cancelado', 'Cancelled'],
    inquiry: ['Consulta', 'Inquiry'],
    dispute: ['Disputa', 'Dispute'],
    chargeback: ['Contracargo', 'Chargeback'],
    needs_response: ['Requiere respuesta', 'Needs response'],
    won: ['Ganada', 'Won'],
    lost: ['Perdida', 'Lost'],
    preparing: ['En preparación', 'Preparing'],
    ready_for_pickup: ['Listo para retirar', 'Ready for pickup'],
    shipped: ['Enviado', 'Shipped'],
    delivered: ['Entregado', 'Delivered'],
    problem: ['Con incidencia', 'Issue reported'],
    // Existing server states and issue types; these labels do not advance any
    // workflow or equate support review/approval with movement of funds.
    withdrawn: ['Solicitud retirada', 'Application withdrawn'],
    inactive: ['Inactiva', 'Inactive'],
    closed: ['Cerrada', 'Closed'],
    created: ['Creado', 'Created'],
    confirmed: ['Confirmado', 'Confirmed'],
    completed: ['Completado', 'Completed'],
    paid: ['Pagado', 'Paid'],
    partially_refunded: ['Reembolsado parcialmente', 'Partially refunded'],
    refunded: ['Reembolsado', 'Refunded'],
    disputed: ['En disputa', 'Disputed'],
    return_requested: ['Devolución solicitada', 'Return requested'],
    returned: ['Devuelto', 'Returned'],
    none: ['Ninguno', 'None'],
    partial: ['Parcial', 'Partial'],
    open: ['Abierto', 'Open'],
    not_ready: ['Aún no disponible', 'Not ready'],
    ready: ['Listo', 'Ready'],
    held: ['Retenido', 'Held'],
    adjusted: ['Ajustado', 'Adjusted'],
    reversed: ['Revertido', 'Reversed'],
    seller_review: ['En revisión del vendedor', 'Under seller review'],
    staff_review: ['En revisión del equipo TDF', 'Under TDF staff review'],
    awaiting_buyer: ['Esperando al comprador', 'Awaiting buyer'],
    resolved: ['Resuelto', 'Resolved'],
    general: ['Consulta general', 'General inquiry'],
    address: ['Dirección', 'Address'],
    stock: ['Existencias', 'Stock'],
    shipping: ['Envío', 'Shipping'],
    damaged: ['Producto dañado', 'Damaged product'],
    missing: ['Producto faltante', 'Missing product'],
    cancellation: ['Cancelación', 'Cancellation'],
    return: ['Devolución', 'Return'],
    refund: ['Reembolso', 'Refund'],
    fraud: ['Posible fraude', 'Suspected fraud'],
  };
  return labels[status]?.[language === 'en' ? 1 : 0] ?? status.replace(/_/g, ' ');
};

export const merchLanguage = (resolvedLanguage?: string): 'es' | 'en' =>
  resolvedLanguage?.toLowerCase().startsWith('en') ? 'en' : 'es';

export interface MerchOrderExportRow {
  orderNumber: string;
  createdAt?: string;
  commercialStatus: string;
  paymentStatus: string;
  fulfillmentStatus: string;
  refundStatus: string;
  disputeStatus: string;
  settlementStatus?: string;
  currency: string;
  productSubtotalMinor: number;
  taxMinor: number;
  shippingMinor: number;
  totalMinor: number;
  tdfCommissionMinor?: number;
  sellerNetMinor?: number;
}

export const buildMerchOrdersCsv = (
  orders: MerchOrderExportRow[],
  language: 'es' | 'en',
) => {
  const includesFinance = orders.some(
    (order) => order.tdfCommissionMinor !== undefined || order.sellerNetMinor !== undefined,
  );
  const commonHeaders = language === 'en'
    ? ['Order', 'Created at', 'Commercial status', 'Payment status', 'Fulfillment status', 'Refund status', 'Dispute status', 'Settlement status', 'Currency', 'Product subtotal (minor)', 'Tax (minor)', 'Shipping (minor)', 'Total (minor)']
    : ['Pedido', 'Creado', 'Estado comercial', 'Estado de pago', 'Estado de entrega', 'Estado de reembolso', 'Estado de disputa', 'Estado de liquidación', 'Moneda', 'Subtotal de productos (minor)', 'Impuestos (minor)', 'Envío (minor)', 'Total (minor)'];
  const financeHeaders = language === 'en'
    ? ['TDF commission (minor)', 'Seller net (minor)']
    : ['Comisión TDF (minor)', 'Neto del vendedor (minor)'];
  const rows = orders.map((order) => {
    const common = [
      order.orderNumber,
      order.createdAt,
      order.commercialStatus,
      order.paymentStatus,
      order.fulfillmentStatus,
      order.refundStatus,
      order.disputeStatus,
      order.settlementStatus,
      order.currency,
      order.productSubtotalMinor,
      order.taxMinor,
      order.shippingMinor,
      order.totalMinor,
    ];
    const finance = includesFinance ? [order.tdfCommissionMinor, order.sellerNetMinor] : [];
    return [...common, ...finance].map(serializeCsvCell).join(',');
  });

  // The BOM makes UTF-8 Spanish headers open reliably in common spreadsheet tools.
  return `\uFEFF${[...commonHeaders, ...(includesFinance ? financeHeaders : [])].map(serializeCsvCell).join(',')}\r\n${rows.join('\r\n')}\r\n`;
};
