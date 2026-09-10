import { API_BASE_URL } from '../api/client';

export const formatMerchMoney = (minor: number, currency = 'USD', locale = 'es-EC') =>
  new Intl.NumberFormat(locale, { style: 'currency', currency }).format(minor / 100);

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

const safeCsvCell = (value: string | number | undefined) => {
  let text = value === undefined ? '' : String(value);
  // Spreadsheet applications may execute cells beginning with these characters.
  // Prefixing an apostrophe keeps exported, user-influenced identifiers as text.
  if (/^\s*[=+\-@]/.test(text)) text = `'${text}`;
  return `"${text.replace(/"/g, '""')}"`;
};

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
    return [...common, ...finance].map(safeCsvCell).join(',');
  });

  // The BOM makes UTF-8 Spanish headers open reliably in common spreadsheet tools.
  return `\uFEFF${[...commonHeaders, ...(includesFinance ? financeHeaders : [])].map(safeCsvCell).join(',')}\r\n${rows.join('\r\n')}\r\n`;
};
