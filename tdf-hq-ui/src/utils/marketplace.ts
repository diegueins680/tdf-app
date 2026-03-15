export interface OrderStatusMeta {
  label: string;
  color: 'success' | 'warning' | 'info' | 'default';
  desc: string;
}

const toTokenList = (status: string): string[] => {
  const normalized = status.toLowerCase();
  return normalized.split(/[^a-z0-9]+/).filter(Boolean);
};

const toTokenSet = (status: string): Set<string> => {
  return new Set(toTokenList(status));
};

const normalizeStatus = (status: string): string => status.trim();

// Map backend order status to badge copy and color.
export const getOrderStatusMeta = (status: string): OrderStatusMeta => {
  const normalizedStatus = normalizeStatus(status);
  const norm = normalizedStatus.toLowerCase();
  const tokenList = toTokenList(norm);
  const tokens = toTokenSet(norm);
  const hasToken = (...values: string[]) => values.some((value) => tokens.has(value));
  const hasFragment = (...values: string[]) => values.some((value) => norm.includes(value));
  const hasTokenPair = (left: string, right: string) =>
    tokenList.some((value, idx) => value === left && tokenList[idx + 1] === right);
  const hasPaid = () => hasToken('paid', 'success', 'successful', 'approved', 'complete', 'completed');
  const hasExplicitNotPaid = () =>
    hasToken('unpaid') ||
    hasFragment('not_paid', 'not-paid', 'not paid', 'unpaid') ||
    hasTokenPair('not', 'paid') ||
    hasTokenPair('un', 'paid');
  const hasRejected = () =>
    hasExplicitNotPaid() ||
    hasToken('fail', 'failed', 'reject', 'rejected', 'decline', 'declined', 'deny', 'denied', 'error', 'void', 'voided', 'expire', 'expired');
  const hasCancelled = () => hasToken('cancel', 'cancelled', 'canceled') || hasFragment('cancel');
  const hasRefunded = () => hasToken('refund', 'refunded', 'reversal', 'reversed', 'chargeback');

  if (hasToken('datafast') || hasFragment('datafast')) {
    if (hasRefunded()) {
      return { label: 'Reembolsado', color: 'default', desc: 'Pago con tarjeta reembolsado.' };
    }
    if (hasRejected()) {
      return { label: 'Pago rechazado', color: 'default', desc: 'El pago con tarjeta fue rechazado.' };
    }
    if (hasCancelled()) {
      return { label: 'Cancelado', color: 'default', desc: 'Pago con tarjeta cancelado.' };
    }
    if (hasPaid()) {
      return { label: 'Pagado', color: 'success', desc: 'Pago con tarjeta confirmado.' };
    }
    if (hasToken('init', 'created', 'new')) {
      return {
        label: 'Pago iniciado',
        color: 'info',
        desc: 'Se inició el pago con tarjeta. Falta confirmación del banco.',
      };
    }
    return {
      label: 'Pago en revisión',
      color: 'warning',
      desc: 'Pago con tarjeta en revisión. Si no se confirma en minutos, contáctanos.',
    };
  }

  if (hasToken('paypal') || hasFragment('paypal')) {
    if (hasRejected()) {
      return { label: 'Pago rechazado', color: 'default', desc: 'El pago de PayPal fue rechazado.' };
    }
    if (hasCancelled()) {
      return { label: 'Cancelado', color: 'default', desc: 'Pago de PayPal cancelado.' };
    }
    if (hasRefunded()) {
      return { label: 'Reembolsado', color: 'default', desc: 'Pago de PayPal reembolsado.' };
    }
    if (hasPaid()) {
      return { label: 'Pagado', color: 'success', desc: 'Pago PayPal confirmado.' };
    }
    if (hasToken('pending', 'init', 'review', 'processing', 'incomplete', 'action', 'required', 'created')) {
      return { label: 'Pendiente', color: 'warning', desc: 'Pago PayPal en revisión.' };
    }
    return { label: 'Pago en revisión', color: 'warning', desc: 'Pago PayPal en revisión.' };
  }

  if (hasRefunded()) {
    return { label: 'Reembolsado', color: 'default', desc: 'Pago reembolsado.' };
  }

  if (hasRejected()) {
    return { label: 'Pago rechazado', color: 'default', desc: 'El pago no pudo completarse.' };
  }

  if (hasCancelled()) {
    return { label: 'Cancelado', color: 'default', desc: 'Pedido cancelado.' };
  }

  if (hasToken('pending')) {
    return { label: 'Pendiente', color: 'warning', desc: 'Recibido, a la espera de confirmación.' };
  }

  if (hasPaid()) {
    return { label: 'Pagado', color: 'success', desc: 'Pago confirmado.' };
  }

  if (hasToken('process', 'processing') || hasToken('contact')) {
    return { label: 'En proceso', color: 'info', desc: 'Estamos coordinando pago/entrega.' };
  }

  if (hasToken('delivered')) {
    return { label: 'Completado', color: 'success', desc: 'Pedido entregado.' };
  }

  return { label: normalizedStatus || 'Estado', color: 'default', desc: '' };
};

export const isPaidOrderStatus = (status: string): boolean => {
  return getOrderStatusMeta(status).label === 'Pagado';
};

// Present human readable "last updated" text from a timestamp in ms.
export const formatLastSavedTimestamp = (updatedAt?: number | null): string | null => {
  if (updatedAt == null || !Number.isFinite(updatedAt) || updatedAt <= 0) return null;
  const diffMs = Math.max(0, Date.now() - updatedAt);
  const minutes = Math.floor(diffMs / 60000);
  if (minutes < 1) return 'Actualizado hace <1 min';
  if (minutes < 60) return `Actualizado hace ${minutes} min`;
  const hours = Math.floor(minutes / 60);
  return `Actualizado hace ${hours} h`;
};
