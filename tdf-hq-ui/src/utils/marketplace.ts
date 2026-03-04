export interface OrderStatusMeta {
  label: string;
  color: 'success' | 'warning' | 'info' | 'default';
  desc: string;
}

const toTokenSet = (status: string): Set<string> => {
  const normalized = status.toLowerCase();
  return new Set(normalized.split(/[^a-z0-9]+/).filter(Boolean));
};

// Map backend order status to badge copy and color.
export const getOrderStatusMeta = (status: string): OrderStatusMeta => {
  const norm = status.toLowerCase();
  const tokens = toTokenSet(norm);
  const hasToken = (...values: string[]) => values.some((value) => tokens.has(value));
  const hasFragment = (...values: string[]) => values.some((value) => norm.includes(value));

  if (hasToken('datafast') || hasFragment('datafast')) {
    if (hasToken('fail', 'failed', 'reject', 'rejected', 'decline', 'declined', 'error')) {
      return { label: 'Pago rechazado', color: 'default', desc: 'El pago con tarjeta fue rechazado.' };
    }
    if (hasToken('paid', 'success', 'successful', 'approved', 'complete', 'completed')) {
      return { label: 'Pagado', color: 'success', desc: 'Pago con tarjeta confirmado.' };
    }
    return {
      label: 'Pago en revisión',
      color: 'warning',
      desc: 'Pago con tarjeta en revisión. Si no se confirma en minutos, contáctanos.',
    };
  }

  if (hasToken('paypal') || hasFragment('paypal')) {
    if (hasToken('pending', 'init', 'review', 'processing')) {
      return { label: 'Pendiente', color: 'warning', desc: 'Pago PayPal en revisión.' };
    }
    if (hasToken('paid', 'success', 'successful', 'approved', 'complete', 'completed')) {
      return { label: 'Pagado', color: 'success', desc: 'Pago PayPal confirmado.' };
    }
    return { label: 'Pago en revisión', color: 'warning', desc: 'Pago PayPal en revisión.' };
  }

  if (hasToken('unpaid', 'failed', 'rejected', 'declined', 'error')) {
    return { label: 'Pago rechazado', color: 'default', desc: 'El pago no pudo completarse.' };
  }

  if (hasToken('pending')) {
    return { label: 'Pendiente', color: 'warning', desc: 'Recibido, a la espera de confirmación.' };
  }

  if (hasToken('paid', 'success', 'successful', 'approved', 'complete', 'completed')) {
    return { label: 'Pagado', color: 'success', desc: 'Pago confirmado.' };
  }

  if (hasToken('process', 'processing') || hasToken('contact')) {
    return { label: 'En proceso', color: 'info', desc: 'Estamos coordinando pago/entrega.' };
  }

  if (hasToken('delivered')) {
    return { label: 'Completado', color: 'success', desc: 'Pedido entregado.' };
  }

  if (hasFragment('cancel')) {
    return { label: 'Cancelado', color: 'default', desc: 'Pedido cancelado.' };
  }

  return { label: status || 'Estado', color: 'default', desc: '' };
};

// Present human readable "last updated" text from a timestamp in ms.
export const formatLastSavedTimestamp = (updatedAt?: number | null): string | null => {
  if (!updatedAt) return null;
  const diffMs = Math.max(0, Date.now() - updatedAt);
  const minutes = Math.floor(diffMs / 60000);
  if (minutes < 1) return 'Actualizado hace <1 min';
  if (minutes < 60) return `Actualizado hace ${minutes} min`;
  const hours = Math.floor(minutes / 60);
  return `Actualizado hace ${hours} h`;
};
