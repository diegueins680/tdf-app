export type OrderStatusMeta = {
  label: string;
  color: 'success' | 'warning' | 'info' | 'default';
  desc: string;
};

// Map backend order status to badge copy and color.
export const getOrderStatusMeta = (status: string): OrderStatusMeta => {
  const norm = status.toLowerCase();
  if (norm.includes('paid') || norm.includes('paypal')) {
    return { label: 'Pagado', color: 'success', desc: 'Pago recibido vía PayPal.' };
  }
  if (norm.includes('pending')) {
    return { label: 'Pendiente', color: 'warning', desc: 'Recibido, a la espera de confirmación.' };
  }
  if (norm.includes('process') || norm.includes('contact')) {
    return { label: 'En proceso', color: 'info', desc: 'Estamos coordinando pago/entrega.' };
  }
  if (norm.includes('delivered') || norm.includes('complete')) {
    return { label: 'Completado', color: 'success', desc: 'Pedido entregado.' };
  }
  if (norm.includes('cancel')) {
    return { label: 'Cancelado', color: 'default', desc: 'Pedido cancelado.' };
  }
  return { label: status || 'Estado', color: 'default', desc: '' };
};

// Present human readable "last updated" text from a timestamp in ms.
export const formatLastSavedTimestamp = (updatedAt?: number | null): string | null => {
  if (!updatedAt) return null;
  const diffMs = Date.now() - updatedAt;
  const minutes = Math.floor(diffMs / 60000);
  if (minutes < 1) return 'Actualizado hace <1 min';
  if (minutes < 60) return `Actualizado hace ${minutes} min`;
  const hours = Math.floor(minutes / 60);
  return `Actualizado hace ${hours} h`;
};
