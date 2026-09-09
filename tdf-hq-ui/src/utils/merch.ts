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
