import type { RoomDTO } from '../api/types';

export const CHECKOUT_DISPOSITION_OPTIONS = [
  { value: 'loan', label: 'Préstamo' },
  { value: 'rental', label: 'Alquiler' },
  { value: 'sale', label: 'Venta' },
  { value: 'repair', label: 'Mantenimiento' },
  { value: 'transfer', label: 'Transferencia' },
  { value: 'other', label: 'Otro' },
] as const;

export const CHECKOUT_TARGET_OPTIONS = [
  { value: 'party', label: 'Persona / externo' },
  { value: 'room', label: 'Sala / cuarto' },
  { value: 'session', label: 'Sesión' },
] as const;

export const CHECKOUT_PAYMENT_TYPE_OPTIONS = [
  { value: 'cash', label: 'Efectivo' },
  { value: 'bank_transfer', label: 'Transferencia' },
  { value: 'card', label: 'Tarjeta' },
  { value: 'paypal', label: 'PayPal' },
  { value: 'stripe', label: 'Stripe' },
  { value: 'wompi', label: 'Wompi' },
  { value: 'payphone', label: 'PayPhone' },
  { value: 'crypto', label: 'Crypto' },
  { value: 'other', label: 'Otro' },
] as const;

export function getCheckoutDispositionLabel(value?: string | null) {
  if (!value) return 'Sin clasificar';
  const normalized = value.trim().toLowerCase();
  return CHECKOUT_DISPOSITION_OPTIONS.find((option) => option.value === normalized)?.label ?? value;
}

export function getCheckoutTargetLabel(value?: string | null) {
  if (!value) return 'Sin destino';
  const normalized = value.trim().toLowerCase();
  return CHECKOUT_TARGET_OPTIONS.find((option) => option.value === normalized)?.label ?? value;
}

export function getCheckoutPaymentTypeLabel(value?: string | null) {
  if (!value) return 'Sin pago';
  const normalized = value.trim().toLowerCase();
  return CHECKOUT_PAYMENT_TYPE_OPTIONS.find((option) => option.value === normalized)?.label ?? value;
}

export function checkoutSupportsReturnDate(disposition?: string | null) {
  return (disposition?.trim().toLowerCase() ?? '') !== 'sale';
}

export function checkoutSupportsPaymentDetails(disposition?: string | null) {
  const normalized = disposition?.trim().toLowerCase() ?? '';
  return normalized === 'sale' || normalized === 'rental';
}

export function formatCheckoutPaymentSummary(paymentType?: string | null, installments?: number | null) {
  const parts = [];
  if (paymentType) parts.push(getCheckoutPaymentTypeLabel(paymentType));
  if (typeof installments === 'number' && Number.isFinite(installments) && installments > 0) {
    parts.push(`${installments} ${installments === 1 ? 'cuota' : 'cuotas'}`);
  }
  return parts.join(' · ');
}

export function formatCheckoutTargetDisplay(
  targetKind?: string | null,
  targetRef?: string | null,
  roomMap?: Map<string, RoomDTO>,
) {
  const normalizedKind = targetKind?.trim().toLowerCase() ?? '';
  const trimmedTarget = targetRef?.trim() ?? '';
  if (trimmedTarget === '') return getCheckoutTargetLabel(targetKind);
  if (normalizedKind === 'room') {
    return roomMap?.get(trimmedTarget)?.rName ?? trimmedTarget;
  }
  return trimmedTarget;
}
