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
