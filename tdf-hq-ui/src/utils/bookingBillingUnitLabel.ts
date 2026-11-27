/** Display translations of the service catalog's seeded English units. */
const BILLING_UNIT_LABELS = {
  hour: 'hora',
  episode: 'episodio',
  song: 'canción',
  package: 'paquete',
} as const;

/** Only formats text: callers must retain the original unit for pricing and API requests. */
export function bookingBillingUnitLabel(unit: string | null | undefined): string {
  const value = unit?.trim() ?? '';
  const key = value.toLowerCase();
  return Object.prototype.hasOwnProperty.call(BILLING_UNIT_LABELS, key)
    ? BILLING_UNIT_LABELS[key as keyof typeof BILLING_UNIT_LABELS]
    : value;
}
