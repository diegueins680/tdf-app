/** Public rollout control only. Record authorization remains server-owned. */
export function isEventAgreementsEnabled(
  configured: unknown = import.meta.env?.VITE_EVENT_AGREEMENTS_ENABLED,
): boolean {
  return configured === 'true';
}
