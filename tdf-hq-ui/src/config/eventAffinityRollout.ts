/** Public build-time rollout control for EVT-AFFINITY-ORDER-001; not an authorization boundary. */
export function isEventAffinityOrderEnabled(
  configured: unknown = import.meta.env?.VITE_EVENT_AFFINITY_ORDER_ENABLED,
): boolean {
  return configured === 'true';
}
