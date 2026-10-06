/** Public build-time rollout control, not an authorization boundary. */
export function isAccountDeletionFormEnabled(
  configured: unknown = import.meta.env?.VITE_ACCOUNT_DELETION_FORM_ENABLED,
): boolean {
  return configured === 'true';
}

/** Keep operator processing independent from pauses in new intake. */
export function isAccountDeletionQueueEnabled(
  configured: unknown = import.meta.env?.VITE_ACCOUNT_DELETION_QUEUE_ENABLED,
): boolean {
  return configured === 'true';
}
