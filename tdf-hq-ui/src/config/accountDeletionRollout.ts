/** Public build-time rollout control, not an authorization boundary. */
export function isAccountDeletionFormEnabled(
  configured: unknown = import.meta.env?.VITE_ACCOUNT_DELETION_FORM_ENABLED,
): boolean {
  return configured === 'true';
}
