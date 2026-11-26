/** Public build-time rollout control for MEDIA-MUSIC-CONTINUATION-001; not an authorization boundary. */
export function isQueueContinuationEnabled(
  configured: unknown = import.meta.env?.VITE_PLAYER_SIMILAR_CONTINUATION_ENABLED,
): boolean {
  return configured === 'true';
}
