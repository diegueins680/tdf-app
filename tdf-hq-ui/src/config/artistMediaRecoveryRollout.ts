/** Public rollout selection only; asset/source authorization remains server-side. */
export function isArtistMediaRecoveryEnabled(configured: unknown = import.meta.env?.VITE_ARTIST_MEDIA_RECOVERY_ENABLED): boolean {
  return configured === 'true';
}
