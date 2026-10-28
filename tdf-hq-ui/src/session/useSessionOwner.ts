import { useRef } from 'react';
import { getAuthSessionEpoch } from './authEvents';

/** Revoke captured callbacks synchronously when credentials or editor scope change. */
export function useSessionOwner(ownerId: number | null, scope = '') {
  const authEpoch = getAuthSessionEpoch();
  const current = useRef({ ownerId, scope, authEpoch });
  if (current.current.ownerId !== ownerId || current.current.scope !== scope || current.current.authEpoch !== authEpoch) {
    current.current = { ownerId, scope, authEpoch };
  }
  const identity = current.current;
  const isCurrentOwner = () => current.current === identity && getAuthSessionEpoch() === identity.authEpoch;
  return { identity, isCurrentOwner };
}
