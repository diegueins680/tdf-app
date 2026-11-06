import { useCallback, useEffect, useRef } from 'react';
import { getAuthSessionEpoch } from './authEvents';

/** Revoke captured callbacks synchronously when credentials or editor scope change. */
export function useSessionOwner(ownerId: number | null, scope = '') {
  const authEpoch = getAuthSessionEpoch();
  const mounted = useRef(true);
  useEffect(() => { mounted.current = true; return () => { mounted.current = false; }; }, []);
  const current = useRef({ ownerId, scope, authEpoch });
  if (current.current.ownerId !== ownerId || current.current.scope !== scope || current.current.authEpoch !== authEpoch) {
    current.current = { ownerId, scope, authEpoch };
  }
  const identity = current.current;
  const isCurrentOwner = useCallback(() => mounted.current && current.current === identity && getAuthSessionEpoch() === identity.authEpoch, [identity]);
  return { identity, isCurrentOwner };
}
