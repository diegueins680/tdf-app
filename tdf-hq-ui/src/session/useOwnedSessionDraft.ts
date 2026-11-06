import { useCallback, useEffect, useState, type Dispatch, type SetStateAction } from 'react';
import { useSessionOwner } from './useSessionOwner';
import { getAuthSessionEpoch } from './authEvents';
import { discardRecoverableDraft, readRecoverableDraft, saveRecoverableDraft } from './recoverableDraft';

// A blank form is not evidence that an account has an empty persisted profile.
// Keep the successful read and every asynchronous editor callback with its owner.
export function useOwnedSessionDraft<T>({
  ownerId,
  scope,
  baseline,
  empty,
  persistence,
}: {
  ownerId: number | null;
  scope?: string;
  baseline: T | undefined;
  empty: T;
  persistence?: { scope: string; parse: (value: unknown) => T | undefined };
}) {
  const { identity, isCurrentOwner } = useSessionOwner(ownerId, scope);
  const initialize = useCallback(() => {
    const restored = ownerId !== null && baseline !== undefined && persistence
      ? readRecoverableDraft(ownerId, persistence.scope, persistence.parse) : undefined;
    return { identity, draft: restored ?? baseline ?? empty, baseline, dirty: restored !== undefined, restored: restored !== undefined };
  }, [ownerId, baseline, persistence, identity, empty]);
  const [state, setState] = useState(initialize);
  const [persistenceAvailable, setPersistenceAvailable] = useState(true);
  useEffect(() => { setPersistenceAvailable(true); }, [identity]);

  useEffect(() => {
    setState((previous) => {
      if (previous.identity !== identity) {
        return initialize();
      }
      // A background read must not erase edits, including edits after a save began.
      if (baseline === undefined || previous.dirty || previous.baseline === baseline) return previous;
      if (previous.baseline === undefined) return initialize();
      return { identity, draft: baseline, baseline, dirty: false, restored: false };
    });
  }, [baseline, identity, initialize]);

  const ready = ownerId !== null && state.identity === identity
    && state.baseline !== undefined && baseline !== undefined && isCurrentOwner();
  const draft = state.identity === identity ? state.draft : empty;
  const setDraft: Dispatch<SetStateAction<T>> = (update) => {
    if (!ready || !isCurrentOwner()) return;
    setState((previous) => {
      if (previous.identity !== identity || !isCurrentOwner()) return previous;
      return {
        ...previous,
        draft: typeof update === 'function' ? (update as (value: T) => T)(previous.draft) : update,
        dirty: true,
        restored: false,
      };
    });
  };

  const persistenceScope = persistence?.scope;
  useEffect(() => {
    if (ready && ownerId !== null && persistenceScope && state.dirty && !state.restored
      && state.identity === identity && getAuthSessionEpoch() === identity.authEpoch) {
      setPersistenceAvailable(saveRecoverableDraft(ownerId, persistenceScope, state.draft));
    }
  }, [ready, ownerId, persistenceScope, state, identity]);

  const acknowledgeSave = (submitted: T) => {
    if (!isCurrentOwner()) return;
    setState((previous) => {
      if (!isCurrentOwner() || previous.identity !== identity || previous.draft !== submitted) return previous;
      if (ownerId !== null && persistence) discardRecoverableDraft(ownerId, persistence.scope);
      return { ...previous, dirty: false, restored: false };
    });
  };

  return { draft, setDraft, ready, acknowledgeSave, isCurrentOwner, restored: ready && state.restored, persistenceAvailable };
}
