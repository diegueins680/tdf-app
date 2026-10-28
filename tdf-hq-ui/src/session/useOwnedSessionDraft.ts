import { useEffect, useState, type Dispatch, type SetStateAction } from 'react';
import { useSessionOwner } from './useSessionOwner';

// A blank form is not evidence that an account has an empty persisted profile.
// Keep the successful read and every asynchronous editor callback with its owner.
export function useOwnedSessionDraft<T>({
  ownerId,
  scope,
  baseline,
  empty,
}: {
  ownerId: number | null;
  scope?: string;
  baseline: T | undefined;
  empty: T;
}) {
  const { identity, isCurrentOwner } = useSessionOwner(ownerId, scope);
  const [state, setState] = useState(() => ({
    identity,
    draft: baseline ?? empty,
    baseline,
    dirty: false,
  }));

  useEffect(() => {
    setState((previous) => {
      if (previous.identity !== identity) {
        return { identity, draft: baseline ?? empty, baseline, dirty: false };
      }
      // A background read must not erase edits, including edits after a save began.
      if (baseline === undefined || previous.dirty || previous.baseline === baseline) return previous;
      return { identity, draft: baseline, baseline, dirty: false };
    });
  }, [baseline, empty, identity]);

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
      };
    });
  };

  const acknowledgeSave = (submitted: T) => {
    if (!isCurrentOwner()) return;
    setState((previous) => isCurrentOwner() && previous.identity === identity && previous.draft === submitted
      ? { ...previous, dirty: false } : previous);
  };

  return { draft, setDraft, ready, acknowledgeSave, isCurrentOwner };
}
