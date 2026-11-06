import { useCallback, useEffect, useRef, useState, type SetStateAction } from 'react';
import { getAuthSessionEpoch } from './authEvents';
import { discardRecoverableDraft, readRecoverableDraft, saveRecoverableDraft } from './recoverableDraft';
import { useSessionOwner } from './useSessionOwner';

/** Opt-in form metadata recovery in this tab; binary files are never stored. */
export function useOwnedTabDraft<T>(initialValue: T, options: {
  ownerId: number | null;
  scope: string;
  parse: (value: unknown) => T | undefined;
}) {
  const { ownerId, scope } = options;
  const initial = useRef(initialValue);
  const parse = useRef(options.parse);
  parse.current = options.parse;
  const { identity, isCurrentOwner } = useSessionOwner(ownerId, scope);
  const initialize = useCallback(() => {
    const recovered = ownerId === null ? undefined : readRecoverableDraft(ownerId, scope, parse.current);
    return { identity, value: recovered ?? initial.current, dirty: false, restored: recovered !== undefined };
  }, [identity, ownerId, scope]);
  const [state, setState] = useState(initialize);
  const [persistenceAvailable, setPersistenceAvailable] = useState(true);
  useEffect(() => { setPersistenceAvailable(true); }, [identity]);
  useEffect(() => { setState((previous) => previous.identity === identity ? previous : initialize()); }, [identity, initialize]);

  const value = state.identity === identity ? state.value : initial.current;
  const setValue = useCallback((update: SetStateAction<T>) => {
    if (!isCurrentOwner()) return;
    setState((previous) => {
      if (previous.identity !== identity || !isCurrentOwner()) return previous;
      return { identity, value: typeof update === 'function' ? (update as (old: T) => T)(previous.value) : update, dirty: true, restored: false };
    });
  }, [identity, isCurrentOwner]);
  const saved = useRef<{ identity: typeof identity; serialized: string } | null>(null);
  useEffect(() => {
    if (ownerId === null || state.identity !== identity || !state.dirty || getAuthSessionEpoch() !== identity.authEpoch) return;
    try {
      const serialized = JSON.stringify(state.value);
      if (serialized === JSON.stringify(initial.current)) {
        discardRecoverableDraft(ownerId, scope);
        saved.current = null;
        return;
      }
      if (saved.current?.identity !== identity || serialized !== saved.current.serialized) {
        const savedDraft = saveRecoverableDraft(ownerId, scope, state.value);
        setPersistenceAvailable(savedDraft);
        if (savedDraft) saved.current = { identity, serialized };
      }
    } catch { setPersistenceAvailable(false); }
  }, [identity, ownerId, scope, state]);

  const discard = (replacement = initial.current) => {
    if (!isCurrentOwner()) return;
    if (ownerId !== null) discardRecoverableDraft(ownerId, scope);
    saved.current = null;
    setState({ identity, value: replacement, dirty: false, restored: false });
  };
  const setBaseline = useCallback((baseline: T) => {
    if (!isCurrentOwner()) return;
    setState((previous) => previous.identity !== identity || previous.dirty || previous.restored || previous.value === baseline
      ? previous : { ...previous, value: baseline });
  }, [identity, isCurrentOwner]);
  const bindings = useRef({ identity, setters: new Map<keyof T, unknown>() });
  if (bindings.current.identity !== identity) bindings.current = { identity, setters: new Map() };
  const field = <K extends keyof T>(key: K): [T[K], (update: SetStateAction<T[K]>) => void] => {
    let setter = bindings.current.setters.get(key) as ((update: SetStateAction<T[K]>) => void) | undefined;
    if (!setter) {
      setter = (update) => setValue((previous) => ({ ...previous, [key]: typeof update === 'function'
        ? (update as (old: T[K]) => T[K])(previous[key]) : update }));
      bindings.current.setters.set(key, setter);
    }
    return [value[key], setter];
  };
  return { value, setValue, field, discard, setBaseline, restored: state.identity === identity && state.restored, isCurrentOwner, persistenceAvailable };
}
