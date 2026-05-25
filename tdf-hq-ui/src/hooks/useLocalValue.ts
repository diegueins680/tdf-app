import { useReducer } from 'react';

type LocalValueAction<T> = T | ((previous: T) => T);

function localValueReducer<T>(state: T, action: LocalValueAction<T>): T {
  return typeof action === 'function' ? (action as (previous: T) => T)(state) : action;
}

export function useLocalValue<T>(initialValue: T): [T, (action: LocalValueAction<T>) => void] {
  return useReducer(localValueReducer<T>, initialValue);
}
