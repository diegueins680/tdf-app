import { useEffect, useRef, useCallback } from 'react';

/**
 * Auto-saves form data to localStorage with debouncing.
 * Returns helpers to load, save, and clear the draft.
 */
export function useAutoSave<T>(
  key: string,
  data: T,
  options: {
    enabled?: boolean;
    debounceMs?: number;
    storageKey?: string;
  } = {}
) {
  const { enabled = true, debounceMs = 1000, storageKey = `draft:${key}` } = options;
  const timeoutRef = useRef<ReturnType<typeof setTimeout>>();

  // Save to localStorage with debounce
  useEffect(() => {
    if (!enabled) return;

    if (timeoutRef.current) clearTimeout(timeoutRef.current);
    timeoutRef.current = setTimeout(() => {
      try {
        localStorage.setItem(storageKey, JSON.stringify(data));
      } catch {
        // localStorage full or unavailable — silently ignore
      }
    }, debounceMs);

    return () => {
      if (timeoutRef.current) clearTimeout(timeoutRef.current);
    };
  }, [data, debounceMs, enabled, storageKey]);

  // Load draft from localStorage
  const loadDraft = useCallback((): T | null => {
    try {
      const saved = localStorage.getItem(storageKey);
      if (!saved) return null;
      try {
        const parsed: unknown = JSON.parse(saved);
        return parsed as T;
      } catch {
        return null;
      }
    } catch {
      return null;
    }
  }, [storageKey]);

  // Immediate save (no debounce)
  const saveDraft = useCallback(
    (value: T) => {
      try {
        localStorage.setItem(storageKey, JSON.stringify(value));
      } catch {
        // ignore
      }
    },
    [storageKey],
  );

  // Clear draft
  const clearDraft = useCallback(() => {
    try {
      localStorage.removeItem(storageKey);
    } catch {
      // ignore
    }
  }, [storageKey]);

  return { loadDraft, saveDraft, clearDraft };
}
