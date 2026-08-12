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
  } = {}
) {
  const { enabled = true, debounceMs = 1000 } = options;
  const timeoutRef = useRef<ReturnType<typeof setTimeout>>();

  // Save to localStorage with debounce
  useEffect(() => {
    if (!enabled) return;

    if (timeoutRef.current) clearTimeout(timeoutRef.current);
    timeoutRef.current = setTimeout(() => {
      try {
        localStorage.setItem(`draft:${key}`, JSON.stringify(data));
      } catch {
        // localStorage full or unavailable — silently ignore
      }
    }, debounceMs);

    return () => {
      if (timeoutRef.current) clearTimeout(timeoutRef.current);
    };
  }, [key, data, enabled, debounceMs]);

  // Load draft from localStorage
  const loadDraft = useCallback((): T | null => {
    try {
      const saved = localStorage.getItem(`draft:${key}`);
      return saved ? JSON.parse(saved) : null;
    } catch {
      return null;
    }
  }, [key]);

  // Immediate save (no debounce)
  const saveDraft = useCallback(
    (value: T) => {
      try {
        localStorage.setItem(`draft:${key}`, JSON.stringify(value));
      } catch {
        // ignore
      }
    },
    [key],
  );

  // Clear draft
  const clearDraft = useCallback(() => {
    try {
      localStorage.removeItem(`draft:${key}`);
    } catch {
      // ignore
    }
  }, [key]);

  return { loadDraft, saveDraft, clearDraft };
}
