import { keepPreviousData, useInfiniteQuery } from '@tanstack/react-query';
import { useEffect, useState } from 'react';
import { Directory, type DirectorySearchItem, type DirectorySearchQuery } from '../../api/directory';

export const SEARCH_DEBOUNCE_MS = 300;
export function useDebouncedSearch(value: string, delay = SEARCH_DEBOUNCE_MS) {
  const [query, setQuery] = useState(value);
  useEffect(() => {
    const timer = window.setTimeout(() => setQuery(value.trim().replace(/\s+/gu, ' ')), delay);
    return () => window.clearTimeout(timer);
  }, [value, delay]);
  return query;
}

export function useDirectorySearch(query: DirectorySearchQuery) {
  return useInfiniteQuery({
    queryKey: ['directory', 'search', query],
    initialPageParam: undefined as string | undefined,
    queryFn: ({ pageParam, signal }) => Directory.search({ ...query, cursor: pageParam }, signal),
    getNextPageParam: (page) => page.nextCursor ?? undefined,
    staleTime: 30_000,
    gcTime: 30 * 60_000,
    placeholderData: keepPreviousData,
  });
}

export function directoryResultPath(item: DirectorySearchItem): string {
  // Never accept arbitrary URLs from a result, including protocol-relative URLs.
  if (item.type === 'profile') {
    if (item.canonicalPath && /^\/artista\/[1-9]\d*$/.test(item.canonicalPath)) return item.canonicalPath;
    return `/directorio/${encodeURIComponent(item.slug)}`;
  }
  if (item.type === 'classified') return `/clasificados/${encodeURIComponent(item.slug)}`;
  if (item.type === 'event') return `/eventos/${encodeURIComponent(item.id)}`;
  return `/venues/${encodeURIComponent(item.id)}`;
}
