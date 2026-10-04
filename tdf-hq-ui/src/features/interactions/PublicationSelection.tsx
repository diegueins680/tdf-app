import { useEffect, useRef, type HTMLAttributes } from 'react';
import { Alert, Box } from '@mui/material';
import { useLocation } from 'react-router-dom';

/** Select only already-authorized source items; never fetch around permissions. */
export function usePublicationSelection(parameter: string, ids: readonly (string | number)[], loading = false) {
  const { search } = useLocation();
  const requested = new URLSearchParams(search).get(parameter);
  const index = requested === null ? -1 : ids.findIndex((id) => String(id) === requested);
  return { requested, index, notice: requested !== null && !loading && index < 0
    ? <Alert severity="info" role="status">Esta publicación ya no está disponible o no tienes acceso.</Alert> : null };
}
export type PublicationSelection = ReturnType<typeof usePublicationSelection>;

/** Mounted by pagination only when the requested item has become renderable. */
export function PublicationAnchor({ selected, children, ...props }: HTMLAttributes<HTMLDivElement> & { selected: boolean }) {
  const anchor = useRef<HTMLDivElement>(null);
  useEffect(() => {
    if (!selected) return;
    anchor.current?.scrollIntoView({ block: 'center' });
    anchor.current?.focus({ preventScroll: true });
  }, [selected]);
  return <Box {...props} ref={anchor} tabIndex={selected ? -1 : undefined}
    sx={{ '&:focus': { outline: '3px solid', outlineColor: 'primary.main', outlineOffset: 2 } }}>{children}</Box>;
}
